namespace FSCL.Runtime.RuntimeSteps

open System
open FSCL.Compiler
open FSCL.Compiler.Util
open FSCL.Language
open FSCL.Runtime
open FSCL.Runtime.Managers
open FSCL.Compiler.AcceleratedCollections
open System.Collections.Generic
open System.Reflection
open OpenCL
open Microsoft.FSharp.Linq.RuntimeHelpers
open System.Runtime.InteropServices
open Microsoft.FSharp.Quotations

[<StepProcessor("FSCL_RUNTIME_EXECUTION_ACCELERATED_REDUCE_PROCESSOR", 
                "FSCL_RUNTIME_EXECUTION_STEP",
                Before = [| "FSCL_RUNTIME_EXECUTION_DEFAULT_PROCESSOR" |])>]

type ReduceKernelExecutionProcessor() =      
    inherit CompilerStepProcessor<FlowGraphNode * bool, ExecutionOutput option>()

    override this.Run((fnode, isRoot), s, opts) =
        match fnode with
        | :? KernelFlowGraphNode ->
            let node = fnode :?> KernelFlowGraphNode
            let isAccelerateReduce = (node.KernelData.Kernel :? AcceleratedKernelInfo) && 
                                     (node.KernelData.Kernel :?> AcceleratedKernelInfo).CollectionFunctionName = "Array.reduce"
                                 
            let isAccelerateSum = (node.KernelData.Kernel :? AcceleratedKernelInfo) && 
                                     (node.KernelData.Kernel :?> AcceleratedKernelInfo).CollectionFunctionName = "Array.sum"
            if (isAccelerateReduce || isAccelerateSum) then
                let dev = node.KernelData.Kernel.Meta.KernelMeta.Get<DeviceTypeAttribute>().Type
                if dev = DeviceType.Cpu then
                    this.StageReduceCpu((node, isRoot), s, opts)
                else
                    this.StageReduceGpu((node, isRoot), s, opts)
            else
                None
        | _ ->
            None

    // Reduce for CPU
    member private this.StageReduceCpu((node, isRoot), s, opts) =
        let step = s :?> KernelExecutionStep
        let pool = step.BufferPoolManager

        // Check if this is an accelerated collection array reduce
        let sharePriority = 
            if opts.ContainsKey(RuntimeOptions.BufferSharePriority) then
                opts.[RuntimeOptions.BufferSharePriority] :?> BufferSharePriority
            else
                BufferSharePriority.PriorityToFlags

        let deviceData, kernelData, compiledData = node.DeviceData, node.KernelData, node.CompiledKernelData
        let localSize = [| 1L |]
        let globalSize = node.KernelData.Kernel.Meta.KernelMeta.Get<WorkSizeAttribute>().GlobalSize
        let mutable executionOutput = ReturnedValue(())
        // Coming from preceding kernels buffers
        let mutable prevExecutionResult = ReturnedValue(())
        // Input (coming from preceding kernels) buffers
        let mutable inputBuffer = null
        // Output (returned or simply written) buffers
        let mutable outputBuffer = null

        // Get node input binding
        let nodeInput = FlowGraphUtil.GetNodeInput(node)
        let parameters = kernelData.Kernel.Parameters
        // Check if the first parameter (input array) is bound to the output of a kernel
        let inputPar = parameters.[0]
        if nodeInput.ContainsKey(inputPar.Name) then
            match nodeInput.[inputPar.Name] with
            | KernelOutput(otherKernel, otherParIndex) ->
                let kernelOutput = step.Process(otherKernel, false)
                prevExecutionResult <- kernelOutput
            | _ ->
                ()

        // Now that we have executed all the preceding kernels, complete the evaluation of the arguments
        // We do this in a second time to avoid allocating many buffers in a recursive function, risking stack overflow
        let mutable argIndex = 0  

        // First parameter: input array, may be coming from actual argument or kernel output
        let par = parameters.[argIndex]
        match nodeInput.[par.Name] with
        | ActualArgument(expr) ->
            // Input from an actual argument
            let o = LeafExpressionConverter.EvaluateQuotation(expr)
            // Create buffer and eventually init it
            inputBuffer <- pool.RequireBufferForParameter(par, Some(o :?> Array), ArrayUtil.GetArrayLengths(o), node.DeviceData.Context, node.DeviceData.Queue, isRoot, sharePriority)                      
            // Set kernel arg
            compiledData.Kernel.SetMemoryArgument(argIndex, inputBuffer)                      
            // Set size parameter
            compiledData.Kernel.SetValueArgument(argIndex + 3, ArrayUtil.GetArrayLength(o))
        | KernelOutput(n, a) ->
            let lengths = 
                match prevExecutionResult with
                | ReturnedUntrackedBuffer(b)
                | ReturnedTrackedBuffer(b, _) ->
                    b.Count
                | ReturnedValue(o) ->
                    if o.GetType().IsArray then
                        ArrayUtil.GetArrayLengths(o)
                    else
                        [||]

            // Copy the output buffer of the input kernel
            inputBuffer <- pool.RequireBufferForParameter(par, None, lengths, node.DeviceData.Context, node.DeviceData.Queue, isRoot, sharePriority, prevExecutionResult)                      
            // Set kernel arg
            compiledData.Kernel.SetMemoryArgument(argIndex, inputBuffer)   
            // Set size parameter                
            compiledData.Kernel.SetValueArgument(argIndex + 3, lengths.[0] |> int)                               
        | _ ->
            raise (new KernelSetupException("The parameter " + par.Name + " is considered as implicit, which means that the runtime should be able to provide its value automatically, but this can't be done for array parameters"))

        argIndex <- argIndex + 1
        // Second parameter: output buffer
        let par = parameters.[argIndex]
        // Create buffer and eventually init it
        let elementType = par.DataType.GetElementType()
        outputBuffer <- pool.RequireBufferForParameter(par, None, globalSize, deviceData.Context, deviceData.Queue, isRoot, sharePriority)                            
        // Set kernel arg
        compiledData.Kernel.SetMemoryArgument(argIndex, outputBuffer)                      
        // Store dim sizes
        let sizeParameters = par.SizeParameters
        // Set size parameter
        compiledData.Kernel.SetValueArgument(argIndex + 3, globalSize.[0] |> int)

        argIndex <- argIndex + 1
        // Third parameter: block
        // Get number of cores and use it for global size, local size = 1
        let cores = node.DeviceData.Device.MaxOpenCLUnits
        let blockSize = Math.Ceiling((double)inputBuffer.TotalCount / (double)cores) |> int
        compiledData.Kernel.SetValueArgument(argIndex, blockSize) 

        // Execute
        deviceData.Queue.Execute(compiledData.Kernel, [| 0L |], globalSize, [| 1L |], null)                
         
        // Operate final reduce on cpu
        let result = ref null
        pool.OperateOnBuffer(outputBuffer, 
                             (if isRoot then AccessMode.ReadOnly else AccessMode.ReadWrite),
                             fun arr ->
                                // Do final iteration on CPU
                                let reduceFunction = kernelData.Kernel.CustomInfo.["ReduceFunction"]
                                match reduceFunction with
                                | :? MethodInfo ->
                                    result := arr.GetValue(0)
                                    for i = 1 to arr.Length - 1 do
                                        // THIS REQUIRES STATIC METHOD OR MODULE FUNCTION
                                        let mi = (reduceFunction :?> MethodInfo)
                                        let args = [| !result; arr.GetValue(i) |]
                                        result := mi.Invoke(null, args)
                                | _ ->
                                    // WARNING:
                                    // If we execute this lambda starting from an expr passed by compiler, using LeafExpression and getting invoke methods we get a CLR failure exception
                                    // It seems that converting to Linq expressions works, but it's less efficient 
                                    let lambda = LeafExpressionConverter.EvaluateQuotation(reduceFunction :?> Expr)
                                    
                                    result := arr.GetValue(0)
                                    // Curried
                                    for i = 1 to arr.Length - 1 do
                                        let r1 = lambda.GetType().GetMethod("Invoke").Invoke(lambda, [| !result |])
                                        let r2 = r1.GetType().GetMethod("Invoke").Invoke(r1, [| arr.GetValue(i) |])
                                        result := r2
                                [| !result |] :> Array)

        // Dispose input buffer
        pool.EndUsingBuffer(inputBuffer)

        if not isRoot then
            // Return 
            Some(ReturnedUntrackedBuffer(outputBuffer))
        else
            // Dispose output buffer
            pool.EndUsingBuffer(outputBuffer)
            Some(ReturnedValue(!result))

    member private this.StageReduceGpu((node, isRoot), s, opts) =
        let step = s :?> KernelExecutionStep
        let pool = step.BufferPoolManager
        //new KernelExecutionOutput()

        let sharePriority = 
            if opts.ContainsKey(RuntimeOptions.BufferSharePriority) then
                opts.[RuntimeOptions.BufferSharePriority] :?> BufferSharePriority
            else
                BufferSharePriority.PriorityToFlags

        let deviceData, kernelData, compiledData = node.DeviceData, node.KernelData, node.CompiledKernelData
        let localSize = node.KernelData.Kernel.Meta.KernelMeta.Get<WorkSizeAttribute>().LocalSize
        let mutable executionOutput = ReturnedValue(())
        // Coming from preceding kernels buffers
        let mutable prevExecutionResult = ReturnedValue(())
        // Input (coming from preceding kernels) buffers
        let mutable inputBuffer = null
        // Output (returned or simply written) buffers
        let mutable outputBuffer = null

        // Get node input binding
        let nodeInput = FlowGraphUtil.GetNodeInput(node)
        let parameters = kernelData.Kernel.Parameters
        // Check if the first parameter (input array) is bound to the output of a kernel
        let inputPar = parameters.[0]
        if nodeInput.ContainsKey(inputPar.Name) then
            match nodeInput.[inputPar.Name] with
            | KernelOutput(otherKernel, otherParIndex) ->
                let kernelOutput = step.Process(otherKernel, false)
                prevExecutionResult <- kernelOutput
            | _ ->
                ()

        // Now that we have executed all the preceding kernels, complete the evaluation of the arguments
        // We do this in a second time to avoid allocating many buffers in a recursive function, risking stack overflow
        let mutable argIndex = 0  

        // First parameter: input array, may be coming from actual argument or kernel output
        let par = parameters.[argIndex]
        match nodeInput.[par.Name] with
        | ActualArgument(expr) ->
            // Input from an actual argument
            let o = LeafExpressionConverter.EvaluateQuotation(expr)
            // Create buffer and eventually init it
            inputBuffer <- pool.RequireBufferForParameter(par, Some(o :?> Array), ArrayUtil.GetArrayLengths(o), node.DeviceData.Context, node.DeviceData.Queue, isRoot, sharePriority)                      
            // Set kernel arg
            compiledData.Kernel.SetMemoryArgument(argIndex, inputBuffer)                      
            // Set size parameter
            compiledData.Kernel.SetValueArgument(argIndex + 3, ArrayUtil.GetArrayLength(o))
        | KernelOutput(n, a) ->
            let lengths = 
                match prevExecutionResult with
                | ReturnedUntrackedBuffer(b)
                | ReturnedTrackedBuffer(b, _) ->
                    b.Count
                | ReturnedValue(o) ->
                    if o.GetType().IsArray then
                        ArrayUtil.GetArrayLengths(o)
                    else
                        [||]

            // WE SHOULD AVOID COPY!!!
            // Copy the output buffer of the input kernel
            inputBuffer <- pool.RequireBufferForParameter(par, None, lengths, node.DeviceData.Context, node.DeviceData.Queue, isRoot, sharePriority, prevExecutionResult)                      
            // Set kernel arg
            compiledData.Kernel.SetMemoryArgument(argIndex, inputBuffer)   
            // Set size parameter                
            compiledData.Kernel.SetValueArgument(argIndex + 3, lengths.[0] |> int)                               
        | _ ->
            raise (new KernelSetupException("The parameter " + par.Name + " is considered as implicit, which means that the runtime should be able to provide its value automatically, but this can't be done for array parameters"))

        argIndex <- argIndex + 1
        // Second parameter: local buffer
        compiledData.Kernel.SetLocalArgument(argIndex, (Marshal.SizeOf(par.DataType.GetElementType()) |> int64) * localSize.[0]) 
        // Store dim sizes
        let sizeParameters = par.SizeParameters
        // Set size parameter                
        compiledData.Kernel.SetValueArgument(argIndex + 3, localSize.[0] |> int)
          
        let mutable currentDataSize = inputBuffer.Count.[0]
        let mutable currentGlobalSize = currentDataSize / 2L
        let mutable currentLocalSize = if localSize.[0] > currentGlobalSize then currentGlobalSize else localSize.[0]
          
        argIndex <- argIndex + 1
        // Third parameter: output buffer
        let par = parameters.[argIndex]
        // Create buffer and eventually init it
        let elementType = par.DataType.GetElementType()
        outputBuffer <- pool.RequireBufferForParameter(par, None, [| currentGlobalSize / currentLocalSize |], deviceData.Context, deviceData.Queue, isRoot, sharePriority)                            
        // Set kernel arg
        compiledData.Kernel.SetMemoryArgument(argIndex, outputBuffer)                      
        // Store dim sizes
        let sizeParameters = par.SizeParameters
        // Set size parameter
        compiledData.Kernel.SetValueArgument(argIndex + 3, currentGlobalSize / currentLocalSize |> int)
        
        // Execute until the output size is smaller than group size * number of compute units (full utilization of pipeline)
        let smallestDataSize = node.KernelData.Kernel.Meta.KernelMeta.Get<MinReduceArrayLengthAttribute>().Length //currentGlobalSize - 1L // 
                    
        while (currentDataSize > smallestDataSize) do
            deviceData.Queue.Execute(compiledData.Kernel, [| 0L |], [| currentGlobalSize |], [| currentLocalSize |], null)                
               
           
            // TESTING ITERATION - TO COMMENT
            (*
            let obj = Array.CreateInstance(outputBuffer.ElementType, [| currentGlobalSize |])
            BufferTools.ReadBuffer(deviceData.Queue, false, obj, outputBuffer, [| currentGlobalSize |])
            *)
            // Recompute work size
            // Il local size become greater than or equal to global size, we set it to be half the global size
            currentDataSize <- currentGlobalSize / currentLocalSize
            if currentLocalSize >= (currentDataSize / 2L) then
                currentLocalSize <- currentDataSize / 2L
            currentGlobalSize <- currentDataSize / 2L
            
            if(currentDataSize > smallestDataSize) then
                // Exchange buffer
                compiledData.Kernel.SetMemoryArgument(0, outputBuffer)
                compiledData.Kernel.SetValueArgument(3, currentDataSize |> int)

                // Set local buffer arg                 
                compiledData.Kernel.SetValueArgument(4, currentLocalSize |> int)

        // Read buffer
        let outputPar = kernelData.Kernel.Parameters.[2]
        let result = ref null 
        let finalReduceOnCPU = currentDataSize > 1L
        pool.OperateOnBuffer(outputBuffer, 
                             (if isRoot then AccessMode.ReadOnly else AccessMode.ReadWrite),
                                fun arr ->
                                    if finalReduceOnCPU then
                                        // Do final iteration on CPU
                                        let reduceFunction = kernelData.Kernel.CustomInfo.["ReduceFunction"]
                                        match reduceFunction with
                                        | :? MethodInfo ->
                                            result := arr.GetValue(0)
                                            for i = 1 to arr.Length - 1 do
                                                // THIS REQUIRES STATIC METHOD OR MODULE FUNCTION
                                                result := (reduceFunction :?> MethodInfo).Invoke(null, [| !result; arr.GetValue(i) |])
                                        | _ ->
                                            // WARNING:
                                            // If we execute this lambda starting from an expr passed by compiler, using LeafExpression and getting invoke methods we get a CLR failure exception
                                            // It seems that converting to Linq expressions works, but it's less efficient 
                                            let lambda = LeafExpressionConverter.EvaluateQuotation(reduceFunction :?> Expr)
                                        
                                            result := arr.GetValue(0)
                                            // Curried
                                            for i = 1 to arr.Length - 1 do
                                                let r1 = lambda.GetType().GetMethod("Invoke").Invoke(lambda, [| !result |])
                                                let r2 = r1.GetType().GetMethod("Invoke").Invoke(r1, [| arr.GetValue(i) |])
                                                result := r2
                                        [| !result |] :> Array
                                     else
                                        result := arr.GetValue(0)
                                        [| !result |] :> Array)

        // Dispose input buffer
        pool.EndUsingBuffer(inputBuffer)

        // If not root must write the result to buffer for the next kernel
        if not isRoot then
            // Return 
            Some(ReturnedUntrackedBuffer(outputBuffer))
        else
            // Dispose output buffer
            pool.EndUsingBuffer(outputBuffer)
            Some(ReturnedValue(!result))