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

[<StepProcessor("FSCL_RUNTIME_EXECUTION_REDUCE_PROCESSOR", 
                "FSCL_RUNTIME_EXECUTION_STEP")>]

type ReduceKernelExecutionProcessor() =      
    inherit CompilerStepProcessor<IKFGNode * Dictionary<Var, obj> * bool, ExecutionOutput option>()

    override this.Run((fnode, env, isRoot), s, opts) =
        let step = s :?> NodeExecutionStep
        match fnode with
        | :? IKFGKernelNode as node ->                            
            let isAccelerateReduce = (node.Module.Kernel :? AcceleratedKernelInfo) && 
                                     (node.Module.Kernel :?> AcceleratedKernelInfo).CollectionFunctionName = "Array.reduce"
                                 
            let isAccelerateSum = (node.Module.Kernel :? AcceleratedKernelInfo) && 
                                  (node.Module.Kernel :?> AcceleratedKernelInfo).CollectionFunctionName = "Array.sum"
                                  
            if (isAccelerateReduce || isAccelerateSum) then
                let dev = node.Module.Kernel.Meta.KernelMeta.Get<DeviceTypeAttribute>().Type
                if dev = DeviceType.Cpu then
                    this.StageReduceCpu((node, env, isRoot), step, opts)
                else
                    this.StageReduceGpu((node, env, isRoot), step, opts)
            else
                None
        | _ ->
            None
            
    // Multithread execution
    // Reduce for CPU
    member private this.StageReduceCpu((node, collectionVars, isRoot), step, opts) =
        let km = node.Module

        let sharePriority = 
            if opts.ContainsKey(RuntimeOptions.BufferSharePriority) then
                opts.[RuntimeOptions.BufferSharePriority] :?> BufferSharePriority
            else
                BufferSharePriority.PriorityToFlags
                 
        // Execute input (reduce has only one input)
        let input = node.Input |> Seq.map(fun i -> step.Process(i, collectionVars, false)) |> Seq.head  
                                 
        // Create kernel
        let runtimeKernel = step.KernelCreationManager.Process(node, opts)

        match runtimeKernel with
        // OpenCL kernel execution
        | Some(OpenCLKernel(rk)) ->
            let pool = step.BufferPoolManager

            let deviceData, kernelData, compiledData = rk.DeviceData, rk.KernelData, rk.CompiledKernelData
            let localSize = [| 1L |]
            let globalSize = [| deviceData.Device.MaxOpenCLUnits |]

            let mutable executionOutput = ReturnedValue(())
            // Coming from preceding kernels buffers
            let mutable prevExecutionResult = ReturnedValue(())
            // Input (coming from preceding kernels) buffers
            let mutable inputBuffer = null
            // Output (returned or simply written) buffers
            let mutable outputBuffer = null
            
            // Now that we have executed all the preceding kernels, complete the evaluation of the arguments
            // We do this in a second time to avoid allocating many buffers in a recursive function, risking stack overflow
            let mutable argIndex = 0  
            let arguments = new Dictionary<string, obj>()

            // First parameter: input array, may be coming from actual argument or kernel output
            let par = rk.KernelData.Kernel.Parameters.[argIndex]
            let elementType = par.DataType.GetElementType()  
            let arr, lengths, isBuffer = 
                match input with
                | ReturnedUntrackedBuffer(b) ->
                    None, b.Count, true
                | ReturnedTrackedBuffer(b, v) ->
                    Some(v), b.Count, true
                | ReturnedValue(v) ->
                    if v.GetType().IsArray then
                        Some(v :?> Array), ArrayUtil.GetArrayLengths(v), false
                    else
                        // Impossible
                        failwith "Error"

            // Get buffer or try reuse the input one
            inputBuffer <- 
                if isBuffer |> not then
                    pool.RequireBufferForParameter(par, Some(arr.Value), lengths, rk.DeviceData.Context, rk.DeviceData.Queue, isRoot, sharePriority) 
                else
                    pool.RequireBufferForParameter(par, None, lengths, rk.DeviceData.Context, rk.DeviceData.Queue, isRoot, sharePriority, input) 
                            
            // Set kernel arg
            rk.CompiledKernelData.Kernel.SetMemoryArgument(argIndex, inputBuffer)    

            // Store size parameter
            let sizeParameters = par.SizeParameters
            for i = 0 to sizeParameters.Count - 1 do
                arguments.Add(sizeParameters.[i].Name, lengths.[i])
                
            // Second parameter: output buffer
            argIndex <- argIndex + 1
            let par = rk.KernelData.Kernel.Parameters.[argIndex]
            // Create buffer and eventually init it
            let elementType = par.DataType.GetElementType()
            outputBuffer <- pool.RequireBufferForParameter(par, None, globalSize, deviceData.Context, deviceData.Queue, isRoot, sharePriority)                            
            // Set kernel arg
            compiledData.Kernel.SetMemoryArgument(argIndex, outputBuffer)  
            // Set size parameter
            arguments.Add(par.SizeParameters.[0].Name, globalSize.[0])

            // Third parameter: block
            argIndex <- argIndex + 1
            // Get number of cores and use it for global size, local size = 1
            let cores = deviceData.Device.MaxOpenCLUnits
            let blockSize = Math.Ceiling((double)inputBuffer.TotalCount / (double)cores) |> int64
            compiledData.Kernel.SetValueArgument(argIndex, blockSize |> int) 
        
            // Add possible outsiders and size parameters
            for i = argIndex + 1 to kernelData.Kernel.Parameters.Count - 1 do
                let par = kernelData.Kernel.Parameters.[i]
                match par.ParameterType with                
                // A reference to a collection variable
                // or to an env value
                | FunctionParameterType.EnvVarParameter(_)
                | FunctionParameterType.OutValParameter(_) ->
                    let ob =
                        match par.ParameterType with
                        | FunctionParameterType.EnvVarParameter(v) ->
                            // Determine the value associated to the collection var
                            collectionVars.[v]
                        | FunctionParameterType.OutValParameter(e) ->
                            // Evaluate the expr
                            LeafExpressionConverter.EvaluateQuotation(e)
                        | _ ->
                            // Impossible
                            null
                    if par.DataType.IsArray then    
                        let elementType = par.DataType.GetElementType()  
                        let arr, lengths = 
                            ob :?> Array,
                            ArrayUtil.GetArrayLengths(ob :?> Array)

                        // Get buffer or try reuse the input one
                        let buffer = 
                            pool.RequireBufferForParameter(par, Some(arr), lengths, deviceData.Context, deviceData.Queue, isRoot, sharePriority) 
                            
                        // Set kernel arg
                        compiledData.Kernel.SetMemoryArgument(i, buffer)                      
                        // Store dim sizes
                        let sizeParameters = par.SizeParameters
                        for i = 0 to sizeParameters.Count - 1 do
                            arguments.Add(sizeParameters.[i].Name, lengths.[i])
                        // Store argument and buffer
                        arguments.Add(par.Name, arr)
                    else
                        // Ref to a scalar
                        compiledData.Kernel.SetValueArgumentAsObject(i, ob)  
                // A size parameter
                | FunctionParameterType.SizeParameter ->
                    // We can access succesfully "arguments" cause 
                    // length args come always later than the relative array
                    let v = arguments.[par.Name]             
                    compiledData.Kernel.SetValueArgument<int>(i, v :?> int64 |> int)
                | _ ->
                    // Impossible
                    failwith "Error"
                 
            // Execute        
            let smallestDataSize = kernelData.Kernel.Meta.KernelMeta.Get<MinReduceArrayLengthAttribute>().Length

            let mutable currentInputSize = inputBuffer.TotalCount
            let mutable currentOutputSize = currentInputSize / blockSize
            let mutable currentBlockSize = blockSize
            if currentOutputSize = 0L then
                currentOutputSize <- 1L

            let mutable iteration = 0
            while currentOutputSize > smallestDataSize do
                if iteration = 1 then
                    compiledData.Kernel.SetMemoryArgument(0, outputBuffer)
                deviceData.Queue.Execute(compiledData.Kernel, [| 0L |], [| currentOutputSize |], [| 1L |], null)  
              
                if currentOutputSize = 1L then
                    currentOutputSize <- 0L
                else
                    let prevOutputSize = currentOutputSize
                    currentInputSize <- prevOutputSize
                    currentOutputSize <- currentOutputSize / currentBlockSize    
                    if currentOutputSize = 0L then
                        currentBlockSize <- prevOutputSize
                        currentOutputSize <- 1L
                iteration <- iteration + 1            
         
            let finalReduceOnCPU = currentOutputSize > 0L

            // Operate final reduce on cpu
            let result = ref null        
            let arr = pool.BeginOperateOnBuffer(outputBuffer, true) 
            let newArr =                           
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
                    let na = Array.CreateInstance(result.Value.GetType(), 1)
                    na.SetValue(!result, 0)
                    na
                else
                    result := arr.GetValue(0)
                    let na = Array.CreateInstance(result.Value.GetType(), 1)
                    na.SetValue(!result, 0)
                    na
            pool.EndOperateOnBuffer(outputBuffer, newArr, not isRoot)

            // Dispose input buffer
            pool.EndUsingBuffer(inputBuffer)

            if not isRoot then
                // Return 
                Some(ReturnedUntrackedBuffer(outputBuffer))
            else
                // Dispose output buffer
                pool.EndUsingBuffer(outputBuffer)
                Some(ReturnedValue(!result))
        
        // Multithread execution
        | Some(MultithreadKernel(mk)) ->
            failwith "Not supported yet"
        | _ ->
            None

    member private this.StageReduceGpu((node, collectionVars, isRoot), s, opts) =
        failwith "NOT SUPPORTED YET"
    (*
        let step = s :?> KernelExecutionStep
        let pool = step.BufferPoolManager
        //new KernelExecutionOutput()

        let sharePriority = 
            if opts.ContainsKey(RuntimeOptions.BufferSharePriority) then
                opts.[RuntimeOptions.BufferSharePriority] :?> BufferSharePriority
            else
                BufferSharePriority.PriorityToFlags

        let deviceData, kernelData, compiledData = node.DeviceData, node.KernelData, node.CompiledKernelData

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
            
        // Get preferred local size
        let mutable localSize = node.CompiledKernelData.Kernel.GetWorkGroupSize(node.DeviceData.Device) |> int
        let globalSize = (inputBuffer.Count.[0] |> int) / 2
        // Decrease until globalSize is multiple
        while globalSize % localSize <> 0 do
            localSize <- localSize - 1

        argIndex <- argIndex + 1
        // Second parameter: local buffer
        compiledData.Kernel.SetLocalArgument(argIndex, (Marshal.SizeOf(par.DataType.GetElementType()) |> int64) * (localSize |> int64)) 
        // Store dim sizes
        let sizeParameters = par.SizeParameters
        // Set size parameter                
        compiledData.Kernel.SetValueArgument(argIndex + 3, localSize)
          
        let mutable currentDataSize = inputBuffer.Count.[0]
        let mutable currentGlobalSize = globalSize |> int64
        let mutable currentLocalSize = if (localSize |> int64) > currentGlobalSize then currentGlobalSize else localSize |> int64
          
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
        let arr = pool.BeginOperateOnBuffer(outputBuffer, true)
        let newArr =
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
                [| !result |] :> Array
        pool.EndOperateOnBuffer(outputBuffer, newArr, not isRoot)

        // Dispose input buffer
        pool.EndUsingBuffer(inputBuffer)

        // If not root must write the result to buffer for the next kernel
        if not isRoot then
            // Return 
            Some(ReturnedUntrackedBuffer(outputBuffer))
        else
            // Dispose output buffer
            pool.EndUsingBuffer(outputBuffer)
            Some(ReturnedValue(!result)) *)