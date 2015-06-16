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

    member this.FinalResultCPU(pool:BufferPoolManager, reduceFunction:obj, outputBuffer:OpenCLBuffer) =        
        // Operate final reduce on cpu
        let result = ref null        
        let arr = pool.BeginOperateOnBuffer(outputBuffer, true) 
        let finV =                     
            // Do final iteration on CPU
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
            !result     
        pool.EndOperateOnBuffer(outputBuffer, arr, false) 
        finV

    // Reduce for CPU
    member private this.StageReduceCpu((node, collectionVars, isRoot), step, opts) =
        let km = node.Module

        let sharePriority = 
            if opts.ContainsKey(RuntimeOptions.BufferSharePriority) then
                opts.[RuntimeOptions.BufferSharePriority] :?> BufferSharePriority
            else
                BufferSharePriority.PriorityToFlags
                 
        // Execute input (reduce has only one input)
        let input = node.Input |> Seq.map(fun i -> step.Process(i, collectionVars, false, opts)) |> Seq.head  
                                 
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
            
            // Get instance of opencl kernel
            let openclKernel = rk.CompiledKernelData.StartUsingKernel()

            // First parameter: input array, may be coming from actual argument or kernel output
            let par = rk.KernelData.Kernel.Parameters.[argIndex]
            let elementType = par.DataType.GetElementType()  
            let arr, lengths, isBuffer = 
                match input with
                | ReturnedBuffer(b) ->
                    None, b.Count, true
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
            openclKernel.SetMemoryArgument(argIndex, inputBuffer)    

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
            openclKernel.SetMemoryArgument(argIndex, outputBuffer)  
            // Set size parameter
            arguments.Add(par.SizeParameters.[0].Name, globalSize.[0])

            // Third parameter: block
            argIndex <- argIndex + 1
            // Get number of cores and use it for global size, local size = 1
            let cores = deviceData.Device.MaxOpenCLUnits
            let blockSize = Math.Ceiling((double)inputBuffer.TotalCount / (double)cores) |> int64
            openclKernel.SetValueArgument(argIndex, blockSize |> int) 
        
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
                        openclKernel.SetMemoryArgument(i, buffer)                      
                        // Store dim sizes
                        let sizeParameters = par.SizeParameters
                        for i = 0 to sizeParameters.Count - 1 do
                            arguments.Add(sizeParameters.[i].Name, lengths.[i])
                        // Store argument and buffer
                        arguments.Add(par.Name, arr)
                    else
                        // Ref to a scalar
                        openclKernel.SetValueArgumentAsObject(i, ob)  
                // A size parameter
                | FunctionParameterType.SizeParameter ->
                    // We can access succesfully "arguments" cause 
                    // length args come always later than the relative array
                    let v = arguments.[par.Name]             
                    openclKernel.SetValueArgument<int>(i, v :?> int64 |> int)
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
                    openclKernel.SetMemoryArgument(0, outputBuffer)
                deviceData.Queue.Execute(openclKernel, [| 0L |], [| currentOutputSize |], [| 1L |], null)  
              
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
         
            // Release opencl kernel
            rk.CompiledKernelData.EndUsingKernel(openclKernel)

            let finalReduceOnCPU = currentOutputSize > 0L
            let finV =
                if finalReduceOnCPU then
                    this.FinalResultCPU(pool, kernelData.Kernel.CustomInfo.["ReduceFunction"], outputBuffer)
                else
                    let arr = pool.BeginOperateOnBuffer(outputBuffer, true) 
                    let v = arr.GetValue(0)
                    pool.EndOperateOnBuffer(outputBuffer, arr, false) 
                    v

            // Dispose input buffer
            pool.EndUsingBuffer(inputBuffer)

            if not isRoot then
                // Return 
                Some(ReturnedBuffer(outputBuffer))
            else
                // Dispose output buffer
                pool.EndUsingBuffer(outputBuffer)
                Some(ReturnedValue(finV))
        
        // Multithread execution
        | Some(MultithreadKernel(mk)) ->
            failwith "Not supported yet"
        | _ ->
            None

    member private this.StageReduceGpu((node, collectionVars, isRoot), step, opts) =
        let km = node.Module

        let sharePriority = 
            if opts.ContainsKey(RuntimeOptions.BufferSharePriority) then
                opts.[RuntimeOptions.BufferSharePriority] :?> BufferSharePriority
            else
                BufferSharePriority.PriorityToFlags
                 
        // Execute input (reduce has only one input)
        let input = node.Input |> Seq.map(fun i -> step.Process(i, collectionVars, false, opts)) |> Seq.head  
                                 
        // Create kernel
        let runtimeKernel = step.KernelCreationManager.Process(node, opts)

        match runtimeKernel with
        // OpenCL kernel execution
        | Some(OpenCLKernel(rk)) ->
            let pool = step.BufferPoolManager

            let deviceData, kernelData, compiledData = rk.DeviceData, rk.KernelData, rk.CompiledKernelData

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
            
            // Get instance of opencl kernel
            let openclKernel = rk.CompiledKernelData.StartUsingKernel()

            // First parameter: input array, may be coming from actual argument or kernel output
            let par = rk.KernelData.Kernel.Parameters.[argIndex]
            let elementType = par.DataType.GetElementType()  
            let arr, lengths, isBuffer = 
                match input with
                | ReturnedBuffer(b) ->
                    None, b.Count, true
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
            openclKernel.SetMemoryArgument(argIndex, inputBuffer)  
            arguments.Add(par.SizeParameters.[0].Name, inputBuffer.Count.[0])  
            
            let globalSize = (inputBuffer.Count.[0] |> int) / 2
            // Decrease until globalSize is multiple
            let localSize = if globalSize < 256 then 16 else 256            
                
            // Second parameter: local buffer
            argIndex <- argIndex + 1
            let par = rk.KernelData.Kernel.Parameters.[argIndex]
            openclKernel.SetLocalArgument(argIndex, (Marshal.SizeOf(par.DataType.GetElementType()) |> int64) * (localSize |> int64)) 
            // Store dim sizes
            let sizeParameters = par.SizeParameters
            // Set size parameter                
            arguments.Add(par.SizeParameters.[0].Name, localSize |> int64)
            
            let mutable currentDataSize = inputBuffer.Count.[0]
            let mutable currentGlobalSize = globalSize |> int64
            let mutable currentLocalSize = if (localSize |> int64) > currentGlobalSize then currentGlobalSize else localSize |> int64
          
            // Third parameter: output buffer
            argIndex <- argIndex + 1
            // Third parameter: output buffer
            let par = rk.KernelData.Kernel.Parameters.[argIndex]
            // Create buffer and eventually init it
            let elementType = par.DataType.GetElementType()
            outputBuffer <- pool.RequireBufferForParameter(par, None, [| currentGlobalSize / currentLocalSize |], deviceData.Context, deviceData.Queue, isRoot, sharePriority)                            
            // Set kernel arg
            openclKernel.SetMemoryArgument(argIndex, outputBuffer)   
            // Set size parameter
            arguments.Add(par.SizeParameters.[0].Name, globalSize |> int64)

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
                        openclKernel.SetMemoryArgument(i, buffer)                      
                        // Store dim sizes
                        let sizeParameters = par.SizeParameters
                        for i = 0 to sizeParameters.Count - 1 do
                            arguments.Add(sizeParameters.[i].Name, lengths.[i])
                        // Store argument and buffer
                        arguments.Add(par.Name, arr)
                    else
                        // Ref to a scalar
                        openclKernel.SetValueArgumentAsObject(i, ob)  
                // A size parameter
                | FunctionParameterType.SizeParameter ->
                    // We can access succesfully "arguments" cause 
                    // length args come always later than the relative array
                    let v = arguments.[par.Name]             
                    openclKernel.SetValueArgument<int>(i, v :?> int64 |> int)
                | _ ->
                    // Impossible
                    failwith "Error"
            
            let mutable currentDataSize = inputBuffer.Count.[0]
            let mutable currentGlobalSize = globalSize |> int64
            let mutable currentLocalSize = if (localSize |> int64) > currentGlobalSize then currentGlobalSize else localSize |> int64
          
            // Execute until the output size is smaller than group size * number of compute units (full utilization of pipeline)       
            let smallestDataSize = kernelData.Kernel.Meta.KernelMeta.Get<MinReduceArrayLengthAttribute>().Length
                    
            while (currentDataSize > smallestDataSize) do
                deviceData.Queue.Execute(openclKernel, [| 0L |], [| currentGlobalSize |], [| currentLocalSize |], null)                
               
           
                // TESTING ITERATION - TO COMMENT
                
                let obj = pool.ReadBuffer(outputBuffer)
                
                // Recompute work size
                // Il local size become greater than or equal to global size, we set it to be half the global size
                currentDataSize <- currentGlobalSize / currentLocalSize
                if currentLocalSize >= (currentDataSize / 2L) then
                    currentLocalSize <- currentDataSize / 2L
                currentGlobalSize <- currentDataSize / 2L
            
                if(currentDataSize > smallestDataSize) then
                    // Exchange buffer
                    openclKernel.SetMemoryArgument(0, outputBuffer)
                    openclKernel.SetValueArgument(3, currentDataSize |> int)

                    // Set local buffer arg                 
                    openclKernel.SetValueArgument(4, currentLocalSize |> int)

            // Release opencl kernel
            rk.CompiledKernelData.EndUsingKernel(openclKernel)

            let finalReduceOnCPU = currentGlobalSize > 1L
            let finV =
                if finalReduceOnCPU then
                    this.FinalResultCPU(pool, kernelData.Kernel.CustomInfo.["ReduceFunction"], outputBuffer)
                else
                    let arr = pool.BeginOperateOnBuffer(outputBuffer, true) 
                    let v = arr.GetValue(0)
                    pool.EndOperateOnBuffer(outputBuffer, arr, false) 
                    v

            // Dispose input buffer
            pool.EndUsingBuffer(inputBuffer)

            if not isRoot then
                // Return 
                Some(ReturnedBuffer(outputBuffer))
            else
                // Dispose output buffer
                pool.EndUsingBuffer(outputBuffer)
                Some(ReturnedValue(finV))

        // Multithread execution
        | Some(MultithreadKernel(mk)) ->
            failwith "Not supported yet"
        | _ ->
            None