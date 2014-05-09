namespace FSCL.Runtime.RuntimeSteps

open System
open FSCL.Compiler
open FSCL.Compiler.Language
open FSCL.Runtime
open FSCL.Runtime.Managers
open FSCL.Runtime.Language
open FSCL.Compiler.Plugins.AcceleratedCollections
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

    override this.Run(input, s, opts) =
        this.TwoStageReduce(input, s, opts)
            
    member private this.TwoStageReduce((node, isRoot), s, opts) =
        let step = s :?> KernelExecutionStep
        let pool = step.BufferPoolManager
        //new KernelExecutionOutput()
        
        let isAccelerateReduce = (node.KernelData.Kernel :? AcceleratedKernelInfo) && 
                                 node.KernelData.Kernel.CustomInfo.ContainsKey("ReduceFunction")

        // Check if this is an accelerated collection array reduce
        if (isAccelerateReduce) then
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
            | KernelOutput(node, a) ->
                let ib = 
                    match prevExecutionResult with
                    | ReturnedUntrackedBuffer(b)
                    | ReturnedTrackedBuffer(b, _) ->
                        b
                    | _ ->
                        raise (new KernelSetupException("Explicit return value  is possible only for root kernels"))

                // WE SHOULD AVOID COPY!!!
                // Copy the output buffer of the input kernel
                inputBuffer <- pool.RequireBufferForParameter(par, None, ib.Count, node.DeviceData.Context, node.DeviceData.Queue, isRoot, sharePriority, prevExecutionResult)                      
                // Set kernel arg
                compiledData.Kernel.SetMemoryArgument(argIndex, inputBuffer)   
                // Set size parameter                
                compiledData.Kernel.SetValueArgument(argIndex + 3, ib.Count.[0] |> int)                               
            | _ ->
                raise (new KernelSetupException("The parameter " + par.Name + " is considered as implicit, which means that the runtime should be able to provide its value automatically, but this can't be done for array parameters"))

            argIndex <- argIndex + 1
            // Second parameter: local buffer
            compiledData.Kernel.SetLocalArgument(argIndex, (Marshal.SizeOf(par.DataType.GetElementType()) |> int64) * localSize.[0]) 
            // Store dim sizes
            let sizeParameters = par.SizeParameters
            // Set size parameter                
            compiledData.Kernel.SetValueArgument(argIndex + 3, localSize.[0] |> int)
                
            argIndex <- argIndex + 1
            // Third parameter: output buffer
            let par = parameters.[argIndex]
            // Create buffer and eventually init it
            let elementType = par.DataType.GetElementType()
            outputBuffer <- pool.RequireBufferForParameter(par, None, [| inputBuffer.Count.[0] / localSize.[0] / 2L |], deviceData.Context, deviceData.Queue, isRoot, sharePriority)                            
            // Set kernel arg
            compiledData.Kernel.SetMemoryArgument(argIndex, outputBuffer)                      
            // Store dim sizes
            let sizeParameters = par.SizeParameters
            // Set size parameter
            compiledData.Kernel.SetValueArgument(argIndex + 3, inputBuffer.Count.[0] / localSize.[0] / 2L |> int)
            
            // Execute until the output size is smaller than group size * number of compute units (full utilization of pipeline)
            let mutable currentGlobalSize = inputBuffer.Count.[0] / 2L
            let mutable currentDataSize = inputBuffer.Count.[0]
            let smallestDataSize = node.KernelData.Kernel.Meta.KernelMeta.Get<MinReduceArrayLengthAttribute>().Length //currentGlobalSize - 1L // 
            let mutable currentLocalSize = localSize.[0]
            let mutable lastOutputSize = inputBuffer.Count.[0]
            
            let offset = Array.zeroCreate<int64>(inputBuffer.Count.[0] |> int)
            
            while (currentDataSize > smallestDataSize) do
                deviceData.Queue.Execute(compiledData.Kernel, offset, [| currentGlobalSize |], [| currentLocalSize |], null)                
                   
               
                // TESTING ITERATION - TO COMMENT
                //let obj = Array.CreateInstance(kernelData.Info.Parameters.[2].Type.GetElementType(), [| currentGlobalSize / currentLocalSize |])
                //BufferTools.ReadBuffer(kernelData.Info.Parameters.[2].Type.GetElementType(), deviceData.Queue, obj, outputBuffer, [| 1 |])

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
            let mutable result = null
            let outputPar = kernelData.Kernel.Parameters.[2]
            if currentDataSize > 1L then
                let useMap = outputPar.Meta.Get<BufferReadModeAttribute>().Mode = BufferReadMode.MapBuffer
                // Allocate array (since if this is a return parameter it has no .NET array matching it)
                let arrobj = Array.CreateInstance(outputPar.DataType.GetElementType(), lastOutputSize)
                BufferTools.ReadBuffer(deviceData.Queue, useMap, arrobj, outputBuffer)

                // Do final iteration on CPU
                let reduceFunction = kernelData.Kernel.CustomInfo.["ReduceFunction"]
                match reduceFunction with
                | :? MethodInfo ->
                    for i = 1 to arrobj.Length - 1 do
                        // THIS REQUIRES STATIC METHOD OR MODULE FUNCTION
                        result <- (reduceFunction :?> MethodInfo).Invoke(null, [| result; arrobj.GetValue(i) |])
                | _ ->
                    // WARNING:
                    // If we execute this lambda starting from an expr passed by compiler, using LeafExpression and getting invoke methods we get a CLR failure exception
                    // It seems that converting to Linq expressions works, but it's less efficient 
                    let lambda = LeafExpressionConverter.EvaluateQuotation(reduceFunction :?> Expr)

                    // Curried
                    for i = 1 to arrobj.Length - 1 do
                        let r1 = lambda.GetType().GetMethod("Invoke").Invoke(lambda, [| result |])
                        let r2 = r1.GetType().GetMethod("Invoke").Invoke(r1, [| arrobj.GetValue(i) |])
                        result <- r2

            // Dispose input buffer
            pool.EndUsingBuffer(inputBuffer)

            // If not root must write the result to buffer for the next kernel
            if not isRoot then
                if currentDataSize > 1L then
                    let useMap = outputPar.Meta.Get<BufferReadModeAttribute>().Mode = BufferReadMode.MapBuffer
                    // If some iterations performed on CPU write them back to the buffer
                    BufferTools.WriteBuffer(deviceData.Queue, useMap, outputBuffer, [| result |])
                // Otherwise they are already in the buffer
                outputBuffer.Count <- [| 1L |]
                // Dispose output buffer
                pool.EndUsingBuffer(outputBuffer)
                // Return 
                Some(ReturnedUntrackedBuffer(outputBuffer))
            else
                // Dispose output buffer
                pool.EndUsingBuffer(outputBuffer)
                // Return
                Some(ReturnedValue(result))
        else
            None