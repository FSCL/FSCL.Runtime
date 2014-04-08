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
open Cloo
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
            let deviceData, kernelData, compiledData = node.DeviceData, node.KernelData, node.CompiledKernelData
            let localSize = node.KernelData.Kernel.Meta.KernelMeta.Get<WorkSizeAttribute>().LocalSize
                                
            // Coming from preceding kernels buffers
            let mutable prevBuffer = None
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
                    // MUST HANDLE MULTIPLE BUFFERS RETURNED: POSSIBLE?
                    prevBuffer <- kernelOutput.ReturnBuffer
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
                let elementType = par.DataType.GetElementType()
                inputBuffer <- pool.CreateTrackedBuffer(deviceData.Context, deviceData.Queue, par, o, BufferTools.AccessModeToFlags(par.Access), false, isRoot)                      
                // Set kernel arg
                compiledData.Kernel.SetMemoryArgument(argIndex, inputBuffer)                      
                // Store dim sizes
                let sizeParameters = par.SizeParameters
                //bufferSizes.Add(par.Name, new Dictionary<string, int>())
                // Set size parameter
                compiledData.Kernel.SetValueArgument(argIndex + 3, ArrayUtil.GetArrayLength(o))
            | KernelOutput(node, a) ->
                // WE SHOULD AVOID COPY!!!
                // Copy the output buffer of the input kernel
                inputBuffer <- pool.CreateUntrackedBuffer(deviceData.Context, deviceData.Queue, par, prevBuffer.Value.Count, BufferTools.AccessModeToFlags(par.Access), isRoot)                       
                // Set kernel arg
                compiledData.Kernel.SetMemoryArgument(argIndex, inputBuffer)             
                // Store dim sizes
                let sizeParameters = par.SizeParameters
                // Set size parameter                
                compiledData.Kernel.SetValueArgument(argIndex + 3, inputBuffer.Count.[0] |> int)                               
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
            outputBuffer <- pool.CreateUntrackedBuffer(deviceData.Context, deviceData.Queue, par,  [| inputBuffer.Count.[0] / localSize.[0] / 2L |], ComputeMemoryFlags.ReadWrite, isRoot)                            
            // Set kernel arg
            compiledData.Kernel.SetMemoryArgument(argIndex, outputBuffer)                      
            // Store dim sizes
            let sizeParameters = par.SizeParameters
            // Set size parameter
            compiledData.Kernel.SetValueArgument(argIndex + 3, inputBuffer.Count.[0] / localSize.[0] / 2L |> int)
            
            // Execute until the output size is smaller than group size * number of compute units (full utilization of pipeline)
            let mutable currentGlobalSize = inputBuffer.Count.[0] / 2L
            let smallestGlobalSize = (1L <<< 10) / 2L - 1L//currentGlobalSize - 1L // 
            let mutable currentLocalSize = localSize.[0]
            let mutable lastOutputSize = inputBuffer.Count.[0]
            
            let offset = Array.zeroCreate<int64>(inputBuffer.Count.[0] |> int)
            
            while (currentGlobalSize > smallestGlobalSize) do
                deviceData.Queue.Execute(compiledData.Kernel, offset, [| currentGlobalSize |], [| currentLocalSize |], null)                
               
                // TESTING ITERATION - TO COMMENT
                lastOutputSize <- currentGlobalSize / currentLocalSize     
               
                // TESTING ITERATION - TO COMMENT
                //let obj = Array.CreateInstance(kernelData.Info.Parameters.[2].Type.GetElementType(), [| currentGlobalSize / currentLocalSize |])
                //BufferTools.ReadBuffer(kernelData.Info.Parameters.[2].Type.GetElementType(), deviceData.Queue, obj, outputBuffer, [| 1 |])

                // Recompute work size
                // Il local size become greater than or equal to global size, we set it to be half the global size
                let outputSize = currentGlobalSize / currentLocalSize
                if currentLocalSize >= (outputSize / 2L) then
                    currentLocalSize <- outputSize / 4L
                currentGlobalSize <- outputSize / 2L
                
                if(currentGlobalSize > smallestGlobalSize) then
                    // Exchange buffer
                    compiledData.Kernel.SetMemoryArgument(0, outputBuffer)
                    compiledData.Kernel.SetValueArgument(3, outputSize |> int)

                    // Set local buffer arg                 
                    compiledData.Kernel.SetValueArgument(4, currentLocalSize |> int)

            // Read buffer
            let outputPar = kernelData.Kernel.Parameters.[2]
            // Allocate array (since if this is a return parameter it has no .NET array matching it)
            let arrobj = Array.CreateInstance(outputPar.DataType.GetElementType(), lastOutputSize)

            BufferTools.ReadBuffer(outputPar.DataType.GetElementType(), deviceData.Queue, arrobj, outputBuffer)

            // Dispose kernel            
            //compiledData.Kernel.Dispose()

            // Do final iteration on CPU
            let reduceFunction = kernelData.Kernel.CustomInfo.["ReduceFunction"]
            let mutable result = arrobj.GetValue(0)
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
            pool.DisposeBuffer(inputBuffer)

            // If not root must write the result to buffer for the next kernel
            if not isRoot then
                let ob = pool.CreateUntrackedBuffer(deviceData.Context, deviceData.Queue, outputPar, [| 1L |], BufferTools.AccessModeToFlags(outputPar.Access), false)
                BufferTools.WriteBuffer(outputPar.GetType().GetElementType(), deviceData.Queue, ob, [| result |])
                
                // Dispose output buffer
                pool.DisposeBuffer(outputBuffer)
                // Return
                Some(ExecutionOutput(ob))
            else
                // Dispose output buffer
                pool.DisposeBuffer(outputBuffer)
                // Return
                Some(ExecutionOutput(result))
        else
            None