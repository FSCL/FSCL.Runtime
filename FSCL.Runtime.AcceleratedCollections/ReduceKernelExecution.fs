namespace FSCL.Runtime.KernelExecution

open System
open FSCL.Compiler
open FSCL.Runtime
open System.Collections.Generic
open System.Reflection
open Cloo
open Microsoft.FSharp.Linq.RuntimeHelpers
open System.Runtime.InteropServices
open Microsoft.FSharp.Quotations

[<assembly:DefaultComponentAssembly>]
do()

[<StepProcessor("FSCL_RUNTIME_EXECUTION_ACCELERATED_REDUCE_PROCESSOR", 
                "FSCL_RUNTIME_EXECUTION_STEP",
                Before = [| "FSCL_RUNTIME_EXECUTION_DEFAULT_PROCESSOR" |])>]

type ReduceKerernelExecutionProcessor() =      
    inherit CompilerStepProcessor<KernelExecutionInput, KernelExecutionOutput option>()
    
    member this.EvaluateAndApply(e:Expr<'T -> 'U -> 'W>, a:obj, b:obj) =
        let f = LeafExpressionConverter.EvaluateQuotation(e) :?> 'T -> 'U -> 'W
        f (a :?> 'T) (b :?> 'U)

    override this.Run(input, s, opts) =
        let step = s :?> KernelExecutionStep
        let pool = step.BufferPoolManager
        //new KernelExecutionOutput()
        
        let node = input.Node
        let isAccelerateReduce = match input.RuntimeInfo.[node.KernelID] with
                                 | di, ri, ci ->
                                    ri.Info.CustomInfo.ContainsKey("AcceleratedFunction") && 
                                    (ri.Info.CustomInfo.["AcceleratedFunction"] :?> String) = "ArrayReduce"
        // Check if this is an accelerated collection array reduce
        if (isAccelerateReduce) then
            let isRoot = input.IsRoot
            let node = input.Node
            let deviceData, kernelData, compiledData = input.RuntimeInfo.[input.Node.KernelID]
            let gSize = input.GlobalSize
            let lSize = input.LocalSize

            // We don't need global size (it is based on the input array)
            // If localSize is 0-length they should be retrieved in the kernel expression, i.e. in the graph node custom info
            let localSize =
                if lSize.Length = 0 then
                    if node.CustomInfo.ContainsKey("LOCAL_SIZE") then
                        node.CustomInfo.["LOCAL_SIZE"] :?> int64 array
                    else
                        raise (new KernelSetupException("No runtime local size value has been specified and no local size information can be found inside the kernel expression"))
                else
                    lSize
                    
            // Coming from preceding kernels buffers
            let mutable prevBuffer = None
            // Input (coming from preceding kernels) buffers
            let mutable inputBuffer = null
            // Output (returned or simply written) buffers
            let mutable outputBuffer = null

            // Get node input binding
            let nodeInput = FlowGraphManager.GetNodeInput(node)
            // Check if the first parameter (input array) is bound to the output of a kernel
            let inputPar = kernelData.Info.Parameters.[0]
            if nodeInput.ContainsKey(inputPar.Name) then
                match nodeInput.[inputPar.Name] with
                | KernelOutput(otherKernel, otherParIndex) ->
                    let kernelOutput = step.Process(new KernelExecutionInput(false, otherKernel, input.RuntimeInfo, [||], [||]))
                    // MUST HANDLE MULTIPLE BUFFERS RETURNED: POSSIBLE?
                    prevBuffer <- kernelOutput.ReturnBuffer
                | _ ->
                    ()

            // Now that we have executed all the preceding kernels, complete the evaluation of the arguments
            // We do this in a second time to avoid allocating many buffers in a recursive function, risking stack overflow
            let mutable argIndex = 0  

            // First parameter: input array, may be coming from actual argument or kernel output
            let par = kernelData.Info.Parameters.[argIndex]
            match nodeInput.[par.Name] with
            | ActualArgument(expr) ->
                // Input from an actual argument
                let o = LeafExpressionConverter.EvaluateQuotation(expr)
                // Create buffer and eventually init it
                let elementType = par.Type.GetElementType()
                inputBuffer <- pool.CreateTrackedBuffer(deviceData.Context, deviceData.Queue, par, o, BufferTools.KernelParameterAccessModeToFlags(par.Access), false)                      
                // Set kernel arg
                compiledData.Kernel.SetMemoryArgument(argIndex, inputBuffer)                      
                // Store dim sizes
                let sizeParameters = par.SizeParameters
                //bufferSizes.Add(par.Name, new Dictionary<string, int>())
                // Set size parameter
                let getLengthMethod = o.GetType().GetMethod("GetLongLength")
                compiledData.Kernel.SetValueArgument(argIndex + 3, getLengthMethod.Invoke(o, [| 0 |]) :?> int64 |> int)
            | KernelOutput(node, a) ->
                // WE SHOULD AVOID COPY!!!
                // Copy the output buffer of the input kernel
                inputBuffer <- pool.CreateUntrackedBuffer(deviceData.Context, deviceData.Queue, par, prevBuffer.Value.Count, BufferTools.KernelParameterAccessModeToFlags(par.Access), isRoot)                       
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
            let par = kernelData.Info.Parameters.[argIndex]
            if par.AddressSpace = KernelParameterAddressSpace.LocalSpace then
                compiledData.Kernel.SetLocalArgument(argIndex, (Marshal.SizeOf(par.Type.GetElementType()) |> int64) * localSize.[0]) 
                // Store dim sizes
                let sizeParameters = par.SizeParameters
                // Set size parameter                
                compiledData.Kernel.SetValueArgument(argIndex + 3, localSize.[0] |> int)
                
            argIndex <- argIndex + 1
            // Third parameter: output buffer
            let par = kernelData.Info.Parameters.[argIndex]
            // Create buffer and eventually init it
            let elementType = par.Type.GetElementType()
            outputBuffer <- pool.CreateUntrackedBuffer(deviceData.Context, deviceData.Queue, par,  [| inputBuffer.Count.[0] / (localSize.[0]) / 2L |], BufferTools.KernelParameterAccessModeToFlags(par.Access), isRoot)                            
            // Set kernel arg
            compiledData.Kernel.SetMemoryArgument(argIndex, outputBuffer)                      
            // Store dim sizes
            let sizeParameters = par.SizeParameters
            // Set size parameter
            compiledData.Kernel.SetValueArgument(argIndex + 3, (Marshal.SizeOf(par.Type.GetElementType()) |> int64) * (inputBuffer.Count.[0]) / (localSize.[0] / 2L) |> int)
            
            // Execute until the output size is smaller than group size * number of compute units (full utilization of pipeline)
            let smallestGlobalSize = (localSize.[0] |> int64) * deviceData.Device.MaxComputeUnits
            let mutable currentGlobalSize = inputBuffer.Count.[0] / 2L
            let mutable currentLocalSize = localSize.[0]
            let mutable lastOutputSize = inputBuffer.Count.[0]

            let offset = Array.zeroCreate<int64>(inputBuffer.Count.[0] |> int)
            while (currentGlobalSize > smallestGlobalSize) do
                deviceData.Queue.Execute(compiledData.Kernel, offset, [| currentGlobalSize |], [| currentLocalSize |], null)                
               
                // TESTING ITERATION - TO COMMENT
                let obj = Array.CreateInstance(kernelData.Info.Parameters.[2].Type.GetElementType(), [| currentGlobalSize / currentLocalSize |])
                BufferTools.ReadBuffer(kernelData.Info.Parameters.[2].Type.GetElementType(), deviceData.Queue, obj, outputBuffer)

                lastOutputSize <- currentGlobalSize / currentLocalSize

                // Recompute work size
                // Il local size become greater than or equal to global size, we set it to be half the global size
                let outputSize = currentGlobalSize / currentLocalSize
                if currentLocalSize >= (outputSize / 2L) then
                    currentLocalSize <- outputSize / 4L
                currentGlobalSize <- outputSize / 2L

                // Exchange buffer
                compiledData.Kernel.SetMemoryArgument(0, outputBuffer)
                compiledData.Kernel.SetValueArgument(3, (outputSize |> int) * (Marshal.SizeOf(kernelData.Info.Parameters.[0].Type.GetElementType())))
            
            // Read buffer
            let outputPar = kernelData.Info.Parameters.[2]
            // Allocate array (since if this is a return parameter it has no .NET array matching it)
            let arrobj = Array.CreateInstance(outputPar.Type.GetElementType(), [| lastOutputSize |])
            BufferTools.ReadBuffer(outputPar.Type.GetElementType(), deviceData.Queue, arrobj, outputBuffer)
            
            // Do final iteration on CPU
            let reduceFunction = kernelData.Info.CustomInfo.["ReduceFunction"]
            let mutable result = arrobj.GetValue(0)
            match reduceFunction with
            | :? MethodInfo ->
                for i = 1 to arrobj.Length - 1 do
                    // THIS REQUIRES STATIC METHOD OR MODULE FUNCTION
                    result <- (reduceFunction :?> MethodInfo).Invoke(null, [| result; arrobj.GetValue(i) |])
            | _ ->
                let compiledLambda = LeafExpressionConverter.EvaluateQuotation(reduceFunction :?> Expr)
                let lambdaMethod = compiledLambda.GetType().GetMethod("Invoke")
                // Check if tupled or curried args
                if(lambdaMethod.GetParameters().Length = 1) then
                    // Curried
                    for i = 1 to arrobj.Length - 1 do
                        let tempResult = lambdaMethod.Invoke(compiledLambda, [| result |])
                        result <- tempResult.GetType().GetMethod("Invoke").Invoke(tempResult, [| arrobj.GetValue(i) |])
                else 
                    // Tupled
                    for i = 1 to arrobj.Length - 1 do
                        result <- lambdaMethod.Invoke(compiledLambda, [| result; arrobj.GetValue(i) |])
            
            // Dispose input buffer
            pool.DisposeBuffer(inputBuffer)

            // If not root must write the result to buffer for the next kernel
            if not isRoot then
                let ob = pool.CreateUntrackedBuffer(deviceData.Context, deviceData.Queue, outputPar, [| 1L |], BufferTools.KernelParameterAccessModeToFlags(outputPar.Access), false)
                BufferTools.WriteBuffer(outputPar.GetType().GetElementType(), deviceData.Queue, ob, [| result |])
                
                // Dispose output buffer
                pool.DisposeBuffer(outputBuffer)
                // Return
                Some(KernelExecutionOutput(ob))
            else
                // Dispose output buffer
                pool.DisposeBuffer(outputBuffer)
                // Return
                Some(KernelExecutionOutput(result))
        else
            None