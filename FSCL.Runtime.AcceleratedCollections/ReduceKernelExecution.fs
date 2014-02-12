namespace FSCL.Runtime.KernelExecution

open System
open FSCL.Compiler
open FSCL.Runtime
open System.Collections.Generic
open System.Reflection
open Cloo
open Microsoft.FSharp.Linq.QuotationEvaluation

[<assembly:DefaultComponentAssembly>]
do()

[<StepProcessor("FSCL_RUNTIME_EXECUTION_ACCELERATED_REDUCE_PROCESSOR", 
                "FSCL_RUNTIME_EXECUTION_STEP",
                Before = [| "FSCL_RUNTIME_EXECUTION_DEFAULT_PROCESSOR" |])>]

type ReduceKerernelExecutionProcessor() =      
    inherit CompilerStepProcessor<KernelExecutionInput, KernelExecutionOutput option>()
    
    override this.Run(input, s) =
        let step = s :?> KernelExecutionStep
        //new KernelExecutionOutput()

        let node = input.Node

        // Check if this is an accelerated collection array reduce
        if (node.CustomInfo.ContainsKey("AcceleratedFunction") && 
            (node.CustomInfo.["AcceleratedFunction"] :?> String) = "ArrayReduce") then

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
                        node.CustomInfo.["LOCAL_SIZE"] :?> int array
                    else
                        raise (new KernelSetupException("No runtime local size value has been specified and no local size information can be found inside the kernel expression"))
                else
                    lSize

            // Input (coming from preceding kernels) buffers
            let mutable inputBuffer = null
            // Output (returned or simply written) buffers
            let mutable outputBuffer = null
            // Arguments
            let arguments = new Dictionary<string, obj>()

            // Get node input binding
            let nodeInput = FlowGraphManager.GetNodeInput(node)
            // Check if the first parameter (input array) is bound to the output of a kernel
            let inputPar = kernelData.Info.Parameters.[0]
            if nodeInput.ContainsKey(inputPar.Name) then
                match nodeInput.[inputPar.Name] with
                | KernelOutput(otherKernel, otherParIndex) ->
                    let kernelOutput = step.Run(new KernelExecutionInput(false, otherKernel, input.RuntimeInfo, [||], [||]))
                    // MUST HANDLE MULTIPLE BUFFERS RETURNED: POSSIBLE?
                    inputBuffer <- kernelOutput.ReturnBuffers.[0] :?> ComputeMemory  
                | _ ->
                    ()

            // Now that we have executed all the preceding kernels, complete the evaluation of the arguments
            // We do this in a second time to avoid allocating many buffers in a recursive function, risking stack overflow

            let mutable argIndex = 0  
            // First parameter: input array, may be coming from actual argument or kernel output
            let par = kernelData.Info.Parameters.[0]
            match nodeInput.[par.Name] with
            | ActualArgument(expr) ->
                // Input from an actual argument
                let o = expr.EvalUntyped()
                // Check if read or read_write mode
                let access = par.Access
                // Create buffer and eventually init it
                let elementType = par.Type.GetElementType()
                //let buffer = BufferTools.WriteBuffer(elementType, deviceData.Context, deviceData.Queue, o, par.SizeParameters.Count, true)                            
                // Set kernel arg
                //compiledData.Kernel.SetMemoryArgument(argIndex, buffer.Value)                      
                // Store dim sizes
                let sizeParameters = par.SizeParameters
                //bufferSizes.Add(par.Name, new Dictionary<string, int>())
                let getLengthMethod = o.GetType().GetMethod("GetLongLength")
                for i = 0 to sizeParameters.Count - 1 do
                    //bufferSizes.[par.Name]
                    arguments.Add(sizeParameters.[i].Name, getLengthMethod.Invoke(o, [| i |]))
                // Store argument and buffer
                arguments.Add(par.Name, o)
            | KernelOutput(node, a) ->
                // WE SHOULD AVOID COPY!!!
                // Copy the output buffer of the input kernel
                //let buffer = BufferTools.CopyBuffer(par.Type.GetElementType(), deviceData.Context, deviceData.Queue, inputBuffers.[par.Name])            
                // Store dim sizes
                let sizeParameters = par.SizeParameters
                ()
                //bufferSizes.Add(par.Name, new Dictionary<string, int>())
                //for i = 0 to sizeParameters.Count - 1 do
                  //  arguments.Add(sizeParameters.[i].Name, buffer.Value.Count.[i])
                            
                //compiledData.Kernel.SetMemoryArgument(argIndex, buffer.Value)                      
            | _ ->
                raise (new KernelSetupException("The parameter " + par.Name + " is considered as implicit, which means that the runtime should be able to provide its value automatically, but this can't be done for array parameters"))

            argIndex <- argIndex + 1

            // Second parameter: local storage
            Some(new KernelExecutionOutput())
        else
            None