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

type DefaultKerernelExecutionProcessor() =      
    inherit CompilerStepProcessor<KernelExecutionInput, KernelExecutionOutput>()
    
    override this.Run(input, s) =
        let step = s :?> KernelExecutionStep

        let node = input.Node

        // Check if this is an accelerated collection array reduce
        if (node.CustomInfo.ContainsKey("AcceleratedFunction") && 
            (node.CustomInfo.["AcceleratedFunction"] :?> String) = "ArrayReduce") then

            if not (node.CustomInfo.ContainsKey("ExecutionInProcess")) then
                // Add a marker to skip this same node
                node.CustomInfo.Add("ExecutionInProcess", true)

                // Do not set this not as root if it it, cause we don't want the output to be read from buffer
                // to variable
                // Let other processors (mainly the default one) execute this kernel
                let mutable result = step.Process(new KernelExecutionInput(false, input.Node, input.RuntimeInfo, input.GlobalSize, input.LocalSize))

                // Let's iterate until the globalSize is lower than the localSize
                let deviceData, kernelData, compiledData = input.RuntimeInfo.[input.Node.KernelID]

                // Check if we need to iterate
                let mutable outputSize = input.GlobalSize.[0]
                while outputSize > 128 do
                    let globalSize = outputSize / 2
                    let localSize = 
                        if input.LocalSize.[0] >= globalSize then
                            input.LocalSize.[0] / 2
                        else
                            input.LocalSize.[0]

                    // Set prev output buffer as input of next iter
                    let currInput = FlowGraphManager.GetNodeInput(input.Node)
                    FlowGraphManager.SetNodeInput(input.Node, 
                                                  kernelData.Info.Parameters.[0].Name, 
                                                  currInput.[kernelData.Info.Parameters.[2].Name])
                    result <- step.Process(new KernelExecutionInput(false, input.Node, input.RuntimeInfo, [|globalSize|], [|localSize|]))
                    outputSize <- outputSize / 2
                
                // Final cpu sequential reduction

                    
                    
           
                    
        // Return the objects that the F# kernels eventually returns as a tuple (if more than 1)
        if isRoot then
            // Return values to be given back to host side
            if returnedObjects.Count = 0 then
                new KernelExecutionOutput()
            else
                new KernelExecutionOutput(returnedObjects)
        else
            // Return buffers to be used by other kernels
            new KernelExecutionOutput(returnedBuffers)
                  