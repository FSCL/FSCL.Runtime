namespace FSCL.Runtime.RuntimeSteps

open System
open FSCL.Compiler
open FSCL.Compiler.Util
open FSCL.Language
open FSCL.Runtime
open FSCL.Runtime.Managers
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Linq.RuntimeHelpers
open OpenCL

[<assembly:DefaultComponentAssembly>]
do()

[<StepProcessor("FSCL_RUNTIME_EXECUTION_REGULAR_FUNCTION_PROCESSOR", "FSCL_RUNTIME_EXECUTION_STEP")>]
type RegularFunctionExecutionProcessor() =      
    inherit CompilerStepProcessor<FlowGraphNode * bool, ExecutionOutput option>()
    
    override this.Run(input, s, opts) =
        let step = s :?> KernelExecutionStep
        let pool = step.BufferPoolManager

        let fnode, isRoot = input

        match fnode with
        | :? RegularFunctionFlowGraphNode ->
            let node = fnode :?> RegularFunctionFlowGraphNode

            // Input (coming from preceding kernels) buffers
            let inputFromOtherKernels = new Dictionary<String, ExecutionOutput>()
            let arguments = new List<obj>()
            // Buffers on which the regular function is operating
            let danglingBuffers = new List<OpenCLBuffer * Array>()
        
            // Get node input binding
            let nodeInput = FlowGraphUtil.GetNodeInput(node)
            // Check which parameters are bound to the output of a kernel
            for par in node.MethodInfo.GetParameters() do
                if nodeInput.ContainsKey(par.Name) then
                    match nodeInput.[par.Name] with
                    | KernelOutput(otherKernel, otherParIndex) ->
                        let kernelOutput = step.Process(otherKernel, false)
                        inputFromOtherKernels.Add(par.Name, kernelOutput)    
                    | _ ->
                        ()
                    
            // Now that we have executed all the preceding kernels, complete the evaluation of the arguments
            // We do this in a second time to avoid allocating many buffers ina recursive function, risking stack overflow
            let mutable argIndex = 0            
            // Foreach argument of the kernel
            for par in node.MethodInfo.GetParameters() do      
                match nodeInput.[par.Name] with
                | ActualArgument(expr) ->
                    // Input from an actual argument
                    let o = LeafExpressionConverter.EvaluateQuotation(expr)
                    // Store argument
                    arguments.Add(o)
                | KernelOutput(othNode, a) ->
                    // Copy the output buffer of the input kernel     
                    match inputFromOtherKernels.[par.Name] with
                    | ReturnedUntrackedBuffer(b)
                    | ReturnedTrackedBuffer(b, _) ->
                        // Must read buffer cause this is a regular function
                        let arr = pool.BeginOperateOnBuffer(b)
                        danglingBuffers.Add((b, arr))
                        arguments.Add(arr)
                    | ReturnedValue(o) ->
                        arguments.Add(o)
                | _ ->
                    raise (new KernelSetupException("A regular function cannot receive an input different from ActualArgument or KernelOutput"))
            
                // Process next parameter
                argIndex <- argIndex + 1

            // Invoke the function
            let result = 
                if node.Object.IsSome then
                    node.MethodInfo.Invoke(LeafExpressionConverter.EvaluateQuotation(node.Object.Value), arguments |> Array.ofSeq)
                else
                    node.MethodInfo.Invoke(null, arguments |> Array.ofSeq)

            // End operating on buffers
            for b, arr in danglingBuffers do
                pool.EndOperateOnBuffer(arr)

            // Return the objects that the F# kernels eventually returns as a tuple (if more than 1)
            Some(ReturnedValue(result))

        | _ ->
            None
                  