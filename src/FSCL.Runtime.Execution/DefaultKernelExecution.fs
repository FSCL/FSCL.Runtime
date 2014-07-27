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
open Microsoft.FSharp.Quotations
open System.Threading

[<assembly:DefaultComponentAssembly>]
do()
        
[<StepProcessor("FSCL_RUNTIME_EXECUTION_DEFAULT_PROCESSOR", 
                "FSCL_RUNTIME_EXECUTION_STEP",
                Dependencies = [| "FSCL_RUNTIME_EXECUTION_ACCELERATED_REDUCE_PROCESSOR"; 
                                  "FSCL_RUNTIME_EXECUTION_ACCELERATED_MAPREV_PROCESSOR" |])>]
type DefaultKernelExecutionProcessor() =      
    inherit CompilerStepProcessor<FlowGraphNode * bool, ExecutionOutput option>()
    
    override this.Run(input, s, opts) =
        let step = s :?> KernelExecutionStep
        let pool = step.BufferPoolManager

        let fnode, isRoot = input
        
        if opts.ContainsKey("IterativeSetup") then
           None
        else
            match fnode with
            | :? OpenCLKernelFlowGraphNode ->
                let node = fnode :?> OpenCLKernelFlowGraphNode
                let sharePriority = 
                    if opts.ContainsKey(RuntimeOptions.BufferSharePriority) then
                        opts.[RuntimeOptions.BufferSharePriority] :?> BufferSharePriority
                    else
                        BufferSharePriority.PriorityToFlags

                let workSize = LeafExpressionConverter.EvaluateQuotation(node.WorkSize.Value) :?> WorkSize
                let globalSize, localSize, globalOffset = workSize.GlobalSize(), workSize.LocalSize(), workSize.GlobalOffset() 

                // Input (coming from preceding kernels) buffers
                let inputFromOtherKernels = new Dictionary<String, ExecutionOutput>()
        
                // Get node input binding
                let nodeInput = FlowGraphUtil.GetNodeInput(node)
                // Check which parameters are bound to the output of a kernel
                for par in node.KernelData.Kernel.Parameters do
                    if nodeInput.ContainsKey(par.Name) then
                        match nodeInput.[par.Name] with
                        | KernelOutput(otherKernel, otherParIndex) ->
                            let kernelOutput = step.Process(otherKernel, false)
                            inputFromOtherKernels.Add(par.Name, kernelOutput)    
                        | _ ->
                            ()
                    
                let arguments, buffers, outputFromThisKernel = 
                    KernelSetupUtil.PrepareKernelAguments(node, 
                                                          nodeInput, 
                                                          inputFromOtherKernels, 
                                                          Some(workSize), 
                                                          pool, 
                                                          isRoot, 
                                                          sharePriority)


                // Cool, we processed the input and now all the arguments have been set
                // Run kernel
                // 32 bit enought for size_t. Kernel uses size_t like int without cast. 
                // We cannot put case into F# kernels each time the user does operations with get_global_id and similar!
                node.DeviceData.Queue.Execute(node.CompiledKernelData.Kernel, globalOffset, globalSize, localSize, null)
                node.DeviceData.Queue.Finish()

                // Dispose buffers
                for b in buffers do
                    pool.EndUsingBuffer(b.Value)

                // Return the objects that the F# kernels eventually returns as a tuple (if more than 1)
                Some(outputFromThisKernel)

            // Multithread execution
            | :? MultithreadKernelFlowGraphNode ->
                let node = fnode :?> MultithreadKernelFlowGraphNode
                
                // Input (coming from preceding kernels) buffers
                let inputFromOtherKernels = new Dictionary<String, ExecutionOutput>()
                let arguments = new List<obj>()
                // Buffers on which the regular function is operating
                let danglingBuffers = new List<OpenCLBuffer * Array>()
        
                // Get node input binding
                let nodeInput = FlowGraphUtil.GetNodeInput(node)
                // Check which parameters are bound to the output of a kernel
                for par in node.KernelData.ParsedSignature.GetParameters() do
                    if nodeInput.ContainsKey(par.Name) then
                        match nodeInput.[par.Name] with
                        | KernelOutput(otherKernel, otherParIndex) ->
                            let kernelOutput = step.Process(otherKernel, false)
                            inputFromOtherKernels.Add(par.Name, kernelOutput)    
                        | _ ->
                            ()
                             
                // Foreach argument of the function
                let mutable workSize = WorkSize(0L)
                for par in node.KernelData.ParsedSignature.GetParameters() do      
                    match nodeInput.[par.Name] with
                    | ActualArgument(expr) ->
                        // Check if worksize arg
                        if typeof<WorkItemInfo>.IsAssignableFrom(expr.Type) then
                            workSize <- LeafExpressionConverter.EvaluateQuotation(expr) :?> WorkSize
                        else
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

                // Setup workitem info
                let globalSize, _, globalOffset = KernelSetupUtil.NormalizeWorkSize(workSize.GlobalSize(), workSize.LocalSize(), workSize.GlobalOffset())

                // Create barrier
                let mutable totalGlobalSize = 1L
                for i in 0.. globalSize.Length - 1 do
                    totalGlobalSize <- totalGlobalSize * globalSize.[i]
                let barrier = ref null
                let recreateBarrier = ref true
                let lockObj = new Object()

                // Evaluate arguments
                let args = node.Arguments |> List.map(fun (e:Expr) -> LeafExpressionConverter.EvaluateQuotation(e))
                
                // Spawn threads
                let methodToExecute = node.KernelData.ParsedSignature      
                let threads = new List<Thread>()          
                for i = globalOffset.[2] |> int to globalSize.[2] - 1L |> int do
                    for j = globalOffset.[1] |> int to globalSize.[1] - 1L |> int do
                        for k = globalOffset.[0] |> int to globalSize.[0] - 1L |> int do
                            let globalId = [| k |> int64; j |> int64; i |> int64 |]
                            let workItemInfo = new MultithreadWorkItemInfo(globalId, globalSize, globalOffset, lockObj, recreateBarrier, barrier)
                            let data = args @ [ box workItemInfo ] |> List.toArray
                            let t = new Thread(fun () -> methodToExecute.Invoke(null, data) |> ignore)
                            threads.Add(t)
                            t.Start()

                // Wait to complete
                for t in threads do
                    t.Join()

                // End operating on buffers
                for b, arr in danglingBuffers do
                    pool.EndOperateOnBuffer(arr)

                // Return
                Some(ReturnedValue(()))
                                  
            // Regular function
            | _ ->
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
                             
                // Foreach argument of the function
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