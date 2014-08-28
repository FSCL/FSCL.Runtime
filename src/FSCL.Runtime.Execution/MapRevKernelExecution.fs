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
open FSCL.Compiler.AcceleratedCollections

[<assembly:DefaultComponentAssembly>]
do()
        
[<StepProcessor("FSCL_RUNTIME_EXECUTION_ACCELERATED_MAPREV_PROCESSOR", 
                "FSCL_RUNTIME_EXECUTION_STEP")>]
type MapRevKernelExecutionProcessor() =      
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
                let isAccelerateMapOrRev = (node.KernelData.Kernel :? AcceleratedKernelInfo) && 
                                           ((node.KernelData.Kernel :?> AcceleratedKernelInfo).CollectionFunctionName.StartsWith("Array.map") ||
                                            (node.KernelData.Kernel :?> AcceleratedKernelInfo).CollectionFunctionName.StartsWith("Array.rev"))
                if isAccelerateMapOrRev then
                    let sharePriority = 
                        if opts.ContainsKey(RuntimeOptions.BufferSharePriority) then
                            opts.[RuntimeOptions.BufferSharePriority] :?> BufferSharePriority
                        else
                            BufferSharePriority.PriorityToFlags
                        
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
                                                                None, 
                                                                pool, 
                                                                isRoot, 
                                                                sharePriority)

                    // Compute work size
                    let globalSize = 
                        // Mapi, mapi2
                        if node.KernelData.Kernel.Parameters.[0].DataType = typeof<int> then
                            buffers.[node.KernelData.Kernel.Parameters.[1].Name].TotalCount
                        else
                            // Map, map2, rev
                            buffers.[node.KernelData.Kernel.Parameters.[0].Name].TotalCount
                    let localSize = KernelSetupUtil.ComputeLocalSizeWithGlobalSize(node.CompiledKernelData.Kernel, node.DeviceData.Device, [| globalSize |])

                    // Cool, we processed the input and now all the arguments have been set
                    // Run kernel
                    node.DeviceData.Queue.Execute(node.CompiledKernelData.Kernel, null, [| globalSize |], localSize, null)
                    node.DeviceData.Queue.Finish()

                    // Dispose buffers
                    for b in buffers do
                        pool.EndUsingBuffer(b.Value)

                    // Return the objects that the F# kernels eventually returns as a tuple (if more than 1)
                    Some(outputFromThisKernel)
                else
                    None

            // Multithread execution
            | :? MultithreadKernelFlowGraphNode ->
                let node = fnode :?> MultithreadKernelFlowGraphNode

                let isAccelerateMapOrRev = (node.KernelData :? AcceleratedKernelInfo) && 
                                           ((node.KernelData :?> AcceleratedKernelInfo).CollectionFunctionName.StartsWith("Array.map") ||
                                            (node.KernelData :?> AcceleratedKernelInfo).CollectionFunctionName.StartsWith("Array.rev"))
                if isAccelerateMapOrRev then                    
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
                    for par in node.KernelData.ParsedSignature.GetParameters() do      
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
                                let arr = pool.BeginOperateOnBuffer(b, true)
                                danglingBuffers.Add((b, arr))
                                arguments.Add(arr)
                            | ReturnedValue(o) ->
                                arguments.Add(o)
                        | _ ->
                            raise (new KernelSetupException("A regular function cannot receive an input different from ActualArgument or KernelOutput"))

                    // Setup workitem info
                    let workSize = LeafExpressionConverter.EvaluateQuotation(node.WorkSize.Value) :?> WorkSize
                    let globalSize, _, globalOffset = KernelSetupUtil.NormalizeWorkSize(workSize.GlobalSize(), workSize.LocalSize(), workSize.GlobalOffset())

                    // Create barrier
                    let mutable totalGlobalSize = 1L
                    for i in 0.. globalSize.Length - 1 do
                        totalGlobalSize <- totalGlobalSize * globalSize.[i]
                    let barrier = new Barrier(totalGlobalSize |> int)

                    // Evaluate arguments
                    let args = node.Arguments |> List.map(fun (e:Expr) -> LeafExpressionConverter.EvaluateQuotation(e))
                
                    // Spawn threads
                    let methodToExecute = node.KernelData.ParsedSignature
                    let ids = seq { 
                                    for i = globalOffset.[2] to globalSize.[2] - 1L do
                                        for j = globalOffset.[1] to globalSize.[1] - 1L do
                                            for k = globalOffset.[0] to globalSize.[0] - 1L do
                                                let globalId = [| k; j; i |]
                                                yield globalId
                                   }
                    ids |> 
                    Seq.map(fun gid -> 
                                async {                            
                                    let workItemInfo = new MultithreadWorkItemInfo(gid, globalSize, globalOffset, barrier)
                                    let data = args @ [ box workItemInfo ] |> List.toArray
                                    methodToExecute.Invoke(null, data) |> ignore
                                }) |> Async.Parallel |> Async.Ignore |> Async.RunSynchronously
                            
                    // End operating on buffers
                    for b, arr in danglingBuffers do
                        pool.EndOperateOnBuffer(b, arr, true)

                    // Return
                    Some(ReturnedValue(()))
                else
                    None                  
            // Regular function
            | _ ->
                None