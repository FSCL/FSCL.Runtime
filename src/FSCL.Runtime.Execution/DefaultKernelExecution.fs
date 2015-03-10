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
                Dependencies = [| "FSCL_RUNTIME_EXECUTION_MAPREV_PROCESSOR"; 
                                  "FSCL_RUNTIME_EXECUTION_REDUCE_PROCESSOR" |])>]
type DefaultKernelExecutionProcessor() =      
    inherit CompilerStepProcessor<IKFGNode * Dictionary<Var, obj> * bool, ExecutionOutput option>()
    
    override this.Run((fnode, env, isRoot), s, opts) =
        let step = s :?> NodeExecutionStep
        let pool = step.BufferPoolManager

        if opts.ContainsKey("IterativeSetup") then
           None
        else
            match fnode with
            | :? IKFGKernelNode as node ->
                let km = node.Module

                let sharePriority = 
                    if opts.ContainsKey(RuntimeOptions.BufferSharePriority) then
                        opts.[RuntimeOptions.BufferSharePriority] :?> BufferSharePriority
                    else
                        BufferSharePriority.PriorityToFlags
                        
                // Evaluate input
                let input = node.Input |> Seq.map(fun i -> step.Process(i, env, false)) |> Seq.toArray
                    
                // Create kernel
                let runtimeKernel = step.KernelCreationManager.Process(node, opts)

                match runtimeKernel with
                // OpenCL kernel execution
                | Some(OpenCLKernel(runtimeKernel)) ->
                    // Get work size
                    let workSize = LeafExpressionConverter.EvaluateQuotation(km.Kernel.WorkSize.Value) :?> WorkSize
                    let globalSize, localSize, globalOffset = workSize.GlobalSize(), workSize.LocalSize(), workSize.GlobalOffset() 

                    // Prepare arguments
                    let arguments, buffers, outputFromThisKernel = 
                        KernelSetupUtil.PrepareKernelAguments(runtimeKernel, 
                                                              input, 
                                                              env,
                                                              Some(workSize), 
                                                              pool, 
                                                              isRoot, 
                                                              sharePriority)
                                                              
                    // Cool, we processed the input and now all the arguments have been set
                    // Run kernel
                    // 32 bit enought for size_t. Kernel uses size_t like int without cast. 
                    // We cannot put cast into F# kernels each time the user does operations with get_global_id and similar!
                    runtimeKernel.DeviceData.Queue.Execute(runtimeKernel.CompiledKernelData.Kernel, globalOffset, globalSize, localSize, null)
                    runtimeKernel.DeviceData.Queue.Finish()

                    // Dispose buffers
                    for b in buffers do
                        pool.EndUsingBuffer(b.Value)

                    // Return the objects that the F# kernels eventually returns as a tuple (if more than 1)
                    Some(outputFromThisKernel)

                // Multithread execution
                | Some(MultithreadKernel(multithreadKernel)) ->
                    // Get work size
                    let workSize = LeafExpressionConverter.EvaluateQuotation(km.Kernel.WorkSize.Value) :?> WorkSize
                    let globalSize, localSize, globalOffset = workSize.GlobalSize(), workSize.LocalSize(), workSize.GlobalOffset() 

                    // Create barrier
                    let mutable totalGlobalSize = 1L
                    for i in 0.. globalSize.Length - 1 do
                        totalGlobalSize <- totalGlobalSize * globalSize.[i]
                    let barrier = new Barrier(totalGlobalSize |> int)
                                    
                    // Spawn threads
                    let methodToExecute = multithreadKernel.KernelData.Kernel.ParsedSignature
                    let ids = seq { 
                                    for i = globalOffset.[2] to globalSize.[2] - 1L do
                                        for j = globalOffset.[1] to globalSize.[1] - 1L do
                                            for k = globalOffset.[0] to globalSize.[0] - 1L do
                                                let globalId = [| k; j; i |]
                                                yield globalId
                                    }
                    let tResult = 
                        if methodToExecute.ReturnType <> typeof<unit> && methodToExecute.ReturnType <> typeof<System.Void> then
                            Array.CreateInstance(methodToExecute.ReturnType, globalSize)
                        else
                            null
                    let instance = 
                        if multithreadKernel.KernelData.InstanceExpr.IsNone then
                            null
                        else
                            LeafExpressionConverter.EvaluateQuotation(multithreadKernel.KernelData.InstanceExpr.Value)

                    ids |> 
                    Seq.map(fun gid -> 
                                async {                            
                                    let workItemInfo = new MultithreadWorkItemInfo(gid, globalSize, globalOffset, barrier)
                                    let data =  [ box workItemInfo ] |> List.toArray
                                    let res = methodToExecute.Invoke(instance, data) 
                                    if tResult <> null then
                                        tResult.SetValue(res, gid)                                    
                                }) |> Async.Parallel |> Async.Ignore |> Async.RunSynchronously
                            
                    // Return
                    if tResult <> null then
                        Some(ReturnedValue(tResult.GetValue([| 0; 0; 0 |])))
                    else
                        Some(ReturnedValue(()))
                                  
                // Fail
                | _ ->
                    raise (new KernelCompilationException("Cannot compile and run kernel " + node.Module.Kernel.ParsedSignature.Name))
            | _ ->
                None
//                    let node = fnode :?> RegularFunctionFlowGraphNode
//
//                    // Input (coming from preceding kernels) buffers
//                    let inputFromOtherKernels = new Dictionary<String, ExecutionOutput>()
//                    let arguments = new List<obj>()
//                    // Buffers on which the regular function is operating
//                    let danglingBuffers = new List<OpenCLBuffer * Array>()
//        
//                    // Get node input binding
//                    let nodeInput = FlowGraphUtil.GetNodeInput(node)
//                    // Check which parameters are bound to the output of a kernel
//                    for par in node.MethodInfo.GetParameters() do
//                        if nodeInput.ContainsKey(par.Name) then
//                            match nodeInput.[par.Name] with
//                            | KernelOutput(otherKernel, otherParIndex) ->
//                                let kernelOutput = step.Process(otherKernel, false)
//                                inputFromOtherKernels.Add(par.Name, kernelOutput)    
//                            | _ ->
//                                ()
//                             
//                    // Foreach argument of the function
//                    for par in node.MethodInfo.GetParameters() do      
//                        match nodeInput.[par.Name] with
//                        | ActualArgument(expr) ->
//                            // Input from an actual argument
//                            let o = LeafExpressionConverter.EvaluateQuotation(expr)
//                            // Store argument
//                            arguments.Add(o)
//                        | KernelOutput(othNode, a) ->
//                            // Copy the output buffer of the input kernel     
//                            match inputFromOtherKernels.[par.Name] with
//                            | ReturnedUntrackedBuffer(b)
//                            | ReturnedTrackedBuffer(b, _) ->
//                                // Must read buffer cause this is a regular function
//                                let arr = pool.BeginOperateOnBuffer(b, true)
//                                danglingBuffers.Add((b, arr))
//                                arguments.Add(arr)
//                            | ReturnedValue(o) ->
//                                arguments.Add(o)
//                        | _ ->
//                            raise (new KernelSetupException("A regular function cannot receive an input different from ActualArgument or KernelOutput"))
//
//                    // Invoke the function
//                    let result = 
//                        if node.Object.IsSome then
//                            node.MethodInfo.Invoke(LeafExpressionConverter.EvaluateQuotation(node.Object.Value), arguments |> Array.ofSeq)
//                        else
//                            node.MethodInfo.Invoke(null, arguments |> Array.ofSeq)
//
//                    // End operating on buffers
//                    for b, arr in danglingBuffers do
//                        pool.EndOperateOnBuffer(b, arr, true)
//
//                    // Return the objects that the F# kernels eventually returns as a tuple (if more than 1)
//                    Some(ReturnedValue(result))