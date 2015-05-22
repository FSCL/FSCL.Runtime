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

        match fnode with
        | :? IKFGKernelNode as node ->
            let km = node.Module

            let sharePriority = 
                if opts.ContainsKey(RuntimeOptions.BufferSharePriority) then
                    opts.[RuntimeOptions.BufferSharePriority] :?> BufferSharePriority
                else
                    BufferSharePriority.PriorityToFlags
                        
            // Evaluate input
            let input = node.Input |> 
                        Seq.map(fun i -> 
                            step.Process(i, env, false, opts)
                            ) |> Seq.toArray
                    
            // Create kernel
            let runtimeKernel = step.KernelCreationManager.Process(node, opts)

            match runtimeKernel with
            // OpenCL kernel execution
            | Some(OpenCLKernel(runtimeKernel)) ->
                // Get work size
                let workSize = km.Kernel.WorkSize.Value :?> WorkSize
                let globalSize, localSize, globalOffset = workSize.GlobalSize(), workSize.LocalSize(), workSize.GlobalOffset() 

                // Get instance of opencl kernel
                let openclKernel = runtimeKernel.CompiledKernelData.StartUsingKernel()

                // Prepare arguments
                let arguments, buffers, outputFromThisKernel = 
                    KernelSetupUtil.PrepareKernelAguments(runtimeKernel, openclKernel, 
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
                runtimeKernel.DeviceData.Queue.Execute(openclKernel, globalOffset, globalSize, localSize, null)
                runtimeKernel.DeviceData.Queue.Finish()

                // Release opencl kernel
                runtimeKernel.CompiledKernelData.EndUsingKernel(openclKernel)

                // Dispose buffers
                for b in buffers do
                    pool.EndUsingBuffer(b.Value)

                // Return the objects that the F# kernels eventually returns as a tuple (if more than 1)
                Some(outputFromThisKernel)

            // Multithread execution
            | Some(MultithreadKernel(multithreadKernel)) ->
                // Get work size
                let workSize = km.Kernel.WorkSize.Value :?> WorkSize
                let globalSize, localSize, globalOffset =
                    KernelSetupUtil.NormalizeWorkSize(workSize.GlobalSize(), workSize.LocalSize(), workSize.GlobalOffset()) 

                // Create global barrier
                let mutable totalGlobalSize = 1L
                for i in 0.. globalSize.Length - 1 do
                    totalGlobalSize <- totalGlobalSize * globalSize.[i]
                let barrier = new Barrier(totalGlobalSize |> int)
                                               
                // Ensure input readable from managed environment
                let managedInput = input |> Array.map(fun i ->
                                                        match i with
                                                        | ReturnedBuffer(buffer) ->
                                                            pool.BeginOperateOnBuffer(buffer, true) |> box
                                                        | ReturnedValue(v) ->
                                                            v) |> Array.toList

                // Spawn threads
                let methodToExecute = multithreadKernel.KernelData.Kernel.ParsedSignature.Value
                let ids = seq { 
                                for i = (if globalOffset = null then 0L else globalOffset.[2]) to globalSize.[2] - 1L do
                                    for j = (if globalOffset = null then 0L else globalOffset.[1]) to globalSize.[1] - 1L do
                                        for k = (if globalOffset = null then 0L else globalOffset.[0]) to globalSize.[0] - 1L do
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
                                let data = managedInput @ [ box workItemInfo ] |> List.toArray
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
                raise (new KernelCompilationException("Cannot compile and run kernel " + node.Module.Kernel.Name))
        | _ ->
            None