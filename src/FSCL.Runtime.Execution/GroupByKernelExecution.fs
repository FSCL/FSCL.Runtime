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
        
[<StepProcessor("FSCL_RUNTIME_EXECUTION_GROUPBY_PROCESSOR", 
                "FSCL_RUNTIME_EXECUTION_STEP")>]
type GroupByKernelExecutionProcessor() =      
    inherit CompilerStepProcessor<IKFGNode * Dictionary<Var, obj> * bool, ExecutionOutput option>()
    
    override this.Run((fnode, env, isRoot), s, opts) =
        let step = s :?> NodeExecutionStep
        let pool = step.BufferPoolManager

        if opts.ContainsKey("IterativeSetup") then
           None
        else
            match fnode with
            | :? IKFGKernelNode as node ->                
                let isAccelerateGroupBy = (node.Module.Kernel :? AcceleratedKernelInfo) && 
                                           ((node.Module.Kernel :?> AcceleratedKernelInfo).CollectionFunctionName = "Array.groupBy")
                if isAccelerateGroupBy then                
                    let km = node.Module

                    let sharePriority = 
                        if opts.ContainsKey(RuntimeOptions.BufferSharePriority) then
                            opts.[RuntimeOptions.BufferSharePriority] :?> BufferSharePriority
                        else
                            BufferSharePriority.PriorityToFlags
                        
                    // Evaluate input
                    let input = node.Input |> Seq.map(fun i -> step.Process(i, env, false, opts)) |> Seq.toArray
                    
                    // Create kernel
                    let runtimeKernel = step.KernelCreationManager.Process(node, opts)

                    match runtimeKernel with
                    // OpenCL kernel execution
                    | Some(OpenCLKernel(runtimeKernel)) ->
                        // Get instance of opencl kernel
                        let openclKernel = runtimeKernel.CompiledKernelData.StartUsingKernel()

                        // Prepare arguments
                        let arguments, buffers, outputFromThisKernel = 
                            KernelSetupUtil.PrepareKernelAguments(runtimeKernel, openclKernel,
                                                                  input, 
                                                                  env,
                                                                  None, 
                                                                  pool, 
                                                                  isRoot, 
                                                                  sharePriority)
                                               
                        // Get work size
                        let globalSize = 
                            buffers.[node.Module.Kernel.Parameters.[0].Name].TotalCount
                        let localSize = KernelSetupUtil.ComputeLocalSizeWithGlobalSize(openclKernel, runtimeKernel.DeviceData.Device, [| globalSize |])

                        // Cool, we processed the input and now all the arguments have been set
                        // Run kernel
                        runtimeKernel.DeviceData.Queue.Execute(openclKernel, null, [| globalSize |], localSize, null)
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
                        (*
                        let globalSize = 
                            // Mapi, mapi2
                            if node.Module.Kernel.Parameters.[0].DataType = typeof<int> then
                                buffers.[node.Module.Kernel.Parameters.[1].Name].TotalCount
                            else
                                // Map, map2, rev
                                buffers.[node.Module.Kernel.Parameters.[0].Name].TotalCount
                        let localSize = KernelSetupUtil.ComputeLocalSizeWithGlobalSize(runtimeKernel.CompiledKernelData.Kernel, runtimeKernel.DeviceData.Device, [| globalSize |])

                        // Create barrier
                        let mutable totalGlobalSize = 1L
                        for i in 0.. globalSize.Length - 1 do
                            totalGlobalSize <- totalGlobalSize * globalSize.[i]
                        let barrier = new Barrier(totalGlobalSize |> int)

                        // Spawn threads
                        let methodToExecute = multithreadKernel.KernelData.ParsedSignature
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
                                        methodToExecute.Invoke(null, data) 
                                    }) |> Async.Parallel |> Async.Ignore |> Async.RunSynchronously
                        *)
                        failwith "Not supported yet"
                    
                    // Fail
                    | _ ->
                        raise (new KernelCompilationException("Cannot compile and run kernel " + node.Module.Kernel.Name))
                else
                    None
            | _ ->
                None
                