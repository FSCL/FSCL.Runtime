namespace FSCL.Runtime

open Cloo
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open System.Reflection
open Microsoft.FSharp.Linq.RuntimeHelpers
open FSCL.Compiler
open FSCL.Compiler.KernelLanguage
open System
open System.Collections.Generic
open System.Threading
open System.Collections.ObjectModel
open System.Runtime.InteropServices
open FSCL.Runtime.KernelExecution
open System.Collections.Generic
open System.Collections.ObjectModel

module KernelRunner =
    // The Kernel runner
    type internal Runner(compiler, executionManager, metric) =    
        member val KernelCreationManager = new KernelCreationManager(compiler, metric) with get
        member val KernelExecutionManager:KernelExecutionManager = executionManager with get
                        
        member this.RunOpenCL(input:KernelExecutionInput, opts: ReadOnlyDictionary<string, obj>) =
            this.KernelExecutionManager.Execute(input, opts)
                            
        member this.RunMultithread(kernelData: RuntimeKernelData,
                                   compiledData: RuntimeCompiledKernelData,
                                   globalSize: int64 array, 
                                   localSize: int64 array, 
                                   multithread: bool) =
            raise (KernelSetupException("Multithreading support under development"))
            (*                                               
            // Normalize dimensions of workspace
            // If the workspace is one dim, treansform into 3 dims with the second and the third equals to 1 (thread)
            let normalizedGlobalSize, normalizedLocalSize = 
                match globalSize.Rank with
                | 1 ->
                    ([| globalSize.[0]; 1; 1 |], [| localSize.[0]; 1; 1 |])
                | 2 ->
                    ([| globalSize.[0]; globalSize.[1]; 1 |], [| localSize.[0]; localSize.[1]; 1 |])
                | _ ->
                    (globalSize, localSize)

            // Analyze connections to build a parameter-based indexed structure
            let ic, oc = this.BuildParameterBasedConnections(inputConnections, outputConnections)

            // The list of arguments of the kernel (that must be passed to each cpu thread)
            let arguments = new List<Object>()
            // The storage for buffers potentially used by successive kernels in the call graph flow
            let mutable argIndex = 0
            // To remember the array associated to the length parameters
            let sizeParametersBinding = new Dictionary<string, int>()
            // Foreach argument of the kernel
            for par in kernelData.Info.Parameters do      
                if par.Type.IsArray then             
                    // Check if this is the input coming from a previously executed kernel
                    if not (ic.ContainsKey(par.Name)) then
                        // No input from a  kernel, there must be an input from an actual argument
                        if par.ArgumentExpression.IsNone then
                            raise (new KernelSetupException("Cannot setup the parameter " + par.Name + " for the kernel " + kernelData.Info.Name + ": the parameter is not bound to any other kernel and no associated actual argument has been found"))
                        let o = par.ArgumentExpression.Value.EvalUntyped()                        
                        // Store buffer/object data
                        if not (bufferBinding.ContainsKey(kernelData.Info.ID)) then
                            bufferBinding.Add(kernelData.Info.ID, new Dictionary<string, ComputeMemory * Object * int array>())
                        bufferBinding.[kernelData.Info.ID].Add(par.Name, (null, o, null))
                        // Add the argument value to the list
                        arguments.Add(o)
                    // We get the input from a previously-executed kernel
                    else
                        let _, inputBuffer, _ = bufferBinding.[fst (ic.[par.Name])].[snd (ic.[par.Name])]
                        // Check if this can be written to be the input of a successive kernel
                        if not (bufferBinding.ContainsKey(kernelData.Info.ID)) then
                            bufferBinding.Add(kernelData.Info.ID, new Dictionary<string, ComputeMemory * Object * int array>())
                        bufferBinding.[kernelData.Info.ID].Add(par.Name, (null, inputBuffer, null))      
                        // Add the argument value to the list
                        arguments.Add(inputBuffer)          
                else
                    // Add the argument value to the list
                    arguments.Add(par.ArgumentExpression.Value.EvalUntyped())
            // Process next parameter
            argIndex <- argIndex + 1

            // Finalize the arguments (List -> array)            
            // Add fake additional element to the list of arguments. It will be replaced with the appropriate instance of WorkItemIdContainer foreach thread executed
            arguments.Add(new Object())
            let finalArguments = Array.ofSeq(arguments)

            // Run kernel
            // Launch threads or execute sequential
            let work = kernelData.MultithreadVersion.Value
            for i = 0 to normalizedGlobalSize.[0] - 1 do
                for j = 0 to normalizedGlobalSize.[1] - 1 do
                    for k = 0 to normalizedGlobalSize.[2] - 1 do
                        // Create a ids container for each thread and run the thread
                        let container = new WorkItemIdContainer(globalSize, 
                                                                localSize, 
                                                                [| i; j; k |], 
                                                                [| i / normalizedGlobalSize.[0]; j / normalizedGlobalSize.[1]; k / normalizedGlobalSize.[2] |],
                                                                [| 0; 0; 0 |])
                        // Set container as last paramenter
                        finalArguments.[finalArguments.Length - 1] <- container :> obj
                        if multithread then
                            // Create thread
                            let t = new Thread(new ThreadStart(fun () -> work.Invoke(null, finalArguments) |> ignore))
                            t.Start()
                        else
                            work.Invoke(null, finalArguments) |> ignore            
            () :> obj
            *)
        
        // Run a kernel through a quoted kernel call        
        member this.Run(expr: Expr, 
                        globalSize: int64 array, 
                        localSize: int64 array, 
                        mode: KernelRunningMode, 
                        fallback: bool,
                        opts: ReadOnlyDictionary<string, obj>) =
            // If global or local size empty theyshould be embedded in kernel expression
            let runtimeInfo, callGraphRoot = this.KernelCreationManager.Process(expr, mode, fallback, opts)    
            match mode with
            | KernelRunningMode.OpenCL ->                
                this.RunOpenCL(new KernelExecutionInput(true, callGraphRoot, runtimeInfo, globalSize, localSize), opts)
            | KernelRunningMode.Multithread ->
                this.RunMultithread(null, null, globalSize, localSize, true)
            | _ ->              
                this.RunMultithread(null, null, globalSize, localSize, false)
       
    // Global kernel runner
    let mutable internal kernelRunner = new Runner(new Compiler(), new KernelExecutionManager(), None)

    // Function to set custom kernel manager
    let Init(compiler, executionManager: KernelExecutionManager option, metric) =
        if(executionManager.IsNone) then
            kernelRunner <- new Runner(compiler, new KernelExecutionManager(), metric)
        else
            kernelRunner <- new Runner(compiler, executionManager.Value, metric)

    // List available devices
    let ListDevices() = 
        List.ofSeq(seq {
                        for platform in Cloo.ComputePlatform.Platforms do
                            yield List.ofSeq(seq {
                                                    for device in platform.Devices do
                                                        yield (device.VendorId, device.Name)
                                             })
                   })
                               
    // Extension methods to run a quoted kernel
    type Expr<'T> with
        member this.Run() =
            kernelRunner.Run(this, 
                            [||], [||], 
                            KernelRunningMode.OpenCL, true, 
                            ReadOnlyDictionary<string, obj>(Dictionary<string, obj>())) :?> 'T
        member this.Run(globalSize: int64, localSize: int64) =
            kernelRunner.Run(this, 
                            [| globalSize |], [| localSize |], 
                            KernelRunningMode.OpenCL, true, 
                            ReadOnlyDictionary<string, obj>(Dictionary<string, obj>())) :?> 'T
        member this.Run(globalSize: int64 array, localSize: int64 array) =
            kernelRunner.Run(this, 
                            globalSize, localSize, 
                            KernelRunningMode.OpenCL, true, 
                            ReadOnlyDictionary<string, obj>(Dictionary<string, obj>())) :?> 'T
            
        member this.Run(opts) =
            kernelRunner.Run(this, 
                            [||], [||], 
                            KernelRunningMode.OpenCL, true, 
                            opts) :?> 'T
        member this.Run(globalSize: int64, localSize: int64, opts) =
            kernelRunner.Run(this, 
                            [| globalSize |], [| localSize |], 
                            KernelRunningMode.OpenCL, true, 
                            opts) :?> 'T
        member this.Run(globalSize: int64 array, localSize: int64 array, opts) =
            kernelRunner.Run(this, 
                            globalSize, localSize, 
                            KernelRunningMode.OpenCL, true, 
                            opts) :?> 'T
            
        member this.RunSequential() =
            kernelRunner.Run(this, 
                            [||], [||], 
                            KernelRunningMode.Sequential, true, 
                            ReadOnlyDictionary<string, obj>(Dictionary<string, obj>())) :?> 'T
        member this.RunSequential(globalSize: int64, localSize: int64) =
            kernelRunner.Run(this, 
                            [| globalSize |], [| localSize |], 
                            KernelRunningMode.Sequential, true, 
                            ReadOnlyDictionary<string, obj>(Dictionary<string, obj>())) :?> 'T
        member this.RunSequential(globalSize: int64 array, localSize: int64 array) =
            kernelRunner.Run(this, 
                            globalSize, localSize, 
                            KernelRunningMode.Sequential, true, 
                            ReadOnlyDictionary<string, obj>(Dictionary<string, obj>())) :?> 'T
            
        member this.RunSequential(opts) =
            kernelRunner.Run(this, 
                            [||], [||], 
                            KernelRunningMode.Sequential, true, 
                            opts) :?> 'T
        member this.RunSequential(globalSize: int64, localSize: int64, opts) =
            kernelRunner.Run(this, 
                            [| globalSize |], [| localSize |], 
                            KernelRunningMode.Sequential, true, 
                            opts) :?> 'T
        member this.RunSequential(globalSize: int64 array, localSize: int64 array, opts) =
            kernelRunner.Run(this, 
                            globalSize, localSize, 
                            KernelRunningMode.Sequential, true, 
                            opts) :?> 'T
            

