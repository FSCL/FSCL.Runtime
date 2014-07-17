namespace FSCL

open OpenCL
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open System.Reflection
open Microsoft.FSharp.Linq.RuntimeHelpers
open FSCL.Compiler
open FSCL.Language
open FSCL.Compiler.Configuration
open FSCL.Runtime.Managers
open FSCL.Runtime.RuntimeSteps
open FSCL.Runtime.Scheduling
open FSCL.Runtime
open System
open System.Collections.Generic
open System.Threading
open System.IO
open System.Collections.ObjectModel
open System.Runtime.InteropServices
open System.Collections.Generic
open System.Collections.ObjectModel
open Microsoft.FSharp.Core.LanguagePrimitives

module Runtime =
    let VarArgsToDictionary(args:(string * obj)[]) =    
        let opts = new Dictionary<string, obj>()
        for key, value in args do
            if not (opts.ContainsKey(key)) then
                opts.Add(key, value)
            else
                opts.[key] <- value
        opts

    // The Kernel runner
    type internal Runner =            
        inherit Pipeline

        static member private defConfRoot = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "FSCL.Runtime")
        static member private defConfCompRoot = "Components"

        static member private defComponentsAssemply = 
            [|  typeof<FlowGraphBuildingStep>;
                typeof<ReduceKernelExecutionProcessor> |]
             
        val private globalPool: BufferPoolManager
        val private creationManager: KernelCreationManager
        val mutable private isRunning: bool

        new(comp, metr) = 
            { 
                inherit Pipeline(Runner.defConfRoot, Runner.defConfCompRoot, Runner.defComponentsAssemply) 
                isRunning = false
                globalPool = new BufferPoolManager()
                creationManager = new KernelCreationManager(comp, metr)
            }
    
        new(comp, metr, file: string) = 
            { 
                inherit Pipeline(Runner.defConfRoot, Runner.defConfCompRoot, Runner.defComponentsAssemply, file) 
                isRunning = false
                globalPool = new BufferPoolManager()
                creationManager = new KernelCreationManager(comp, metr)
            }

        new(comp, metr, conf: PipelineConfiguration) =
            { 
                inherit Pipeline(Runner.defConfRoot, Runner.defConfCompRoot, Runner.defComponentsAssemply, conf) 
                isRunning = false
                globalPool = new BufferPoolManager()
                creationManager = new KernelCreationManager(comp, metr)
            }
                                          
        member this.RunExpressionOpenCL(input:Expr, opts: IReadOnlyDictionary<string, obj>, isSubRunning: bool) =  
            if OpenCLPlatform.Platforms.Count = 0 then
                raise (new KernelCompilationException("No OpnCL device has been found on your platform"))

            let result = this.Run((input, this.creationManager, this.globalPool), opts) :?> ExecutionOutput

            // Read output buffers
            if not isSubRunning then
                this.globalPool.TransferBackModifiedBuffers()

            // Check the persistency of buffer pool
            let persistency = 
                if opts.ContainsKey(RuntimeOptions.BufferPoolPersistency) then
                    opts.[RuntimeOptions.BufferPoolPersistency] :?> BufferPoolPersistency
                else
                    BufferPoolPersistency.PersistencyInsideExpression
                                                                           
            // If has return buffer read it
            match result with
            | ReturnedValue(v) ->
                // Clear pool only if this is not a subrunning (a running in a sequential function)
                if not isSubRunning then
                    // Dispose all buffers if PersistencyInsideExpressions
                    if persistency = BufferPoolPersistency.PersistencyInsideExpression then
                        this.globalPool.ClearTrackedAndUntrackedPool()
                    else
                        this.globalPool.ClearUntrackedPoolOnly()
                else
                    this.globalPool.ClearUntrackedPoolOnly()
                v
            | _ ->
                let v = this.globalPool.ReadRootBuffer()
                // Clear pool only if this is not a subrunning (a running in a sequential function)
                if not isSubRunning then
                    // Dispose all buffers is PersistencyInsideExpressions
                    if persistency = BufferPoolPersistency.PersistencyInsideExpression then
                        this.globalPool.ClearTrackedAndUntrackedPool()
                    else
                        this.globalPool.ClearUntrackedPoolOnly()
                else
                    this.globalPool.ClearUntrackedPoolOnly()
                v :> obj
                            
        member this.RunExpressionMultithread(input:Expr, opts: IReadOnlyDictionary<string, obj>, isSubRunning: bool) =    
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
        member this.RunExpression(expr: Expr<'T>, 
                                  globalSize: int64 array, 
                                  localSize: int64 array, 
                                  globalOffset: int64 array, 
                                  mode: RunningMode, 
                                  fallback: bool,
                                  opt: Dictionary<string, obj>) =
            
            let isSubRunning = this.isRunning
            this.isRunning <- true

            // Copy options
            let opts = new Dictionary<string, obj>()
            for item in opt do
                opts.Add(item.Key, item.Value)

            // Add options for work size and running mode
            if not(opts.ContainsKey(RuntimeOptions.WorkSize) && globalSize.Length > 0 && localSize.Length > 0) then
                opts.Add(RuntimeOptions.WorkSize, (globalSize, localSize, globalOffset))
            if not(opts.ContainsKey(RuntimeOptions.RunningMode)) then
                opts.Add(RuntimeOptions.RunningMode, mode)
            if not(opts.ContainsKey(RuntimeOptions.MultithreadFallback)) then
                opts.Add(RuntimeOptions.MultithreadFallback, fallback)                      
                      
            // If global or local size empty theyshould be embedded in kernel expression 
            let result = match mode with
                         | RunningMode.OpenCL ->                
                             this.RunExpressionOpenCL(expr, opts, isSubRunning)
                         | RunningMode.Multithread ->
                             this.RunExpressionMultithread(expr, opts, isSubRunning)
                         | _ ->              
                             this.RunExpressionMultithread(expr, opts, isSubRunning)

            if not isSubRunning then
                this.isRunning <- false
            result       
                   
        member this.IterateExpression(expr: Expr<'T>, 
                                      itSetup: int -> (int64[] *int64[] *int64[] * obj list) option,
                                      opt: Dictionary<string, obj>) =
            
            let isSubRunning = this.isRunning
            this.isRunning <- true

            // Copy options
            let opts = new Dictionary<string, obj>()
            for item in opt do
                opts.Add(item.Key, item.Value)

            opts.Add("IterativeSetup", itSetup)                    
                      
            // If global or local size empty theyshould be embedded in kernel expression 
            let result = this.RunExpressionOpenCL(expr, opts, isSubRunning)

            if not isSubRunning then
                this.isRunning <- false
            result       

        member this.GetLocalMemorySizeForSingleKernelExpression(e: Expr) =
            let opts = new Dictionary<string, obj>()
            opts.Add(RuntimeOptions.CreateOnly, true)
            let result = this.Run((e, this.creationManager, this.globalPool), opts) :?> FlowGraphNode
            if result :? KernelFlowGraphNode then
                let node = result :?> KernelFlowGraphNode
                node.CompiledKernelData.Kernel.GetLocalMemorySize(node.DeviceData.Device)
            else
                raise (new KernelQueryException("The expression to query doesn't contain a single OpenCL kernel call or reference"))

        member this.GetCompileWorkGroupSizeForSingleKernelExpression(e: Expr) =
            let opts = new Dictionary<string, obj>()
            opts.Add(RuntimeOptions.CreateOnly, true)
            let result = this.Run((e, this.creationManager, this.globalPool), opts) :?> FlowGraphNode
            if result :? KernelFlowGraphNode then
                let node = result :?> KernelFlowGraphNode
                node.CompiledKernelData.Kernel.GetCompileWorkGroupSize(node.DeviceData.Device)
            else
                raise (new KernelQueryException("The expression to query doesn't contain a single OpenCL kernel call or reference"))

        member this.GetPreferredWorkGroupSizeMultipleForSingleKernelExpression(e: Expr) =
            let opts = new Dictionary<string, obj>()
            opts.Add(RuntimeOptions.CreateOnly, true)
            let result = this.Run((e, this.creationManager, this.globalPool), opts) :?> FlowGraphNode
            if result :? KernelFlowGraphNode then
                let node = result :?> KernelFlowGraphNode
                node.CompiledKernelData.Kernel.GetPreferredWorkGroupSizeMultiple(node.DeviceData.Device)
            else
                raise (new KernelQueryException("The expression to query doesn't contain a single OpenCL kernel call or reference"))

        member this.GetPrivateMemorySizeForSingleKernelExpression(e: Expr) =
            let opts = new Dictionary<string, obj>()
            opts.Add(RuntimeOptions.CreateOnly, true)
            let result = this.Run((e, this.creationManager, this.globalPool), opts) :?> FlowGraphNode
            if result :? KernelFlowGraphNode then
                let node = result :?> KernelFlowGraphNode
                node.CompiledKernelData.Kernel.GetPrivateMemorySize(node.DeviceData.Device)
            else
                raise (new KernelQueryException("The expression to query doesn't contain a single OpenCL kernel call or reference"))

        member this.GetWorkGroupSizeForSingleKernelExpression(e: Expr) =
            let opts = new Dictionary<string, obj>()
            opts.Add(RuntimeOptions.CreateOnly, true)
            let result = this.Run((e, this.creationManager, this.globalPool), opts) :?> FlowGraphNode
            if result :? KernelFlowGraphNode then
                let node = result :?> KernelFlowGraphNode
                node.CompiledKernelData.Kernel.GetWorkGroupSize(node.DeviceData.Device)
            else
                raise (new KernelQueryException("The expression to query doesn't contain a single OpenCL kernel call or reference"))

        member this.ForceClearPool(transferBackBuffers: bool) =
            if transferBackBuffers then
                // Read output buffers
                this.globalPool.TransferBackModifiedBuffers()   
            this.globalPool.ClearTrackedAndUntrackedPool()

        interface IDisposable with
            member this.Dispose() =
                (this.globalPool :> IDisposable).Dispose()
                (this.creationManager :> IDisposable).Dispose()

    // Global kernel runner
    let mutable internal kernelRunner = new Runner(new Compiler(), None)

    // Function to set custom kernel manager
    let Init(compiler, metric) =
        kernelRunner <- new Runner(compiler, metric)

    let ForceClearPool(transferBackBuffers: bool) =
        kernelRunner.ForceClearPool(transferBackBuffers)

    // List available devices
    let GetOpenCLPlatforms() = 
        let plats = new List<int * string * ReadOnlyCollection<int * string * DeviceType>>()
        for p = 0 to OpenCLPlatform.Platforms.Count - 1 do
            let platform = OpenCLPlatform.Platforms.[p]
            let devs = new List<int * string * DeviceType>()
            for i = 0 to platform.Devices.Count - 1 do
                devs.Add((i, platform.Devices.[i].Name, EnumOfValue<int, DeviceType> (platform.Devices.[i].Type |> int)))
            plats.Add((p, platform.Name, devs.AsReadOnly()))
        plats.AsReadOnly()
                               
    // Extension methods to run a quoted kernel
    type Expr<'T> with
        member this.Run() =
            kernelRunner.RunExpression(this, 
                                        [||], [||], [||],
                                        RunningMode.OpenCL, true, 
                                        Dictionary<string, obj>()) :?> 'T
        member this.Run(globalSize: int64, localSize: int64) =
            kernelRunner.RunExpression(this, 
                                        [| globalSize |], [| localSize |], [||],
                                        RunningMode.OpenCL, true, 
                                        Dictionary<string, obj>()) :?> 'T
        member this.Run(globalSize: int64 array, localSize: int64 array) =
            kernelRunner.RunExpression(this, 
                                        globalSize, localSize, [||],
                                        RunningMode.OpenCL, true, 
                                        Dictionary<string, obj>()) :?> 'T
        member this.Run(globalSize: int64 array, localSize: int64 array, offset: int64 array) =
            kernelRunner.RunExpression(this, 
                                        globalSize, localSize, offset,
                                        RunningMode.OpenCL, true, 
                                        Dictionary<string, obj>()) :?> 'T
            
        member this.Run(opts) =
            kernelRunner.RunExpression(this, 
                                        [||], [||], [||], 
                                        RunningMode.OpenCL, true, 
                                        opts) :?> 'T
        member this.Run(globalSize: int64, localSize: int64, opts) =
            kernelRunner.RunExpression(this, 
                                        [| globalSize |], [| localSize |], [||], 
                                        RunningMode.OpenCL, true, 
                                        opts) :?> 'T
        member this.Run(globalSize: int64 array, localSize: int64 array, opts) =
            kernelRunner.RunExpression(this, 
                                        globalSize, localSize, [||], 
                                        RunningMode.OpenCL, true, 
                                        opts) :?> 'T
        member this.Run(globalSize: int64 array, localSize: int64 array, globalOffset: int64 array, opts) =
            kernelRunner.RunExpression(this, 
                                        globalSize, localSize, globalOffset,
                                        RunningMode.OpenCL, true, 
                                        opts) :?> 'T
                                        
        member this.Run([<ParamArray>] args:(string * obj)[]) =            
            kernelRunner.RunExpression(this, 
                                        [||], [||], [||],
                                        RunningMode.OpenCL, true, 
                                        VarArgsToDictionary(args)) :?> 'T
        member this.Run(globalSize: int64, localSize: int64, [<ParamArray>] args:(string * obj)[]) =
            kernelRunner.RunExpression(this, 
                                        [| globalSize |], [| localSize |], [| 0L |], 
                                        RunningMode.OpenCL, true, 
                                        VarArgsToDictionary(args)) :?> 'T
        member this.Run(globalSize: int64 array, localSize: int64 array, [<ParamArray>] args:(string * obj)[]) =
            kernelRunner.RunExpression(this, 
                                        globalSize, localSize, [| 0L |], 
                                        RunningMode.OpenCL, true,  
                                        VarArgsToDictionary(args)) :?> 'T
        member this.Run(globalSize: int64 array, localSize: int64 array, globalOffset: int64 array, [<ParamArray>] args:(string * obj)[]) =
            kernelRunner.RunExpression(this, 
                                        globalSize, localSize, globalOffset, 
                                        RunningMode.OpenCL, true,  
                                        VarArgsToDictionary(args)) :?> 'T
        member this.Iterate(itSetup: int -> (int64[] * int64[] * int64[] * obj list) option) =
            kernelRunner.IterateExpression(this, 
                                           itSetup, 
                                           Dictionary<string, obj>()) :?> 'T
            
        member this.RunSequential() =
            kernelRunner.RunExpression(this, 
                                        [||], [||], [||], 
                                        RunningMode.Sequential, true, 
                                        Dictionary<string, obj>()) :?> 'T
        member this.RunSequential(globalSize: int64, localSize: int64) =
            kernelRunner.RunExpression(this, 
                                        [| globalSize |], [| localSize |], [| 0L |], 
                                        RunningMode.Sequential, true, 
                                        Dictionary<string, obj>()) :?> 'T
        member this.RunSequential(globalSize: int64 array, localSize: int64 array) =
            kernelRunner.RunExpression(this, 
                                        globalSize, localSize, [| 0L |], 
                                        RunningMode.Sequential, true, 
                                        Dictionary<string, obj>()) :?> 'T
            
        member this.RunSequential(opts) =
            kernelRunner.RunExpression(this, 
                                        [||], [||], [||], 
                                        RunningMode.Sequential, true, 
                                        opts) :?> 'T
        member this.RunSequential(globalSize: int64, localSize: int64, opts) =
            kernelRunner.RunExpression(this, 
                                        [| globalSize |], [| localSize |], [| 0L |], 
                                        RunningMode.Sequential, true, 
                                        opts) :?> 'T
        member this.RunSequential(globalSize: int64 array, localSize: int64 array, opts) =
            kernelRunner.RunExpression(this, 
                                        globalSize, localSize, [||], 
                                        RunningMode.Sequential, true, 
                                        opts) :?> 'T
                                        
        member this.RunSequential([<ParamArray>] args: (string * obj)[]) =
            kernelRunner.RunExpression(this, 
                                        [||], [||], [||], 
                                        RunningMode.Sequential, true, 
                                        VarArgsToDictionary(args)) :?> 'T
        member this.RunSequential(globalSize: int64, localSize: int64, [<ParamArray>] args: (string * obj)[]) =
            kernelRunner.RunExpression(this, 
                                        [| globalSize |], [| localSize |], [||], 
                                        RunningMode.Sequential, true, 
                                        VarArgsToDictionary(args)) :?> 'T
        member this.RunSequential(globalSize: int64 array, localSize: int64 array, [<ParamArray>] args: (string * obj)[]) =
            kernelRunner.RunExpression(this, 
                                        globalSize, localSize, [||], 
                                        RunningMode.Sequential, true, 
                                        VarArgsToDictionary(args)) :?> 'T

    // Extension methods to query info about a (single) quoted kernel
    type Expr<'T> with
        member this.GetLocalMemorySize() =
            kernelRunner.GetLocalMemorySizeForSingleKernelExpression(this)

        member this.GetCompileWorkGroupSize() =
            kernelRunner.GetCompileWorkGroupSizeForSingleKernelExpression(this)

        member this.GetPreferredWorkGroupSizeMultiple() =
            kernelRunner.GetPreferredWorkGroupSizeMultipleForSingleKernelExpression(this)

        member this.GetPrivateMemorySize() =
            kernelRunner.GetPrivateMemorySizeForSingleKernelExpression(this)

        member this.GetWorkGroupSize() =
            kernelRunner.GetWorkGroupSizeForSingleKernelExpression(this)
            

