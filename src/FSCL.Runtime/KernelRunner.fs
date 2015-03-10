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
open FSCL.Runtime.CompilerSteps
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
            [| typeof<ReduceKernelExecutionProcessor> |]
             
        val private globalPool: BufferPoolManager
        val private creationManager: KernelCreationManager
        val private compiler: Compiler
        val private deviceCache: DeviceCache 

        new(comp:Compiler, metr) = 
            { 
                inherit Pipeline(Runner.defConfRoot, Runner.defConfCompRoot, Runner.defComponentsAssemply) 
                globalPool = new BufferPoolManager()
                creationManager = new KernelCreationManager(metr)
                compiler = new Compiler(
                            new PipelineConfiguration(
                                comp.Configuration.LoadDefaultSteps, 
                                Array.append (comp.Configuration.Sources) [| new SourceConfiguration(AssemblySource(typeof<RuntimeToCompilerMetadataMapping>.Assembly)) |]), 
                            (fun m -> new RuntimeKernelCacheEntry(m) :> KernelCacheEntry))
                deviceCache = new DeviceCache() 
            }
    
        new(comp:Compiler, metr, file: string) = 
            { 
                inherit Pipeline(Runner.defConfRoot, Runner.defConfCompRoot, Runner.defComponentsAssemply, file) 
                globalPool = new BufferPoolManager()
                creationManager = new KernelCreationManager(metr)      
                compiler = new Compiler(
                            new PipelineConfiguration(
                                comp.Configuration.LoadDefaultSteps, 
                                Array.append (comp.Configuration.Sources) [| new SourceConfiguration(AssemblySource(typeof<RuntimeToCompilerMetadataMapping>.Assembly)) |]), 
                            (fun m -> new RuntimeKernelCacheEntry(m) :> KernelCacheEntry))
                deviceCache = new DeviceCache() 
            }
            

        new(comp:Compiler, metr, conf: PipelineConfiguration) =
            { 
                inherit Pipeline(Runner.defConfRoot, Runner.defConfCompRoot, Runner.defComponentsAssemply, conf) 
                globalPool = new BufferPoolManager()
                creationManager = new KernelCreationManager(metr)                
                compiler = new Compiler(
                            new PipelineConfiguration(
                                comp.Configuration.LoadDefaultSteps, 
                                Array.append (comp.Configuration.Sources) [| new SourceConfiguration(AssemblySource(typeof<RuntimeToCompilerMetadataMapping>.Assembly)) |]), 
                            (fun m -> new RuntimeKernelCacheEntry(m) :> KernelCacheEntry))
                deviceCache = new DeviceCache() 
            }
                                          
        member this.RunExpressionOpenCL(input:Expr, opts: IReadOnlyDictionary<string, obj>) =  
            if OpenCLPlatform.Platforms.Count = 0 then
                raise (new KernelCompilationException("No OpenCL device has been found on your platform"))

            // Compile expression
            let compiled = this.compiler.Compile(input) :?> IKernelExpression

            // Now run
            let result = this.Run((compiled, this.globalPool, this.creationManager), opts) :?> ExecutionOutput
            
            // Check the persistency of buffer pool
            let persistency = 
                if opts.ContainsKey(RuntimeOptions.BufferPoolPersistency) then
                    opts.[RuntimeOptions.BufferPoolPersistency] :?> BufferPoolPersistency
                else
                    BufferPoolPersistency.PersistencyInsideExpression
                                                                           
            // If has return buffer read it
            match result with
            | ReturnedValue(v) ->
                // Dispose all buffers if PersistencyInsideExpressions
                if persistency = BufferPoolPersistency.PersistencyInsideExpression then
                    this.globalPool.ClearTrackedAndUntrackedPool()
                else
                    this.globalPool.ClearUntrackedPoolOnly()
                v
            | _ ->
                let v = this.globalPool.ReadRootBuffer()
                // Dispose all buffers is PersistencyInsideExpressions
                if persistency = BufferPoolPersistency.PersistencyInsideExpression then
                    this.globalPool.ClearTrackedAndUntrackedPool()
                else
                    this.globalPool.ClearUntrackedPoolOnly()
                v :> obj
                            
        member this.RunExpressionMultithread(input:Expr, opts: IReadOnlyDictionary<string, obj>) =    
            let result = this.Run((input, this.creationManager, this.globalPool), opts) :?> ExecutionOutput
                                                                   
            // If has return buffer read it
            match result with
            | ReturnedValue(v) ->                
                v
            | _ ->
                raise (new KernelExecutionException("A multithread kernel cannot return a buffer"))
        
        // Run a kernel through a quoted kernel call        
        member this.RunExpression(expr: Expr, 
                                  mode: RunningMode, 
                                  fallback: bool,
                                  opt: Dictionary<string, obj>) =
            
            // Copy options
            let opts = new Dictionary<string, obj>()
            for item in opt do
                opts.Add(item.Key, item.Value)

            // Add options for work size and running mode
            if not(opts.ContainsKey(RuntimeOptions.RunningMode)) then
                opts.Add(RuntimeOptions.RunningMode, mode)
            if not(opts.ContainsKey(RuntimeOptions.MultithreadFallback)) then
                opts.Add(RuntimeOptions.MultithreadFallback, fallback)                      
                      
            // If global or local size empty theyshould be embedded in kernel expression 
            let result = match mode with
                         | RunningMode.OpenCL ->                
                             this.RunExpressionOpenCL(expr, opts)
                         | RunningMode.Multithread ->
                             this.RunExpressionMultithread(expr, opts)
                         | _ ->              
                             this.RunExpressionMultithread(expr, opts)
            result       
                   
        member this.IterateExpression(expr: Expr, 
                                      itSetup: int -> obj list option,
                                      opt: Dictionary<string, obj>) =
            // Copy options
            let opts = new Dictionary<string, obj>()
            for item in opt do
                opts.Add(item.Key, item.Value)

            opts.Add("IterativeSetup", itSetup)                    
                      
            // If global or local size empty theyshould be embedded in kernel expression 
            let result = this.RunExpressionOpenCL(expr, opts)
            result       
//
//        member this.GetLocalMemorySizeForSingleKernelExpression(e: Expr) =
//            let opts = new Dictionary<string, obj>()
//            opts.Add(RuntimeOptions.CreateOnly, true)
//            // Compile expression
//            let compiled = this.compiler.Compile(e) :?> IKernelExpression
//            let result = this.Run((compiled.KFGRoot, this.creationManager, this.globalPool), opts)
//            if result :? OpenCLKernelFlowGraphNode then
//                let node = result :?> OpenCLKernelFlowGraphNode
//                node.CompiledKernelData.Kernel.GetLocalMemorySize(node.DeviceData.Device)
//            else
//                raise (new KernelQueryException("The expression to query doesn't contain a single OpenCL kernel call or reference"))
//
//        member this.GetCompileWorkGroupSizeForSingleKernelExpression(e: Expr) =
//            let opts = new Dictionary<string, obj>()
//            opts.Add(RuntimeOptions.CreateOnly, true)
//            let result = this.Run((e, this.creationManager, this.globalPool), opts) :?> FlowGraphNode
//            if result :? OpenCLKernelFlowGraphNode then
//                let node = result :?> OpenCLKernelFlowGraphNode
//                node.CompiledKernelData.Kernel.GetCompileWorkGroupSize(node.DeviceData.Device)
//            else
//                raise (new KernelQueryException("The expression to query doesn't contain a single OpenCL kernel call or reference"))
//
//        member this.GetPreferredWorkGroupSizeMultipleForSingleKernelExpression(e: Expr) =
//            let opts = new Dictionary<string, obj>()
//            opts.Add(RuntimeOptions.CreateOnly, true)
//            let result = this.Run((e, this.creationManager, this.globalPool), opts) :?> FlowGraphNode
//            if result :? OpenCLKernelFlowGraphNode then
//                let node = result :?> OpenCLKernelFlowGraphNode
//                node.CompiledKernelData.Kernel.GetPreferredWorkGroupSizeMultiple(node.DeviceData.Device)
//            else
//                raise (new KernelQueryException("The expression to query doesn't contain a single OpenCL kernel call or reference"))
//
//        member this.GetPrivateMemorySizeForSingleKernelExpression(e: Expr) =
//            let opts = new Dictionary<string, obj>()
//            opts.Add(RuntimeOptions.CreateOnly, true)
//            let result = this.Run((e, this.creationManager, this.globalPool), opts) :?> FlowGraphNode
//            if result :? OpenCLKernelFlowGraphNode then
//                let node = result :?> OpenCLKernelFlowGraphNode
//                node.CompiledKernelData.Kernel.GetPrivateMemorySize(node.DeviceData.Device)
//            else
//                raise (new KernelQueryException("The expression to query doesn't contain a single OpenCL kernel call or reference"))
//
//        member this.GetWorkGroupSizeForSingleKernelExpression(e: Expr) =
//            let opts = new Dictionary<string, obj>()
//            opts.Add(RuntimeOptions.CreateOnly, true)
//            let result = this.Run((e, this.creationManager, this.globalPool), opts) :?> FlowGraphNode
//            if result :? OpenCLKernelFlowGraphNode then
//                let node = result :?> OpenCLKernelFlowGraphNode
//                node.CompiledKernelData.Kernel.GetWorkGroupSize(node.DeviceData.Device)
//            else
//                raise (new KernelQueryException("The expression to query doesn't contain a single OpenCL kernel call or reference"))

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
                devs.Add((i, platform.Devices.[i].VendorID.ToString(), EnumOfValue<int, DeviceType> (platform.Devices.[i].Type |> int)))
            plats.Add((p, platform.Vendor, devs.AsReadOnly()))
        plats.AsReadOnly()
                               
    // Extension methods to run a quoted kernel
    type Expr<'T> with
        member this.Run() =
            kernelRunner.RunExpression(this, 
                                        RunningMode.OpenCL, true, 
                                        Dictionary<string, obj>()) :?> 'T            
        member this.Run(opts) =
            kernelRunner.RunExpression(this, 
                                        RunningMode.OpenCL, true, 
                                        opts) :?> 'T                                        
        member this.Run([<ParamArray>] args:(string * obj)[]) =            
            kernelRunner.RunExpression(this, 
                                        RunningMode.OpenCL, true, 
                                        VarArgsToDictionary(args)) :?> 'T
        member this.RunMultithread() =
            kernelRunner.RunExpression(this, 
                                        RunningMode.Multithread, true, 
                                        Dictionary<string, obj>()) :?> 'T            
        member this.RunMultithread(opts) =
            kernelRunner.RunExpression(this, 
                                        RunningMode.Multithread, true, 
                                        opts) :?> 'T                                        
        member this.RunMultithread([<ParamArray>] args:(string * obj)[]) =            
            kernelRunner.RunExpression(this, 
                                        RunningMode.Multithread, true, 
                                        VarArgsToDictionary(args)) :?> 'T                                        
        member this.RunSequential() =
            kernelRunner.RunExpression(this, 
                                        RunningMode.Sequential, true, 
                                        Dictionary<string, obj>()) :?> 'T            
        member this.RunSequential(opts) =
            kernelRunner.RunExpression(this, 
                                        RunningMode.Sequential, true, 
                                        opts) :?> 'T                                        
        member this.RunSequential([<ParamArray>] args:(string * obj)[]) =            
            kernelRunner.RunExpression(this, 
                                        RunningMode.Sequential, true, 
                                        VarArgsToDictionary(args)) :?> 'T
        member this.Iterate(itSetup: int -> obj list option) =
            kernelRunner.IterateExpression(this, 
                                           itSetup, 
                                           Dictionary<string, obj>()) :?> 'T
                                              
    type Expr with
        member this.Run() =
            kernelRunner.RunExpression(this, 
                                        RunningMode.OpenCL, true, 
                                        Dictionary<string, obj>())           
        member this.Run(opts) =
            kernelRunner.RunExpression(this, 
                                        RunningMode.OpenCL, true, 
                                        opts)                                
        member this.Run([<ParamArray>] args:(string * obj)[]) =            
            kernelRunner.RunExpression(this, 
                                        RunningMode.OpenCL, true, 
                                        VarArgsToDictionary(args)) 
        member this.RunMultithread() =
            kernelRunner.RunExpression(this, 
                                        RunningMode.Multithread, true, 
                                        Dictionary<string, obj>())             
        member this.RunMultithread(opts) =
            kernelRunner.RunExpression(this, 
                                        RunningMode.Multithread, true, 
                                        opts)                             
        member this.RunMultithread([<ParamArray>] args:(string * obj)[]) =            
            kernelRunner.RunExpression(this, 
                                        RunningMode.Multithread, true, 
                                        VarArgsToDictionary(args))                                         
        member this.RunSequential() =
            kernelRunner.RunExpression(this, 
                                        RunningMode.Sequential, true, 
                                        Dictionary<string, obj>())           
        member this.RunSequential(opts) =
            kernelRunner.RunExpression(this, 
                                        RunningMode.Sequential, true, 
                                        opts) :?> 'T                                        
        member this.RunSequential([<ParamArray>] args:(string * obj)[]) =            
            kernelRunner.RunExpression(this, 
                                        RunningMode.Sequential, true, 
                                        VarArgsToDictionary(args)) 
        member this.Iterate(itSetup: int -> obj list option) =
            kernelRunner.IterateExpression(this, 
                                           itSetup, 
                                           Dictionary<string, obj>()) 
                                              

    // Extension methods to query info about a (single) quoted kernel
//    type Expr<'T> with
//        member this.GetLocalMemorySize() =
//            kernelRunner.GetLocalMemorySizeForSingleKernelExpression(this)
//
//        member this.GetCompileWorkGroupSize() =
//            kernelRunner.GetCompileWorkGroupSizeForSingleKernelExpression(this)
//
//        member this.GetPreferredWorkGroupSizeMultiple() =
//            kernelRunner.GetPreferredWorkGroupSizeMultipleForSingleKernelExpression(this)
//
//        member this.GetPrivateMemorySize() =
//            kernelRunner.GetPrivateMemorySizeForSingleKernelExpression(this)
//
//        member this.GetWorkGroupSize() =
//            kernelRunner.GetWorkGroupSizeForSingleKernelExpression(this)
            

