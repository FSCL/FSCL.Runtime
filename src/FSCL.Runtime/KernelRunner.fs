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
    let createCacheEntry m =
        new RuntimeKernelCacheEntry(m) :> KernelCacheEntry

    // The Kernel runner
    type internal Runner =            
        inherit Pipeline

        static member private defConfRoot = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "FSCL.Runtime")
        static member private defConfCompRoot = "Components"

        static member private defComponentsAssemply = 
            [| typeof<ReduceKernelExecutionProcessor> |]
             
        val private globalPool: BufferPoolManager
        val private creationManager: KernelCreationManager
        val mutable private compiler: Compiler


        new(comp:Compiler, metr) = 
            { 
                inherit Pipeline(Runner.defConfRoot, Runner.defConfCompRoot, Runner.defComponentsAssemply) 
                globalPool = new BufferPoolManager()
                creationManager = new KernelCreationManager(metr)
                compiler = new Compiler(
                            new PipelineConfiguration(
                                comp.Configuration.LoadDefaultSteps, 
                                Array.append (comp.Configuration.Sources) [| new SourceConfiguration(AssemblySource(typeof<RuntimeToCompilerMetadataMapping>.Assembly)) |]), 
                            createCacheEntry)
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
                            createCacheEntry)
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
                            createCacheEntry)
            }
                                          
        member this.RunExpressionOpenCL(input:Expr, opts: Map<string, obj>) =  
            if OpenCLPlatform.Platforms.Count = 0 then
                raise (new KernelCompilationException("No OpenCL device has been found on your platform"))

            // Compile expression
            let compiled = this.compiler.Compile(input) :?> IKernelExpression

            // Now run
            let result = 
                this.Run((compiled, this.globalPool, this.creationManager), opts) :?> ExecutionOutput
            
            // Check the persistency of buffer pool
            let persistency = 
                if opts.ContainsKey(RuntimeOptions.BufferPoolPersistency) then
                    opts.[RuntimeOptions.BufferPoolPersistency] :?> BufferPoolPersistency
                else
                    BufferPoolPersistency.PersistencyInsideExpression
                                                                           
            // If has return buffer read it
            let returnedValue = 
                match result with
                | ReturnedValue(v) ->
                    // Dispose all buffers if PersistencyInsideExpressions
                    v
                | ReturnedBuffer(b) ->
                    this.globalPool.ReadBuffer(b) :> obj
            
            if persistency = BufferPoolPersistency.PersistencyInsideExpression then
                this.globalPool.ClearTrackedAndUntrackedPool()
            else
                this.globalPool.ClearUntrackedPoolOnly()

            returnedValue
                            
        member this.RunExpressionMultithread(input:Expr, opts: Map<string, obj>) = 
            let compiled = this.compiler.Compile(input, Map.empty.Add(CompilerOptions.ParseOnly, box true)) :?> IKernelExpression   
            let result = this.Run((compiled, this.globalPool, this.creationManager), opts) :?> ExecutionOutput
                                                                   
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
                                  opt: Map<string, obj>) =
            
            // Copy options
            let mutable opts = opt            
            // Add options for work size and running mode
            if not(opts.ContainsKey(RuntimeOptions.RunningMode)) then
                opts <- opts.Add(RuntimeOptions.RunningMode, mode)
            if not(opts.ContainsKey(RuntimeOptions.MultithreadFallback)) then
                opts <- opts.Add(RuntimeOptions.MultithreadFallback, fallback)                      
                      
            // If global or local size empty theyshould be embedded in kernel expression 
            let result = match mode with
                         | RunningMode.OpenCL ->                
                             this.RunExpressionOpenCL(expr, opts)
                         | RunningMode.Multithread ->
                             this.RunExpressionMultithread(expr, opts)
                         | _ ->              
                             this.RunExpressionMultithread(expr, opts)
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
        member this.Run(?opts:Map<string, obj>) =
            kernelRunner.RunExpression(this, 
                                        RunningMode.OpenCL, true, 
                                        if opts.IsSome then opts.Value else Map.empty) :?> 'T                       
        member this.Run(mode:RunningMode,?opts:Map<string, obj>) =
            kernelRunner.RunExpression(this, 
                                        mode, true, 
                                        if opts.IsSome then opts.Value else Map.empty) :?> 'T  
                                              
    type Expr with
        member this.Run(?opts:Map<string, obj>) =
            kernelRunner.RunExpression(this, 
                                        RunningMode.OpenCL, true, 
                                        if opts.IsSome then opts.Value else Map.empty)                   
        member this.Run(mode:RunningMode,?opts:Map<string, obj>) =
            kernelRunner.RunExpression(this, 
                                        mode, true, 
                                        if opts.IsSome then opts.Value else Map.empty)                
                                              

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
            

