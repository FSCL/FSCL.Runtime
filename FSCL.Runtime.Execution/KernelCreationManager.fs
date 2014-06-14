namespace FSCL.Runtime.Managers

open OpenCL
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open System.Reflection
open System.Collections.Generic
open FSCL
open FSCL.Compiler
open FSCL.Compiler.Util
open FSCL.Compiler.Configuration
open FSCL.Language
open FSCL.Runtime
open FSCL.Runtime.Scheduling.Metric
open System
open System.Reflection
open FSCL.Runtime.CompilerSteps
    
[<AllowNullLiteral>]
type KernelCreationManager(compiler: Compiler, 
                           metric: SchedulingMetric option) =  

    // The data structure caching devices texts) and (queues, concompiled kernels 
    member val private GlobalCache = new RuntimeCache(compiler.IsInvariantToMetaCollection, fun(a,b) -> true) with get
    // The metric used to select the best devices for a kernel
    member val SchedulingMetric = metric with get             
    // The FSCL compiler   
    // Add the additional steps for the compiler to skip kernel recompilation and produce separated source codes for kernels
    member val Compiler = new Compiler(
                            new PipelineConfiguration(
                                compiler.Configuration.LoadDefaultSteps, 
                                Array.append (compiler.Configuration.Sources) [| new SourceConfiguration(AssemblySource(typeof<KernelModuleParser>.Assembly)) |]))
          with get
    // The multithread adaptor (compiler) to run kernels using multithreading
    member val private KernelAdaptor = new MultithreadKernelAdaptor() with get
         
    // Utility function to know if OpenCL is available
    member private this.IsOpenCLAvailable() =
        if OpenCLPlatform.Platforms.Count = 0 then
            false
        else
            // At least one device in a platform available
            OpenCLPlatform.Platforms |> 
                Seq.map(fun (p: OpenCLPlatform) -> p.Devices.Count) |>
                Seq.reduce(fun a b -> a + b) > 0

    // Utility function to store kernels found all around the assembly. Called by the constructor
    member private this.OpenCLBackendAndStore(runtimeKernel: RuntimeKernel,
                                              platformIndex, deviceIndex, opts: IReadOnlyDictionary<string, obj>) =
        let mutable device = null
        let mutable compiledKernel = null
        
        //#1: Check if there is the corresponding device info have been set
        if not (this.GlobalCache.Devices.ContainsKey((platformIndex, deviceIndex))) then        
            // Get OpenCL info
            let platform = OpenCLPlatform.Platforms.[platformIndex]
            let dev = platform.Devices.[deviceIndex]   
            let devs = new System.Collections.Generic.List<OpenCLDevice>();
            devs.Add(dev)
            // Create context and queue
            let contextProperties = new OpenCLContextPropertyList(platform)
            let computeContext = new OpenCLContext(devs, contextProperties, null, System.IntPtr.Zero) 
            let computeQueue = new OpenCLCommandQueue(computeContext, dev, OpenCLCommandQueueProperties.None) 
            // Add device to the global devices cache
            this.GlobalCache.Devices.Add((platformIndex, deviceIndex), new RuntimeDevice(dev, computeContext, computeQueue))
        device <- this.GlobalCache.Devices.[platformIndex, deviceIndex]
                        
        //#2: Evaluate dynamic defines      
        let dynDefines = new Dictionary<string, string>()
        if opts.ContainsKey(RuntimeOptions.ConstantDefines) then
            let defs = opts.[RuntimeOptions.ConstantDefines] :?> (string * obj) list            
            for key, value in defs do
                dynDefines.Add(key, value.ToString())

        //#3: Check if the device target code has been already generated with mathing values of dynamic defines          
        let mustGenerateTargetCode = 
            if runtimeKernel.Instances.ContainsKey(platformIndex, deviceIndex) then
                // Check matching defines
                let oldDynDefines = runtimeKernel.Instances.[(platformIndex, deviceIndex)].DynamicDefines
                let mutable sameDefinesValues = true
                for def in dynDefines do
                    if def.Value <> oldDynDefines.[def.Key] then
                        sameDefinesValues <- false
                not sameDefinesValues
            else
                true
        if mustGenerateTargetCode then
            let computeProgram = new OpenCLProgram(device.Context, runtimeKernel.OpenCLCode)
            // Generate define options
            let mutable definesOption = ""
            for d in dynDefines do
                definesOption <- definesOption + "-D " + d.Key + "=" + d.Value + " "
            try
                computeProgram.Build([| device.Device |], definesOption, null, System.IntPtr.Zero)
            with
            | ex -> 
                let log = computeProgram.GetBuildLog(device.Device)
                raise (new KernelCompilationException("Device code generation failed: " + log))
            // Create kernel
            let computeKernel = computeProgram.CreateKernel(runtimeKernel.Kernel.Signature.Name)
            if runtimeKernel.Instances.ContainsKey(platformIndex, deviceIndex) then
                runtimeKernel.Instances.Remove((platformIndex, deviceIndex)) |> ignore            
            // Add kernel implementation to the list of implementations for the given kernel
            let compiledKernel = new RuntimeCompiledKernel(computeProgram, computeKernel, dynDefines)
            runtimeKernel.Instances.Add((platformIndex, deviceIndex), compiledKernel)

        compiledKernel <- runtimeKernel.Instances.[platformIndex, deviceIndex]
          
        //#3: Return device, kernel and compiled kernel
        (device, compiledKernel)
        
    member this.Process(info: Expr,
                        opts: IReadOnlyDictionary<string, obj>) =  
                        
        // Execute only parsing step of the compiler
        let options = new Dictionary<string, obj>(opts :?> Dictionary<string, obj>)
        options.Add(CompilerOptions.ParseOnly, ())
        let mutable parsedModule = this.Compiler.Compile(info, options) :?> IKernelModule  

        if parsedModule = null then
            None
        else
            // Valid kernel
            // Extract running mode and fallback
            let mode = parsedModule.Kernel.Meta.KernelMeta.Get<RunningModeAttribute>().RunningMode
            let fallback = parsedModule.Kernel.Meta.KernelMeta.Get<MultithreadFallbackAttribute>().Fallback

            // Check if can run
            if mode = RunningMode.OpenCL && not (this.IsOpenCLAvailable()) && not (fallback) then
                raise (KernelSchedulingException("No OpenCL device is available in the system. Please check the functionality of your devices and that OpenCL is properly installed in the system"))
   
            let useOpenCL = this.IsOpenCLAvailable() && mode = RunningMode.OpenCL

            // OpenCL running mode
            if useOpenCL then
                // Check if platform and device indexes are valid  
                let device = parsedModule.Kernel.Meta.KernelMeta.Get<DeviceAttribute>()
                if OpenCLPlatform.Platforms.Count <= device.Platform || (OpenCLPlatform.Platforms.[device.Platform]).Devices.Count <= device.Device then
                    raise (new KernelCompilationException("The platform and device indexes specified for the kernel " + parsedModule.Kernel.Signature.Name + " are invalid"))
  
                // Check if there is a kernel cached to avoid running through the entire compiler pipeline
                match this.GlobalCache.TryFindCompatibleOpenCLCachedKernel(parsedModule.Kernel.ID, parsedModule.Kernel.Meta) with
                | Some(cachedKernel) ->          
                    // A code has been generated (the cached version is not produced by multithread execution)
                    cachedKernel.Kernel.CloneTo(parsedModule.Kernel)  
                                               
                    // Compiler backend and store
                    let devData, ckData = this.OpenCLBackendAndStore(cachedKernel, device.Platform, device.Device, opts) 
                    Some(KernelCreationResult(parsedModule.CallArgs.Value, devData, new RuntimeKernel(parsedModule.Kernel, cachedKernel.OpenCLCode), ckData))                    
                // Else execute entire compiler pipeline
                | None ->
                    parsedModule <- this.Compiler.Compile(parsedModule, opts) :?> IKernelModule
                    
                    // Cache the kernel     
                    if not (this.GlobalCache.OpenCLKernels.ContainsKey(parsedModule.Kernel.ID)) then
                        // First of these kernels
                        this.GlobalCache.OpenCLKernels.Add(parsedModule.Kernel.ID, new List<ReadOnlyMetaCollection * RuntimeKernel>())
                        
                    // Compiler backend and store
                    let runtimeKernel = new RuntimeKernel(parsedModule.Kernel, parsedModule.Code.Value)
                    this.GlobalCache.OpenCLKernels.[parsedModule.Kernel.ID].Add(parsedModule.Kernel.Meta, runtimeKernel)
                    let devData, ckData = this.OpenCLBackendAndStore(runtimeKernel, device.Platform, device.Device, opts)                     
                    Some(KernelCreationResult(parsedModule.CallArgs.Value, devData, runtimeKernel, ckData)) 
            // Multithread running mode
            else
                None
        
    interface IDisposable with
        member this.Dispose() =
            (this.GlobalCache :> IDisposable).Dispose()