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
open FSCL.Runtime.Scheduling
open System
open System.Reflection
open FSCL.Runtime.CompilerSteps
open Microsoft.FSharp.Linq.RuntimeHelpers

[<AllowNullLiteral>]
type KernelCreationManager(compiler: Compiler, 
                           schedulingEngine: ISchedulingEngine option) =  

    // The data structure caching devices texts) and (queues, concompiled kernels 
    member val private GlobalCache = new RuntimeCache(compiler.IsInvariantToMetaCollection, fun(a,b) -> true) with get
    // The metric used to select the best devices for a kernel
    member val SchedulingEngine = schedulingEngine with get             
    // The FSCL compiler   
    // Add the additional steps for the compiler to skip kernel recompilation and produce separated source codes for kernels
    member val Compiler = new Compiler(
                            new PipelineConfiguration(
                                compiler.Configuration.LoadDefaultSteps, 
                                Array.append (compiler.Configuration.Sources) [| new SourceConfiguration(AssemblySource(typeof<KernelModuleParser>.Assembly)) |]))
          with get
         
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
                                              dynamicDefines: IReadOnlyDictionary<string, Expr>,
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
        let dynDefineValues = new Dictionary<string, string>()   
        let mutable dynDefValuesChain = ""             
        for item in dynamicDefines do
            let dynDefVal = item.Value.ToString()
            dynDefValuesChain <- dynDefValuesChain + dynDefVal
            dynDefineValues.Add(item.Key, dynDefVal)

        //#3: Check if the device target code has been already generated 
        //Considering both the platform/device index and the values of dynamic defines        
        let compiledKernel =
            if runtimeKernel.Instances.ContainsKey(platformIndex, deviceIndex, dynDefValuesChain) then
                let computeProgram = new OpenCLProgram(device.Context, runtimeKernel.OpenCLCode)
                // Generate define options
                let mutable definesOption = ""
                for d in dynDefineValues do
                    definesOption <- definesOption + "-D " + d.Key + "=" + d.Value + " "
                try
                    computeProgram.Build([| device.Device |], definesOption, null, System.IntPtr.Zero)
                with
                | ex -> 
                    let log = computeProgram.GetBuildLog(device.Device)
                    raise (new KernelCompilationException("Device code generation failed: " + log))
                // Create kernel
                let computeKernel = computeProgram.CreateKernel(runtimeKernel.Kernel.ParsedSignature.Name)
                // Add kernel implementation to the list of implementations for the given kernel
                let compiledKernel = new RuntimeCompiledKernel(computeProgram, computeKernel, dynDefineValues)
                runtimeKernel.Instances.Add((platformIndex, deviceIndex, dynDefValuesChain), compiledKernel)
                compiledKernel
            else
                runtimeKernel.Instances.[platformIndex, deviceIndex, dynDefValuesChain]
          
        //#4: Return device, kernel and compiled kernel
        (device, compiledKernel)
        
    member this.Process(info: Expr,
                        opts: IReadOnlyDictionary<string, obj>) =  
                        
        // Execute only parsing step of the compiler
        let options = new Dictionary<string, obj>(opts :?> Dictionary<string, obj>)
        options.Add(CompilerOptions.ParseOnly, ())
        let mutable parsedModule = this.Compiler.Compile(info, options) :?> IKernelModule  

        if parsedModule = null then
            // Regular function, we create a regular function result 
            match info with
            | Patterns.Call(o, mi, args) ->
                Some(ComputationCreationResult.RegularFunction(o, mi, args))
            | Patterns.Application(expr, v) ->
                // Extract a call from application
                let rec extractMethodInfo (e:Expr, args: Expr list) =
                    match e with
                    | Patterns.Application(expr, a) ->
                        // Check if a if a NewTuple (curried args)
                        match a with
                        | Patterns.NewTuple(elements) ->
                            extractMethodInfo (expr, args @ elements)
                        | _ ->
                            extractMethodInfo (expr, args @ [ a ])                            
                    | Patterns.Value(v) ->                        
                        let func = FSharpValue.GetTupleField(v, 0)
                        let argsTypes = args |> List.map(fun i -> i.Type) |> List.toArray
                        ComputationCreationResult.RegularFunction(Some(<@ func @> :> Expr), func.GetType().GetMethod("Invoke", argsTypes), args)    
                    | _ ->
                        raise (new KernelCompilationException("The expression cannot be executed: " + info.ToString()))
                Some(extractMethodInfo(expr, [v]))
            | _ ->
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
                    raise (new KernelCompilationException("The platform and device indexes specified for the kernel " + parsedModule.Kernel.ParsedSignature.Name + " are invalid"))
  
                // Check if there is a kernel cached to avoid running through the entire compiler pipeline
                match this.GlobalCache.TryFindCompatibleOpenCLCachedKernel(parsedModule.Kernel.ID, parsedModule.Kernel.Meta) with
                | Some(cachedKernel) ->          
                    // A code has been generated (the cached version is not produced by multithread execution)
                    cachedKernel.Kernel.CloneTo(parsedModule.Kernel)  
                                               
                    // Compiler backend and store
                    let devData, ckData = this.OpenCLBackendAndStore(cachedKernel, cachedKernel.DynamicDefines, device.Platform, device.Device, opts) 
                    Some(OpenCLKernel(OpenCLKernelCreationResult(parsedModule.CallArgs.Value, devData, new RuntimeKernel(parsedModule.Kernel, cachedKernel.OpenCLCode, cachedKernel.DynamicDefines), ckData)))                    
                // Else execute entire compiler pipeline
                | None ->
                    parsedModule <- this.Compiler.Compile(parsedModule, opts) :?> IKernelModule

                    // Cache the kernel     
                    if not (this.GlobalCache.OpenCLKernels.ContainsKey(parsedModule.Kernel.ID)) then
                        // First of these kernels
                        this.GlobalCache.OpenCLKernels.Add(parsedModule.Kernel.ID, new List<ReadOnlyMetaCollection * RuntimeKernel>())
                        
                    // Compiler backend and store
                    let runtimeKernel = new RuntimeKernel(parsedModule.Kernel, parsedModule.Code.Value, parsedModule.DynamicConstantDefines)
                    this.GlobalCache.OpenCLKernels.[parsedModule.Kernel.ID].Add(parsedModule.Kernel.Meta, runtimeKernel)
                    let devData, ckData = this.OpenCLBackendAndStore(runtimeKernel, parsedModule.DynamicConstantDefines, device.Platform, device.Device, opts)                     
                    Some(OpenCLKernel(OpenCLKernelCreationResult(parsedModule.CallArgs.Value, devData, runtimeKernel, ckData)))
            // Multithread running mode
            else
                if mode <> RunningMode.Multithread then
                    System.Diagnostics.Trace.WriteLine("Fallbacking to multithread execution")
                // Cache the kernel     
                if not (this.GlobalCache.MultithreadKernels.ContainsKey(parsedModule.Kernel.ID)) then
                    // First of these kernels
                    this.GlobalCache.MultithreadKernels.Add(parsedModule.Kernel.ID, new List<ReadOnlyMetaCollection * MethodInfo>())
                this.GlobalCache.MultithreadKernels.[parsedModule.Kernel.ID].Add(parsedModule.Kernel.Meta, parsedModule.Kernel.ParsedSignature)
                   
                Some(MultithreadKernel(MultithreadKernelCreationResult(parsedModule.CallArgs.Value, parsedModule.Kernel)))
                
        
    interface IDisposable with
        member this.Dispose() =
            (this.GlobalCache :> IDisposable).Dispose()