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
type KernelCreationManager(cache: RuntimeCache, schedulingEngine: ISchedulingEngine option) =  
    // The metric used to select the best devices for a kernel
    member val SchedulingEngine = schedulingEngine with get             
    member val GlobalCache = cache with get

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
                                              dynamicDefines: IReadOnlyDictionary<string, Var option * Expr option * obj>,
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
            let tv, _, evaluator = item.Value
            let dynDefVal = evaluator.GetType().GetMethod("Invoke").Invoke(evaluator, 
                                if tv.IsSome then 
                                    let thisObj = LeafExpressionConverter.EvaluateQuotation(runtimeKernel.KernelInfo.InstanceExpr.Value)
                                    [| thisObj |]
                                else
                                    [|()|])
            dynDefValuesChain <- dynDefValuesChain + dynDefVal.ToString()
            dynDefineValues.Add(item.Key, dynDefVal.ToString())

        //#3: Check if the device target code has been already generated 
        //Considering both the platform/device index and the values of dynamic defines        
        let compiledKernel =
            if runtimeKernel.Instances.ContainsKey(platformIndex, deviceIndex, dynDefValuesChain) |> not then
                let computeProgram = new OpenCLProgram(device.Context, runtimeKernel.ModuleCode)
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
                let computeKernel = computeProgram.CreateKernel(runtimeKernel.KernelInfo.ParsedSignature.Name)
                // Add kernel implementation to the list of implementations for the given kernel
                let compiledKernel = new RuntimeCompiledKernel(computeProgram, computeKernel, dynDefineValues)
                runtimeKernel.Instances.Add((platformIndex, deviceIndex, dynDefValuesChain), compiledKernel)
                compiledKernel
            else
                runtimeKernel.Instances.[platformIndex, deviceIndex, dynDefValuesChain]
          
        //#4: Return device, kernel and compiled kernel
        (device, compiledKernel)
        
    member this.Process(node: IKFGKernelNode,
                        opts: IReadOnlyDictionary<string, obj>) =  
        // Extract running mode and fallback
        let mode = node.Module.Kernel.Meta.KernelMeta.Get<RunningModeAttribute>().RunningMode
        let fallback = node.Module.Kernel.Meta.KernelMeta.Get<MultithreadFallbackAttribute>().Fallback

        // Check if can run
        if mode = RunningMode.OpenCL && not (this.IsOpenCLAvailable()) && not (fallback) then
            raise (KernelSchedulingException("No OpenCL device is available in the system. Please check the functionality of your devices and that OpenCL is properly installed in the system"))
        let useOpenCL = this.IsOpenCLAvailable() && mode = RunningMode.OpenCL

        // OpenCL running mode
        if useOpenCL then
            // Check if cached kernel
            
            // Check if platform and device indexes are valid  
            let device = node.Module.Kernel.Meta.KernelMeta.Get<DeviceAttribute>()
            if OpenCLPlatform.Platforms.Count <= device.Platform || (OpenCLPlatform.Platforms.[device.Platform]).Devices.Count <= device.Device then
                raise (new KernelCompilationException("The platform and device indexes specified for the kernel " + node.Module.Kernel.ParsedSignature.Name + " are invalid"))
  
            // Check if there is a binary kernel cached to avoid compile
            if not (this.GlobalCache.OpenCLKernels.ContainsKey(node.Module.Kernel.ID)) then
                // First of these kernels
                this.GlobalCache.OpenCLKernels.Add(node.Module.Kernel.ID, new List<ReadOnlyMetaCollection * RuntimeKernel>())
                        
            // Compiler backend and store
            let runtimeKernel = new RuntimeKernel(node.Module.Kernel, node.Module.Code.Value, node.Module.DynamicConstantDefines)
            this.GlobalCache.OpenCLKernels.[node.Module.Kernel.ID].Add(node.Module.Kernel.Meta, runtimeKernel)
            let devData, ckData = this.OpenCLBackendAndStore(runtimeKernel, node.Module.DynamicConstantDefines, device.Platform, device.Device, opts)                     
            Some(OpenCLKernel(OpenCLKernelCreationResult(devData, runtimeKernel, ckData)))
        // Multithread running mode
        else
            if mode <> RunningMode.Multithread then
                System.Diagnostics.Trace.WriteLine("Fallbacking to multithread execution")
            // Cache the kernel     
            Some(MultithreadKernel(MultithreadKernelCreationResult(node.Module.Kernel)))
                
        
    interface IDisposable with
        member this.Dispose() =
            (this.GlobalCache :> IDisposable).Dispose()