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
open System.Runtime.CompilerServices

[<AllowNullLiteral>]
type KernelCreationManager(schedulingEngine: ISchedulingEngine option) = 
    let locker = new Object()
     
    let openCLDevices =
        let devs = new List<OpenCLDevice>()
        for p in OpenCLPlatform.Platforms do
            for d in p.Devices do
                devs.Add(d)
        devs
    //let sharedComputeContext = new OpenCLContext(openCLDevices, new OpenCLContextPropertyList(openCLDevices.[0].Platform), null, System.IntPtr.Zero) 

    // The metric used to select the best devices for a kernel
    member val SchedulingEngine = schedulingEngine with get             
    member val DeviceCache = new DeviceCache() with get

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
    member private this.OpenCLBackendAndStore(kernelModule: IKernelModule,
                                              cacheEntry: RuntimeKernelCacheEntry,
                                              platformIndex, deviceIndex, opts: Map<string, obj>) =
        let mutable device = null
        let mutable compiledKernel = null
        
        //#1: Check if there is the corresponding device info have been set
        let device = 
            lock locker (fun () ->
                            if not (this.DeviceCache.Devices.ContainsKey((platformIndex, deviceIndex))) then        
                                // Get OpenCL info
                                let ps = OpenCLPlatform.Platforms
                                let platform = OpenCLPlatform.Platforms.[platformIndex]
                                let dev = platform.Devices.[deviceIndex]   
                                let devs = new System.Collections.Generic.List<OpenCLDevice>();
                                devs.Add(dev)
                                
                                //
                                // Create context and queue
                                let computeContext = new OpenCLContext(devs, new OpenCLContextPropertyList(devs.[0].Platform), null, System.IntPtr.Zero) 
                                let computeQueue = new OpenCLCommandQueue(computeContext, dev, OpenCLCommandQueueProperties.None) 
                                // Add device to the global devices cache
                                this.DeviceCache.Devices.Add((platformIndex, deviceIndex), new RuntimeDevice(dev, computeContext, computeQueue))
                            this.DeviceCache.Devices.[platformIndex, deviceIndex])
                        
        //#2: Evaluate dynamic defines      
        let dynDefineValues = new Dictionary<string, string>()   
        let dynDefValuesChain = ref ""             
        for item in kernelModule.ConstantDefines do
            let tv, _, evaluator = item.Value
            let dynDefVal = evaluator.GetType().GetMethod("Invoke").Invoke(evaluator, 
                                if tv.IsSome then 
                                    let thisObj = LeafExpressionConverter.EvaluateQuotation(kernelModule.InstanceExpr.Value)
                                    [| thisObj |]
                                else
                                    [|()|])
            dynDefValuesChain := !dynDefValuesChain + dynDefVal.ToString()
            dynDefineValues.Add(item.Key, dynDefVal.ToString())

        //#3: Check if the device target code has been already generated 
        //Considering both the platform/device index and the values of dynamic defines        
        let compiledKernel =
            lock locker (fun () ->
                            if cacheEntry.Instances.ContainsKey(platformIndex, deviceIndex, !dynDefValuesChain) |> not then
                                let computeProgram = new OpenCLProgram(device.Context, kernelModule.Code.Value)
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
                                // Add kernel implementation to the list of implementations for the given kernel
                                let compiledKernel = new RuntimeCompiledKernel(computeProgram, kernelModule.Kernel.Name, dynDefineValues)
                                cacheEntry.Instances.Add((platformIndex, deviceIndex, !dynDefValuesChain), compiledKernel)
                                compiledKernel
                            else
                                cacheEntry.Instances.[platformIndex, deviceIndex, !dynDefValuesChain])
          
        //#4: Return device, kernel and compiled kernel
        (device, compiledKernel)
        
    member this.Process(node: IKFGKernelNode,
                        opts: Map<string, obj>) =  
        // Extract running mode and fallback
        let mode = 
            if opts.ContainsKey(RuntimeOptions.RunningMode) && 
               (opts.[RuntimeOptions.RunningMode] :?> RunningMode) <> RunningMode.OpenCL then
                opts.[RuntimeOptions.RunningMode] :?> RunningMode
            else
                node.Module.Kernel.Meta.KernelMeta.Get<RunningModeAttribute>().RunningMode
        let fallback = node.Module.Kernel.Meta.KernelMeta.Get<MultithreadFallbackAttribute>().Fallback

        // Check if can run
        if mode = RunningMode.OpenCL && not (this.IsOpenCLAvailable()) && not (fallback) then
            raise (KernelSchedulingException("No OpenCL device is available in the system. Please check the functionality of your devices and that OpenCL is properly installed in the system"))
        let useOpenCL = this.IsOpenCLAvailable() && mode = RunningMode.OpenCL

        // OpenCL running mode
        if useOpenCL then            
            // Check if platform and device indexes are valid  
            let device = node.Module.Kernel.Meta.KernelMeta.Get<DeviceAttribute>()
            //Console.WriteLine("Running kernel on platform " + device.Platform.ToString() + ", device " + device.Device.ToString())
            if OpenCLPlatform.Platforms.Count <= device.Platform || (OpenCLPlatform.Platforms.[device.Platform]).Devices.Count <= device.Device then
                raise (new KernelCompilationException("The platform and device indexes specified for the kernel " + node.Module.Kernel.Name + " are invalid"))
                          
            // Compiler backend and store
            let devData, ckData = this.OpenCLBackendAndStore(node.Module, node.CacheEntry :?> RuntimeKernelCacheEntry, device.Platform, device.Device, opts)                     
            Some(OpenCLKernel(OpenCLKernelCreationResult(node.Module, devData, ckData)))
        // Multithread running mode
        else
            if mode <> RunningMode.Multithread then
                System.Diagnostics.Trace.WriteLine("Fallbacking to multithread execution")
            // Cache the kernel     
            Some(MultithreadKernel(MultithreadKernelCreationResult(node.Module)))
                
        
    interface IDisposable with
        member this.Dispose() =
            (this.DeviceCache :> IDisposable).Dispose()