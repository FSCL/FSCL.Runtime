namespace FSCL.Runtime.Managers

open Cloo
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open System.Reflection
open System.Collections.Generic
open FSCL.Compiler
open FSCL.Compiler.Configuration
open FSCL.Runtime
open FSCL.Runtime.Metric
open FSCL.Runtime.Language
open System
open System.Reflection
open System.Collections.ObjectModel
open FSCL.Runtime.CacheInspection
open System.Collections.ObjectModel

type internal KernelParameterTable = Dictionary<String, KernelParameterInfo>
    
type internal KernelCreationManager(compiler: Compiler, 
                                    metric: SchedulingMetric option) =  
    // The data structure caching devices texts) and (queues, concompiled kernels 
    member val private GlobalCache = new RuntimeCache() with get
    // The metric used to select the best devices for a kernel
    member val SchedulingMetric = metric with get             
    // The FSCL compiler   
    // Add the additional steps for the compiler to skip kernel recompilation and produce separated source codes for kernels
    member val Compiler = new Compiler()
                            //new PipelineConfiguration(
                              //  compiler.Configuration.LoadDefaultSteps, 
                                //compiler.Configuration.Sources @ [new SourceConfiguration(AssemblySource(typeof<CacheInspectionStep>.Assembly)) ]))
          with get
    // The multithread adaptor (compiler) to run kernels using multithreading
    member val private KernelAdaptor = new MultithreadKernelAdaptor() with get
         
    // Utility function to store kernels found all around the assembly. Called by the constructor
    member private this.StoreKernel(node: KernelInfo,
                                    multithread: bool, 
                                    platformIndex, deviceIndex) =
        let mutable device = null
        let mutable kernel = null
        let mutable compiledKernel = null

        //#1: Check if there is the corresponding device info have been set
        if not (this.GlobalCache.Devices.ContainsKey((platformIndex, deviceIndex))) then        
            // Get OpenCL info
            let platform = ComputePlatform.Platforms.[platformIndex]
            let dev = platform.Devices.[deviceIndex]   
            let devs = new System.Collections.Generic.List<ComputeDevice>();
            devs.Add(dev)
            // Create context and queue
            let contextProperties = new ComputeContextPropertyList(platform)
            let computeContext = new ComputeContext(devs, contextProperties, null, System.IntPtr.Zero) 
            let computeQueue = new ComputeCommandQueue(computeContext, dev, ComputeCommandQueueFlags.None) 
            // Add device to the global devices cache
            this.GlobalCache.Devices.Add((platformIndex, deviceIndex), new RuntimeDeviceData(dev, computeContext, computeQueue))
        device <- this.GlobalCache.Devices.[platformIndex, deviceIndex]

        //#2: Check if the kernel has been already cached
        if not multithread then
            if not (this.GlobalCache.Kernels.ContainsKey(node.ID)) then
                this.GlobalCache.Kernels.Add(node.ID, new RuntimeKernelData(node, None, Some(node.Code)))
            else if this.GlobalCache.Kernels.[node.ID].OpenCLCode.IsNone then
                //This happens if previously the kernel has been run in Multithread mode
                // We thus have the entry in the cache but no opencl code produces
                this.GlobalCache.Kernels.[node.ID].OpenCLCode <- Some(node.Code)
        else
            if not (this.GlobalCache.Kernels.ContainsKey(node.ID)) then
                // Create multithread version
                let mtKernel = this.KernelAdaptor.CreateMultithreadKernel(node.Signature)
                this.GlobalCache.Kernels.Add(node.ID, new RuntimeKernelData(null, Some(mtKernel), None))
            else if this.GlobalCache.Kernels.[node.ID].MultithreadVersion.IsNone then
                // Create multithread version
                let mtKernel = this.KernelAdaptor.CreateMultithreadKernel(node.Signature)
                this.GlobalCache.Kernels.[node.ID].MultithreadVersion <- Some(mtKernel)
        kernel <- this.GlobalCache.Kernels.[node.ID]
                
        //#3: If OpenCL mode, check if the device target code has been already generated    
        if not multithread then        
            if not (this.GlobalCache.Kernels.[node.ID].Instances.ContainsKey(platformIndex, deviceIndex)) then
                let computeProgram = new ComputeProgram(device.Context, kernel.Info.CustomInfo.["SEPARATED_CODE"] :?> string)
                try
                    computeProgram.Build([| device.Device |], "", null, System.IntPtr.Zero)
                with
                | ex -> 
                    let log = computeProgram.GetBuildLog(device.Device)
                    raise (new KernelCompilationException("Device code generation failed: " + log))
                // Create kernel
                let computeKernel = computeProgram.CreateKernel(kernel.Info.Signature.Name)
                // Add kernel implementation to the list of implementations for the given kernel
                let compiledKernel = new RuntimeCompiledKernelData(computeProgram, computeKernel)
                kernel.Instances.Add((platformIndex, deviceIndex), compiledKernel)
            compiledKernel <- kernel.Instances.[platformIndex, deviceIndex]
          
        //#4: Return device, kernel and compiled kernel
        (device, kernel, compiledKernel)
       
    member private this.AnalyzeAndStoreKernel(kernel:KernelInfo, 
                                              mode: KernelRunningMode, 
                                              fallback: bool) =
        // Check if OpenCL enabled platform (at least one opencl platform with one device)
        let kernelModule = ref None
        if KernelManagerTools.IsOpenCLAvailable() && mode = KernelRunningMode.OpenCL then
            // Check if a particular device is specified by the user via KernelAttribute
            let device = kernel.GetMetadata<DeviceAttribute>()
            if device.IsSome then
                // Check if platform and device indexes are valid  
                if ComputePlatform.Platforms.Count <= device.Value.Platform || (ComputePlatform.Platforms.[device.Value.Platform]).Devices.Count <= device.Value.Device then
                    raise (new KernelCompilationException("The platform and device indexes specified for the kernel " + kernel.Signature.Name + " are invalid"))
                this.StoreKernel(kernel, false, device.Value.Platform, device.Value.Device)
            // No statically determined device: build kernel for all the possible devices
            else
                // The heart: find best device using a metric (by now fixed assignment)
                this.StoreKernel(kernel, false, 0, 0)
        // If opencl not available or multithread/sequential execution required
        else 
            if mode = KernelRunningMode.OpenCL && (not fallback) then
                raise (KernelSchedulingException("No OpenCL device is available in the system. Please check the functionality of your devices and that OpenCL is properly installed in the system"))
            if mode = KernelRunningMode.OpenCL && fallback then
                Console.WriteLine("Warning: kernel " + kernel.Signature.Name + " is fallbacking to multithread execution")
            this.StoreKernel(kernel, true, 0, 0)
        
    member this.Process(input: Object, 
                        mode: KernelRunningMode, 
                        fallback: bool,
                        opts: ReadOnlyDictionary<string, obj>) =  
        let multithread = (mode <> KernelRunningMode.OpenCL)    

        // Copile the input passing the global cache to skip kernels already compiled
        //let kernelModule, code = this.Compiler.Compile((input, this.GlobalCache))  :?> (KernelModule * string)        
        // Collect runtime information (device resources, kernel OpenCL program, ecc) for each method info
        let runtimeInfo = new Dictionary<FunctionInfoID, RuntimeDeviceData * RuntimeKernelData * RuntimeCompiledKernelData>();
        //for k in kernelModule.GetKernels() do
          //  runtimeInfo.Add(k.Info.ID, this.AnalyzeAndStoreKernel(k.Info, mode, fallback))
        //(runtimeInfo, kernelModule.FlowGraph)
        0

    interface IDisposable with
        member this.Dispose() =
            (this.GlobalCache :> IDisposable).Dispose()