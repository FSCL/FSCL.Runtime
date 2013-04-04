namespace FSCL.Runtime

open Cloo
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open System.Reflection
open Microsoft.FSharp.Linq.QuotationEvaluation
open System.Collections.Generic
open FSCL.Compiler
open FSCL.Runtime.Metric
open System

type internal FSCLDeviceData(device:ComputeDevice, context, queue) =
    member val Device = device with get
    member val Context = context with get
    member val Queue = queue with get
    
type internal FSCLCompiledKernelData(program, kernel, device) =
    member val Program = program with get 
    member val Kernel = kernel with get
    member val DeviceIndex = device with get

type internal FSCLKernelData(parameters) =
    member val Info:KernelInfo = parameters with get
    // List of devices and kernel instances potentially executing the kernel
    member val Instances:FSCLCompiledKernelData list = [] with get, set 

type internal FSCLModuleData(genericKernel, kernels, source) =
    member val GenericKernel:MethodInfo = genericKernel with get
    member val Kernels:FSCLKernelData list = kernels with get     
    member val KernelSource = source with get

type internal FSCLGlobalData() =
    member val Modules:FSCLModuleData list = [] with get, set
    member val Devices:FSCLDeviceData list = [] with get, set
    
type internal KernelParameterTable = Dictionary<String, KernelParameterInfo>

type internal KernelManager(compiler: FSCL.Compiler.Compiler, metric: SchedulingMetric option) =       
    // Properties   
    member val GlobalDataStorage = new FSCLGlobalData() with get
    member val SchedulingMetric = metric with get                
    member val Compiler = compiler with get
     
    member this.FindMatchingKernelModule(kernel: MethodInfo) =
        let mutable result = None
        let mutable index = 0
        while result.IsNone && index < this.GlobalDataStorage.Modules.Length do
            let kernelModule = this.GlobalDataStorage.Modules.[index]
            if kernel.IsGenericMethod && (kernelModule.GenericKernel = kernel.GetGenericMethodDefinition()) then
                result <- Some(kernelModule)
            elif (not kernel.IsGenericMethod) && (kernelModule.GenericKernel = kernel) then
                result <- Some(kernelModule)
            else
                index <- index + 1
        result
            
    member this.FindMatchingKernelInfo(kernel: MethodInfo, pars: Type array) =
        let kernelModule = this.FindMatchingKernelModule(kernel)
        let mutable result = None
        if kernelModule.IsSome then
            let mutable index = 0
            while result.IsNone && index < kernelModule.Value.Kernels.Length do
                if (kernelModule.Value.Kernels.[index].Info.Source = kernel) then
                    result <- Some(kernelModule.Value.Kernels.[index])
                else
                    index <- index + 1
        result

    // Utility function to store kernels found all around the assembly. Called by the constructor
    member private this.StoreKernel(globalData:FSCLGlobalData, kernel:MethodInfo, platformIndex, deviceIndex) =    
        // Check if kernel already stored
        // If not, compile the kernel using FSCL
        let mutable matchKernelModule = this.FindMatchingKernelModule(kernel)
        if matchKernelModule.IsNone then
            // Convert kernel         
            let (kernelModule, conversionData) = this.Compiler.Compile(kernel) :?> (KernelModule * string)

            // Store kernel module
            let mutable kernelInstances = []
            for k in kernelModule.Kernels do
                kernelInstances <- kernelInstances @ [ new FSCLKernelData(k) ]
            globalData.Modules <- globalData.Modules @ [ new FSCLModuleData(kernelModule.Source.Signature, kernelInstances, conversionData) ]
            matchKernelModule <- Some(globalData.Modules.[globalData.Modules.Length - 1])
            
        // Check if setup for requested device already created
        // If not, create device, context and queue
        let platform = ComputePlatform.Platforms.[platformIndex]
        let device = platform.Devices.[deviceIndex]   
        let devices = new System.Collections.Generic.List<ComputeDevice>();
        devices.Add(device)

        let deviceIndex = ref (List.tryFindIndex (fun (dev:FSCLDeviceData) -> dev.Device.Handle = device.Handle) globalData.Devices)
        if (!deviceIndex).IsNone then
            // Store device, context and queue (one per device)
            let contextProperties = new ComputeContextPropertyList(platform)
            let computeContext = new ComputeContext(devices, contextProperties, null, System.IntPtr.Zero) 
            let computeQueue = new ComputeCommandQueue(computeContext, device, ComputeCommandQueueFlags.None) 
            // Add device to the list of global devices
            deviceIndex := Some(globalData.Devices.Length)
            let deviceData = new FSCLDeviceData(device, computeContext, computeQueue)
            globalData.Devices <- globalData.Devices @ [ deviceData ]
            deviceIndex := Some(globalData.Devices.Length - 1)
           
        // Check if kernel has already been biult for the device specified
        // If not, build it
        if (List.tryFind (fun (k:FSCLCompiledKernelData) -> k.DeviceIndex = !deviceIndex) (matchKernelModule.Value.Kernels.[0].Instances)).IsNone then
            let computeProgram = new ComputeProgram(globalData.Devices.[(!deviceIndex).Value].Context, matchKernelModule.Value.KernelSource)
            try
                computeProgram.Build(devices, "", null, System.IntPtr.Zero)
            with
            | ex -> 
                let log = computeProgram.GetBuildLog(device)
                raise (new KernelDefinitionException("Kernel build fail: " + log))
        
            // Create kernels for each non generic version
            for ki in 0 .. matchKernelModule.Value.Kernels.Length - 1 do
                let computeKernel = computeProgram.CreateKernel(matchKernelModule.Value.Kernels.[ki].Info.Signature.Name)

                // Add kernel implementation to the list of implementations for the given kernel
                let compiledKernel = new FSCLCompiledKernelData(computeProgram, computeKernel, !deviceIndex)
                matchKernelModule.Value.Kernels.[ki].Instances <- matchKernelModule.Value.Kernels.[ki].Instances @ [ compiledKernel ]
       
    member private this.AnalyzeAndStoreKernel(kernel:MethodInfo) =
        // For each kernel analyze, create device, translate it into CL and compile
        let mutable platformIndex = 0
        let mutable deviceIndex = 0

        // Check if a particular device is specified by the user via KernelAttribute
        let kernelAttribute = List.ofSeq(kernel.GetCustomAttributes<DeviceAttribute>())
        if kernelAttribute.Length > 0 then
            // Check if platform and device indexes are valid
            platformIndex <- kernelAttribute.[0].Platform
            deviceIndex <- kernelAttribute.[0].Device      
            if ComputePlatform.Platforms.Count <= platformIndex || (ComputePlatform.Platforms.[platformIndex]).Devices.Count <= deviceIndex then
                raise (new KernelDefinitionException("The platform and device indexes specified for the kernel " + kernel.Name + " are invalid"))
                
            this.StoreKernel(this.GlobalDataStorage, kernel, platformIndex, deviceIndex)
        // No statically determined device: build kernel for all the possible devices
        else
            // The heart: find best device using a metric (by now fixed assignment)
            platformIndex <- 0
            deviceIndex <- 0    
                
            for platform in 0 .. 1 do
                for device in 0 .. 1 do
                    this.StoreKernel(this.GlobalDataStorage, kernel, platformIndex, deviceIndex)
        // Return the kernel (not the instance!)
        this.GlobalDataStorage.Modules.[this.GlobalDataStorage.Modules.Length - 1]

    member private this.AnalyzeAndStoreKernel(kernel:Expr) =
        let (c, mi, args) = KernelManagerTools.ExtractMethodInfo(kernel)
        this.AnalyzeAndStoreKernel(mi)

    member this.Add (kernel:MethodInfo) =  
        this.AnalyzeAndStoreKernel(kernel)
        
    member this.Add (kernel:Expr) =  
        this.AnalyzeAndStoreKernel(kernel)
