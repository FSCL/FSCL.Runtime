﻿namespace FSCL.Runtime

open Cloo
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open System.Reflection
open System.Collections.Generic
open FSCL.Compiler
open FSCL.Runtime.Metric
open System

type internal KernelParameterTable = Dictionary<String, KernelParameterInfo>

type internal KernelManager(compiler: Compiler, metric: SchedulingMetric option) =       
    // The data structure caching devices (queues, contexts) and compiled kernels 
    member val GlobalRuntimeCache = new RuntimeCache() with get
    // The metric used to select the best devices for a kernel
    member val SchedulingMetric = metric with get             
    // The FSCL compiler   
    member val Compiler = compiler with get
    // The multithread adaptor (compiler) to run kernels using multithreading
    member val KernelAdaptor = new MultithreadKernelAdaptor() with get
         
    member this.FindMatchingKernel(kernel: MethodInfo) =
        let mutable result = None
        let mutable mindex = 0
        let mutable kindex = 0
        while result.IsNone && mindex < this.GlobalRuntimeCache.Modules.Count do
            kindex <- 0
            let kModule = this.GlobalRuntimeCache.Modules.[mindex]
            while result.IsNone && kindex < kModule.Kernels.Count do
                if kModule.Kernels.[kindex].Info.ID = kernel then
                    result <- Some(kModule, kindex)
                else
                    kindex <- kindex + 1
            mindex <- mindex + 1
        result
            (*
    member this.FindMatchingCompiledKernel(kernel: MethodInfo, pars: Type array, multithread: bool) =
        let discoveryResult = this.FindMatchingKernel(kernel)
        let mutable result = None
        match discoveryResult with
        | Some(kernelModule, kernelData) ->
            if multithread then
                result <- kernelData.MultithreadVersion
            else
                let mutable index = 0
                while result.IsNone && index < kernelData.Instances.Count do
                    if (kernelModule.Value.Kernels.[index].Info.Source = kernel) then
                        result <- Some(kernelModule.Value.Kernels.[index])
                    else
                        index <- index + 1
        result*)

    // Utility function to store kernels found all around the assembly. Called by the constructor
    member private this.StoreKernel(globalData:FSCLGlobalData, kernel:MethodInfo, multithread: bool, platformIndex, deviceIndex) =    
        // Check if kernel already stored
        // If not, compile the kernel using FSCL
        let mutable matchKernelModule = this.FindMatchingKernelModule(kernel)
        if matchKernelModule.IsNone then
            if not multithread then
                // Convert kernel         
                let (kernelModule, conversionData) = this.Compiler.Compile(kernel) :?> (KernelModule * string)
                // Create module                
                let modul = new FSCLModuleData(kernel, conversionData)                               
                // Store kernel instances
                for k in kernelModule.KernelsIDs do
                    modul.Kernels.Add(new FSCLKernelData(k))                
                // Store kernel module
                globalData.Modules.Add(modul)
                matchKernelModule <- Some(modul)
            else
                // Create multithread version
                let mtKernel = this.KernelAdaptor.CreateMultithreadKernel(kernel)
                // Create module           
                let modul = new FSCLModuleData(kernel, "")
                // Store multithread version
                modul.Multithread <- Some(new FSCLKernelData(new KernelInfo(mtKernel, Expr.Value(0))))
                // Store kernel module
                globalData.Modules.Add(modul)
                matchKernelModule <- Some(modul)
                
        if not multithread then
            // Check if setup for requested device already created
            // If not, create device, context and queue
            let platform = ComputePlatform.Platforms.[platformIndex]
            let device = platform.Devices.[deviceIndex]   
            let devices = new System.Collections.Generic.List<ComputeDevice>();
            devices.Add(device)

            let deviceIndex = ref (Seq.tryFindIndex (fun (dev:FSCLDeviceData) -> dev.Device.Handle = device.Handle) globalData.Devices)
            if (!deviceIndex).IsNone then
                // Store device, context and queue (one per device)
                let contextProperties = new ComputeContextPropertyList(platform)
                let computeContext = new ComputeContext(devices, contextProperties, null, System.IntPtr.Zero) 
                let computeQueue = new ComputeCommandQueue(computeContext, device, ComputeCommandQueueFlags.None) 
                // Add device to the list of global devices
                deviceIndex := Some(globalData.Devices.Count)
                let deviceData = new FSCLDeviceData(device, computeContext, computeQueue)
                globalData.Devices.Add(deviceData)
                deviceIndex := Some(globalData.Devices.Count - 1)
           
            // Check if kernel has already been built for the device specified. If not, build it
            if (Seq.tryFind (fun (k:FSCLCompiledKernelData) -> k.DeviceIndex = !deviceIndex) (matchKernelModule.Value.Kernels.[0].Instances)).IsNone then
                let computeProgram = new ComputeProgram(globalData.Devices.[(!deviceIndex).Value].Context, matchKernelModule.Value.OpenCLSourceCode)
                try
                    computeProgram.Build(devices, "", null, System.IntPtr.Zero)
                with
                | ex -> 
                    let log = computeProgram.GetBuildLog(device)
                    raise (new KernelDefinitionException("Kernel build fail: " + log))
        
                // Create kernels for each non generic version
                for ki in 0 .. matchKernelModule.Value.Kernels.Count - 1 do
                    let computeKernel = computeProgram.CreateKernel(matchKernelModule.Value.Kernels.[ki].Info.Signature.Name)

                    // Add kernel implementation to the list of implementations for the given kernel
                    let compiledKernel = new FSCLCompiledKernelData(computeProgram, computeKernel, !deviceIndex)
                    matchKernelModule.Value.Kernels.[ki].Instances.Add(compiledKernel)
        // Otherwise
        else
            // Check if multithread kernel has already been built. If not, build it
            if matchKernelModule.Value.Multithread.IsNone then
                let mtKernel = this.KernelAdaptor.CreateMultithreadKernel(kernel)
                matchKernelModule.Value.Multithread <- Some(new FSCLKernelData(new KernelInfo(mtKernel, Expr.Value(0))))
        // Return module
        matchKernelModule.Value
       
    member private this.AnalyzeKernel(kernel:RuntimeKernelData, mode: KernelRunningMode, fallback: bool) =
        // Check if OpenCL enabled platform (at least one opencl platform with one device)
        let kernelModule = ref None
        if KernelManagerTools.IsOpenCLAvailable() && mode = KernelRunningMode.OpenCL then
            // For each kernel analyze, create device, translate it into CL and compile
            let mutable platformIndex = 0
            let mutable deviceIndex = 0

            // Check if a particular device is specified by the user via KernelAttribute
            if kernel.Info.TargetDevice.IsSome then
                // Check if platform and device indexes are valid
                platformIndex <- kernel.Info.TargetDevice.Platform
                deviceIndex <- kernel.Info.TargetDevice.Device      
                if ComputePlatform.Platforms.Count <= platformIndex || (ComputePlatform.Platforms.[platformIndex]).Devices.Count <= deviceIndex then
                    raise (new KernelDeviceSelectionException("The platform and device indexes specified for the kernel " + kernel.Info.Name + " are invalid"))
                
                kernelModule := Some(this.StoreKernel(this.GlobalDataStorage, kernel, false, platformIndex, deviceIndex))
            // No statically determined device: build kernel for all the possible devices
            else
                // THE HEART: find best device using a metric (by now fixed assignment)
                platformIndex <- 0
                deviceIndex <- 0    
                
                for platform in 0 .. 1 do
                    for device in 0 .. 1 do
                        kernelModule := Some(this.StoreKernel(this.GlobalDataStorage, kernel, false, platformIndex, deviceIndex))
        // If opencl not available or multithread/sequential execution required
        else 
            if mode = KernelRunningMode.OpenCL && (not fallback) then
                raise (KernelSchedulingException("No OpenCL device is available in the system. Please check the functionality of your devices and that OpenCL is properly installed in the system"))
            if mode = KernelRunningMode.OpenCL && fallback then
                Console.WriteLine("Warning: kernel " + kernel.Name + " is fallbacking to multithread execution")
            
            kernelModule := Some(this.StoreKernel(this.GlobalDataStorage, kernel, true, 0, 0))
          
        // Return the module
        kernelModule.Value.Value
        
    member this.BuildRuntimeCallGraph(cg: ModuleCallGraph, 
                                      mode: KernelRunningMode, 
                                      fallback: bool) =  
        let multithread = (mode <> KernelRunningMode.OpenCL)       
        for id in cg.KernelIDs do
            let matchingKernel = this.FindMatchingKernel(id)
            match matchingKernel with
            | Some(moduleData, kernelIndex) ->
                this.AnalyzeKernel(moduleData.Kernels.[kernelIndex], mode, fallback)
            | None ->
                this.StoreKernel(cg.GetKernel(id), mode, fallback)

        null
