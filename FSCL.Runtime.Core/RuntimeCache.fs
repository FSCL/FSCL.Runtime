namespace FSCL.Runtime

open System
open Cloo
open FSCL.Compiler
open System.Collections.Generic

type RuntimeDeviceData(device:ComputeDevice, context, queue) =
    member val Device = device with get
    member val Context = context with get
    member val Queue = queue with get
    
type RuntimeCompiledKernelData(program, kernel, device) =
    member val Kernel = kernel with get
    member val DeviceIndex = device with get

type RuntimeKernelData(parameters, mtv) =
    member val Info:KernelInfo = parameters with get 
    member val MultithreadVersion:RuntimeDeviceData option = mtv with get
    // List of devices and kernel instances potentially executing the kernel
    member val Instances:List<RuntimeCompiledKernelData> = new List<RuntimeCompiledKernelData>() with get 

type RuntimeModuleData(program, kcg, code, kernels) =
    member val Program:ComputeProgram = program with get  
    member val OpenCLCode:String = code with get
    member val Kernels:List<RuntimeKernelData> = new List<RuntimeKernelData>() with get 

type RuntimeCache() =
    member val Modules:List<RuntimeModuleData> = new List<RuntimeModuleData>() with get
    member val Devices:List<RuntimeDeviceData> = new List<RuntimeDeviceData>() with get