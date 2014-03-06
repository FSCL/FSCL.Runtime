namespace FSCL.Runtime

open System
open Cloo
open FSCL.Compiler
open System.Collections.Generic
open System.Reflection

[<AllowNullLiteral>]
type RuntimeDeviceData(device: ComputeDevice, context: ComputeContext, queue: ComputeCommandQueue) =
    member val Device = device with get
    member val Context = context with get
    member val Queue = queue with get

    interface IDisposable with
        member this.Dispose() =
            this.Queue.Dispose()
            this.Context.Dispose()
   
[<AllowNullLiteral>] 
type RuntimeCompiledKernelData(program, kernel) =
    member val Program:ComputeProgram = program with get  
    member val Kernel:ComputeKernel = kernel with get  

    interface IDisposable with
        member this.Dispose() =
            this.Kernel.Dispose()
            this.Program.Dispose()

[<AllowNullLiteral>]
type RuntimeKernelData(info, mtv, code) =
    member val Info:KernelInfo = info with get 
    member val MultithreadVersion:MethodInfo option = mtv with get, set
    member val OpenCLCode:String option = code with get, set
    // List of devices and kernel instances potentially executing the kernel
    member val Instances:Dictionary<int * int, RuntimeCompiledKernelData> = new Dictionary<int * int, RuntimeCompiledKernelData>() with get 
    
    interface IDisposable with
        member this.Dispose() =
            for item in this.Instances do
                (item.Value :> IDisposable).Dispose()
    
type RuntimeCache() =
    member val Kernels:Dictionary<obj, RuntimeKernelData> = Dictionary<obj, RuntimeKernelData>() with get
    member val Devices:Dictionary<int * int, RuntimeDeviceData> = new Dictionary<int * int, RuntimeDeviceData>() with get
    
    interface IDisposable with
        member this.Dispose() =
            for item in this.Kernels do
                (item.Value :> IDisposable).Dispose()
            for item in this.Devices do
                (item.Value :> IDisposable).Dispose()