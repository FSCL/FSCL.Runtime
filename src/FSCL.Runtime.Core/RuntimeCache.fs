namespace FSCL.Runtime

open System
open OpenCL
open FSCL.Compiler
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Quotations
open System.Collections.ObjectModel

[<AllowNullLiteral>]
type RuntimeDevice(device: OpenCLDevice, context: OpenCLContext, queue: OpenCLCommandQueue) =
    member val Device = device with get
    member val Context = context with get
    member val Queue = queue with get

    interface IDisposable with
        member this.Dispose() =
            this.Queue.Dispose()
            this.Context.Dispose()
   
[<AllowNullLiteral>] 
type RuntimeCompiledKernel(program, kernel, defines) =
    member val Program:OpenCLProgram = program with get  
    member val Kernel:OpenCLKernel = kernel with get  
    member val DynamicDefineValues: IReadOnlyDictionary<string, string> = defines with get

    interface IDisposable with
        member this.Dispose() =
            this.Kernel.Dispose()
            this.Program.Dispose()

type RuntimeKernelCacheEntry(km) =
    inherit KernelCacheEntry(km)
    
    // List of devices and kernel instances potentially executing the kernel
    member val Instances:Dictionary<int * int * string, RuntimeCompiledKernel> = new Dictionary<int * int * string, RuntimeCompiledKernel>() with get 
    
    interface IDisposable with
        member this.Dispose() =
            for item in this.Instances do
                (item.Value :> IDisposable).Dispose()
    
type DeviceCache() =
    member val Devices = new Dictionary<int * int, RuntimeDevice>() 
        with get

    interface IDisposable with
        member this.Dispose() =
            for item in this.Devices do
                (item.Value :> IDisposable).Dispose()
                