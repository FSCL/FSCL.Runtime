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
type RuntimeCompiledKernel(program, kernelName, defines) =
    // Same kernel instance cannot run concurrently, must create
    // different instances of the same kernel if more than one thread runnin
    let kernelClones = new HashSet<OpenCLKernel>()
    let isRunning = ref false

    member val Program:OpenCLProgram = program with get  
    member val DynamicDefineValues: IReadOnlyDictionary<string, string> = defines with get

    member this.StartUsingKernel() =
        lock kernelClones (fun () ->
            if not !isRunning then
                isRunning := true
            // Clone and return
            let clone = this.Program.CreateKernel(kernelName)
            kernelClones.Add(clone) |> ignore
            clone)

    member this.EndUsingKernel(k:OpenCLKernel) =
        lock kernelClones (fun () ->
            if !isRunning then
                // Check if any clone, in such case remove and dispose
                if kernelClones.Contains(k) then
                    kernelClones.Remove(k) |> ignore
                    k.Dispose()
                // If clone set is empty, set running to false 
                if kernelClones.Count = 0 then
                    isRunning := false)

    interface IDisposable with
        member this.Dispose() =
            lock kernelClones (fun () ->
                for item in kernelClones do
                    item.Dispose())
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
    let i = 0
    member val Devices = new Dictionary<int * int, RuntimeDevice>() 
        with get

    interface IDisposable with
        member this.Dispose() =
            for item in this.Devices do
                (item.Value :> IDisposable).Dispose()
                