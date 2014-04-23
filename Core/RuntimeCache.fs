namespace FSCL.Runtime

open System
open OpenCL
open FSCL.Compiler
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Quotations

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
    member val DynamicDefines: IReadOnlyDictionary<string, string> = defines with get

    interface IDisposable with
        member this.Dispose() =
            this.Kernel.Dispose()
            this.Program.Dispose()

[<AllowNullLiteral>]
type RuntimeKernel(info, code) =
    member val Kernel:IKernelInfo = info with get 
    member val OpenCLCode:String = code with get, set
    // List of devices and kernel instances potentially executing the kernel
    member val Instances:Dictionary<int * int, RuntimeCompiledKernel> = new Dictionary<int * int, RuntimeCompiledKernel>() with get 
    
    interface IDisposable with
        member this.Dispose() =
            for item in this.Instances do
                (item.Value :> IDisposable).Dispose()
    
type RuntimeCache(openCLMetadataVerifier: ReadOnlyMetaCollection * ReadOnlyMetaCollection -> bool,
                  multithreadMetadataVerifier: ReadOnlyMetaCollection * ReadOnlyMetaCollection -> bool) =
    member val OpenCLKernels = Dictionary<FunctionInfoID, List<ReadOnlyMetaCollection * RuntimeKernel>>() 
        with get
    member val MultithreadKernels = Dictionary<FunctionInfoID, List<ReadOnlyMetaCollection * MethodInfo>>() 
        with get
    member val Devices = new Dictionary<int * int, RuntimeDevice>() 
        with get
    
    member this.TryFindCompatibleOpenCLCachedKernel(id: FunctionInfoID, 
                                                    meta: ReadOnlyMetaCollection) =
        if this.OpenCLKernels.ContainsKey(id) then
            let potentialKernels = this.OpenCLKernels.[id]
            // Check if compatible kernel meta in cached kernels
            let item = Seq.tryFind(fun (cachedMeta: ReadOnlyMetaCollection, cachedKernel: RuntimeKernel) ->
                                        openCLMetadataVerifier(cachedMeta, meta)) potentialKernels
            match item with
            | Some(m, k) ->
                Some(k)
            | _ ->
                None
        else
            None  
                     
    member this.TryFindCompatibleMultithreadCachedKernel(id: FunctionInfoID, 
                                                         meta: ReadOnlyMetaCollection) =
        if this.MultithreadKernels.ContainsKey(id) then
            let potentialKernels = this.MultithreadKernels.[id]
            // Check if compatible kernel meta in cached kernels
            let item = Seq.tryFind(fun (cachedMeta: ReadOnlyMetaCollection, cachedKernel: MethodInfo) ->
                                        multithreadMetadataVerifier(cachedMeta, meta)) potentialKernels
            match item with
            | Some(m, k) ->
                Some(k)
            | _ ->
                None
        else
            None                

    interface IDisposable with
        member this.Dispose() =
            for item in this.OpenCLKernels do
                for m, k in item.Value do
                    (k :> IDisposable).Dispose()
            for item in this.Devices do
                (item.Value :> IDisposable).Dispose()
                