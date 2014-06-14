namespace FSCL
open System
open System.Collections.Generic
open FSCL.Language
open FSCL.Compiler

module Language =
    // Kernel run mode
    type RunningMode =
    | OpenCL
    | Multithread
    | Sequential
    
    // BufferReusePriority
    type BufferSharePriority =
    | PriorityToFlags
    | PriorityToShare

    ///
    ///<summary>
    ///The attribute to specify a device
    ///</summary>
    ///
    [<AllowNullLiteral>]
    type DeviceAttribute(plat: int, dev: int) =
        inherit KernelMetadataAttribute()
        member val Platform = plat with get
        member val Device = dev with get
        new() =
            DeviceAttribute(0, 0)   
             
    ///
    ///<summary>
    ///The attribute to specify a device
    ///</summary>
    ///
    [<AllowNullLiteral>]
    type RunningModeAttribute(mode: RunningMode) =
        inherit KernelMetadataAttribute()
        member val RunningMode = mode with get
        new() =
            RunningModeAttribute(RunningMode.OpenCL)   
                
    ///
    ///<summary>
    ///The attribute to specify multithread fallback
    ///</summary>
    ///
    [<AllowNullLiteral>]
    type MultithreadFallbackAttribute(fallback: bool) =
        inherit KernelMetadataAttribute()
        member val Fallback = fallback with get
        new() =
            MultithreadFallbackAttribute(true)   

    ///
    ///<summary>
    ///The attribute to specify a device
    ///</summary>
    ///
    [<AllowNullLiteral>]
    type WorkSizeAttribute(globalSize: int64 array, localSize: int64 array) =
        inherit KernelMetadataAttribute()
        member val GlobalSize = globalSize with get
        member val LocalSize = localSize with get
        new(globalSize: int64, localSize: int64) =
            WorkSizeAttribute([| globalSize |], [| localSize |])  
        new() =
            WorkSizeAttribute([| 0L |], [| 0L |])   
            
    // Functions matching attributes for dynamic marking of parameters
    [<KernelMetadataFunction(typeof<DeviceAttribute>)>]
    let DEVICE(plat: int, dev: int, a) = 
        a
        
    [<KernelMetadataFunction(typeof<WorkSizeAttribute>)>]
    let WORKSIZE(globalSize: int64 array, localSize: int64 array, a) = 
        a

    [<KernelMetadataFunction(typeof<RunningModeAttribute>)>]
    let RUNNING_MODE(mode: RunningMode, a) = 
        a
                
    [<KernelMetadataFunction(typeof<MultithreadFallbackAttribute>)>]
    let MULTITHREAD_FALLBACK(fallback: bool, a) = 
        a

    type WorkItemIdContainer(global_size: int[], local_size: int[], global_id: int[], local_id: int [], global_offset: int[]) = 
        member this.GlobalID(dim) =
            global_id.[dim]
        member this.LocalID(dim) =
            local_id.[dim]
        member this.GlobalSize(dim) =
            global_size.[dim]
        member this.LocalSize(dim) =
            local_size.[dim]
        member this.NumGroups(dim) =
            (int)(Math.Ceiling((float)global_size.[dim] / (float)local_size.[dim]))
        member this.GroupID(dim) =
            (int)(Math.Floor((float)global_id.[dim] / (float)global_size.[dim]))
        member this.GlobalOffset(dim) =
            global_offset.[dim]
        member this.WorkDim() =
            global_size.Rank
        


