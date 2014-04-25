namespace FSCL.Runtime.Managers

open OpenCL
open System
open FSCL.Compiler
open FSCL.Compiler.Language
open System.Runtime.InteropServices
open FSCL.Compiler.Configuration
open Microsoft.FSharp.Core.LanguagePrimitives
open System.Collections.Generic
  
module internal MemoryUtil =      
    [<DllImport("kernel32.dll")>]
    extern void RtlMoveMemory(IntPtr dest, IntPtr src, uint32 len);

open MemoryUtil

type BufferTools() =  
    static member ToOpenCLMemoryFlags(flags: MemoryFlags) =
        EnumOfValue<int64, OpenCLMemoryFlags> (flags |> int64)

    static member AreOpenCLMemoryFlagsCompatible(given: OpenCLMemoryFlags, required: OpenCLMemoryFlags) =
        // Remove readonly, readwrite, writeonly, hostreadonly, hostwriteonly and hostnoaccess bits
        let givenWithNoAccess = given &&& (~~~ (OpenCLMemoryFlags.ReadOnly ||| OpenCLMemoryFlags.WriteOnly ||| OpenCLMemoryFlags.ReadWrite ||| OpenCLMemoryFlags.HostReadOnly ||| OpenCLMemoryFlags.HostWriteOnly ||| OpenCLMemoryFlags.HostNoAccess))
        let requiredWithNoAccess = required &&& (~~~ (OpenCLMemoryFlags.ReadOnly ||| OpenCLMemoryFlags.WriteOnly ||| OpenCLMemoryFlags.ReadWrite ||| OpenCLMemoryFlags.HostReadOnly ||| OpenCLMemoryFlags.HostWriteOnly ||| OpenCLMemoryFlags.HostNoAccess))
        // Check that the remaining bits match
        if givenWithNoAccess <> requiredWithNoAccess then
            false
        else 
            // Read/Write unrelated bits are equal, let's check if related ones are compatible
            let kernelAccessOk =
                // If a readwrite is given, then ok
                if (given &&& OpenCLMemoryFlags.ReadWrite) |> int > 0 then
                    true
                // If nothing is given, then ok (is readwrite)
                else if (given &&& (OpenCLMemoryFlags.ReadWrite ||| OpenCLMemoryFlags.ReadOnly ||| OpenCLMemoryFlags.WriteOnly)) |> int = 0 then
                    true
                // Otherwise must be both read only or both write only
                else (given &&& (OpenCLMemoryFlags.ReadOnly ||| OpenCLMemoryFlags.WriteOnly)) = (required &&& (OpenCLMemoryFlags.ReadOnly ||| OpenCLMemoryFlags.WriteOnly))
                    
            // Host-side access
            let hostAccessOk =
                // If nothing is given then ok (the given host is readwrite)
                if (given &&& (OpenCLMemoryFlags.HostReadOnly ||| OpenCLMemoryFlags.HostWriteOnly ||| OpenCLMemoryFlags.HostNoAccess)) |> int = 0 then
                    true
                // If given is readonly or writeonly and the required is noaccess then ok
                else if ((given &&& OpenCLMemoryFlags.HostReadOnly |> int > 0) || (given &&& OpenCLMemoryFlags.HostWriteOnly |> int > 0)) && (required &&& OpenCLMemoryFlags.HostNoAccess |> int > 0) then
                    true
                // Otherwise must be both read only or both write only
                else (given &&& (OpenCLMemoryFlags.HostNoAccess ||| OpenCLMemoryFlags.HostReadOnly ||| OpenCLMemoryFlags.HostWriteOnly)) = (required &&& (OpenCLMemoryFlags.HostNoAccess ||| OpenCLMemoryFlags.HostReadOnly ||| OpenCLMemoryFlags.HostWriteOnly))
                    
            kernelAccessOk && hostAccessOk
        
    static member MergeAccessAndFlags(space: AddressSpace, mode: AccessMode, flags: MemoryFlags, isRoot: bool, isReturn: bool) =
        // Convert wrapper-independant flags to wrapper-dependant flags
        let mutable oclFlags = EnumOfValue<int64, OpenCLMemoryFlags> (flags |> int64)
        // Override with flags from access analysis is no read/write/readwrite access set
        if ((oclFlags &&& OpenCLMemoryFlags.ReadOnly) |> int = 0) &&
           ((oclFlags &&& OpenCLMemoryFlags.WriteOnly) |> int = 0) &&
           ((oclFlags &&& OpenCLMemoryFlags.ReadWrite) |> int = 0) then
            // If the address space is Constant we set always ReadOnly cause kernel cannot write the buffer
            if space = AddressSpace.Constant then 
                oclFlags <- oclFlags ||| OpenCLMemoryFlags.ReadOnly
            else
                // If not isRoot && isReturn, then it is expected that this buffer will be written by the current kernel and read by another
                // To avoid copy cause incompatible flags, we set ReadWrite for this buffer
                if (not isRoot) && isReturn then
                    oclFlags <- oclFlags ||| OpenCLMemoryFlags.ReadWrite
                // Otherwise let's look at access analysis
                else 
                    match mode with
                    | AccessMode.ReadAccess ->
                        oclFlags <- oclFlags ||| OpenCLMemoryFlags.ReadOnly
                    | AccessMode.WriteAccess ->
                        oclFlags <- oclFlags ||| OpenCLMemoryFlags.WriteOnly
                    | _ ->
                        oclFlags <- oclFlags ||| OpenCLMemoryFlags.ReadWrite
        oclFlags

    static member ShouldWriteBuffer(buffer:OpenCLBuffer, space: AddressSpace, transferMode: TransferMode) =
        buffer.HostCanWrite && 
        buffer.KernelCanRead && 
        space <> AddressSpace.Private &&
        ((transferMode &&& TransferMode.NoTransfer) |> int = 0)
        
    static member ShouldReadBuffer(buffer:OpenCLBuffer, space: AddressSpace, transferMode: TransferMode) =
        buffer.HostCanRead && 
        buffer.KernelCanWrite && 
        space <> AddressSpace.Local &&
        space <> AddressSpace.Private &&
        space <> AddressSpace.Constant &&
        ((transferMode &&& TransferMode.NoTransferBack) |> int = 0)     
        
    static member CopyBuffer(q:OpenCLCommandQueue, input:OpenCLBuffer, output:OpenCLBuffer) =
        let events = new List<OpenCLEventBase>()
        q.CopyBuffer(input, output, 0L, 0L, input.TotalCount, events)
        q.Wait(events)
        
    static member CreateBuffer(t:Type, 
                               count:int64[],
                               c:OpenCLContext, 
                               q:OpenCLCommandQueue, 
                               flags: OpenCLMemoryFlags) =
        let buffer = new OpenCLBuffer(c, flags, t, count)
        buffer

    static member WriteBuffer(queue: OpenCLCommandQueue, useMap:bool, buffer:OpenCLBuffer, o:Array, ?count:int64[]) =
        if useMap then
            let tCount = if count.IsSome then Array.reduce (fun a b -> a * b) count.Value else buffer.TotalCount
            let sourceHandle = GCHandle.Alloc(o, GCHandleType.Pinned)
            try 
                let srcPtr = sourceHandle.AddrOfPinnedObject()
                let dstPtr = queue.Map(buffer, true, OpenCLMemoryMappingFlags.Write, 0L, tCount, null)
                RtlMoveMemory(srcPtr, dstPtr, buffer.Size |> uint32)                                   
                queue.Unmap(buffer, ref dstPtr, null)                             
            finally
                if (sourceHandle.IsAllocated) then
                    sourceHandle.Free()
        else
            match buffer.Count.Length with
            | 1 ->
                if count.IsNone then
                    queue.WriteToBuffer(o, buffer, false, null)
                else
                    queue.WriteToBuffer(o, buffer, false, 0L, 0L, count.Value.[0], null)
            | 2 ->
                let offset = OpenCL.SysIntX2(0, 0)                
                let region = 
                    if count.IsSome then
                        OpenCL.SysIntX2(count.Value.[0], count.Value.[1])
                    else
                        OpenCL.SysIntX2(o.GetLength(0), o.GetLength(1))
                queue.WriteToBuffer(o, buffer, false, offset, offset, region, null)
            | _ ->
                let offset = OpenCL.SysIntX3(0, 0, 0)
                let region = 
                    if count.IsSome then
                        OpenCL.SysIntX3(count.Value.[0], count.Value.[1], count.Value.[2])
                    else
                        OpenCL.SysIntX3(o.GetLength(0), o.GetLength(1), o.GetLength(2))
                queue.WriteToBuffer(o, buffer, false, offset, offset, region, null)
            

    static member ReadBuffer(queue: OpenCLCommandQueue, useMap: bool, o: Array, buffer: OpenCLBuffer, ?count:int64[]) =    
        if useMap then
            let tCount = if count.IsSome then Array.reduce (fun a b -> a * b) count.Value else buffer.TotalCount
            let dstHandle = GCHandle.Alloc(o, GCHandleType.Pinned)
            try 
                let dstPtr = dstHandle.AddrOfPinnedObject()
                let srcPtr = queue.Map(buffer, true, OpenCLMemoryMappingFlags.Read, 0L, tCount, null)
                RtlMoveMemory(srcPtr, dstPtr, buffer.Size |> uint32)                                   
                queue.Unmap(buffer, ref srcPtr, null)                             
            finally
                if (dstHandle.IsAllocated) then
                    dstHandle.Free()
        else
            match buffer.Count.Length with
            | 1 ->
                if count.IsNone then
                    queue.ReadFromBuffer(buffer, ref o, true, 0L, 0L, o.LongLength, null)           
                else
                    queue.ReadFromBuffer(buffer, ref o, true, 0L, 0L, count.Value.[0], null)
            | 2 ->
                let offset = OpenCL.SysIntX2(0,0)
                let region = 
                    if count.IsSome then
                        OpenCL.SysIntX2(count.Value.[0], count.Value.[1])
                    else
                        OpenCL.SysIntX2(o.GetLength(0), o.GetLength(1))
                queue.ReadFromBuffer(buffer, 
                    ref o, 
                    true, 
                    offset, offset, region, null)
            | _ ->
                let offset = OpenCL.SysIntX3(0,0,0)
                let region = 
                    if count.IsSome then
                        OpenCL.SysIntX3(count.Value.[0], count.Value.[1], count.Value.[2])
                    else
                        OpenCL.SysIntX3(o.GetLength(0), o.GetLength(1), o.GetLength(2))
                queue.ReadFromBuffer(buffer, ref o, true, offset, offset, region, null)
       