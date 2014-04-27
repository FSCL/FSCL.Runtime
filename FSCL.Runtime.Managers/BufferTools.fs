namespace FSCL.Runtime.Managers

open OpenCL
open System
open FSCL.Compiler
open FSCL.Compiler.Language
open System.Runtime.InteropServices
open FSCL.Runtime
open FSCL.Compiler.Configuration
open System.Collections.Generic
  
module internal MemoryUtil =      
    [<DllImport("kernel32.dll")>]
    extern void RtlMoveMemory(IntPtr dest, IntPtr src, uint32 len);

open MemoryUtil

type BufferTools() =          
    static member CopyBuffer(q:OpenCLCommandQueue, input:OpenCLBuffer, output:OpenCLBuffer) =
        let events = new List<OpenCLEventBase>()
        q.CopyBuffer(input, output, 0L, 0L, input.TotalCount, events)
        q.Wait(events)
        
    static member CreateBuffer(t:Type, 
                               count:int64[],
                               c:OpenCLContext, 
                               q:OpenCLCommandQueue, 
                               flags: OpenCLMemoryFlags) =
        //Console.WriteLine("Creating buffer with size " + (Array.reduce(fun a b -> a * b) count).ToString() + " and flags " + flags.ToString())
        let buffer = new OpenCLBuffer(c, flags, t, count)
        buffer
        
    static member CreateBuffer(a: obj,
                               c:OpenCLContext, 
                               q:OpenCLCommandQueue, 
                               flags: OpenCLMemoryFlags) =
        //Console.WriteLine("Creating buffer with size " + (Array.reduce(fun a b -> a * b) count).ToString() + " and flags " + flags.ToString())
        let buffer = new OpenCLBuffer(c, flags, a :?> Array)
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
            let evt = new List<OpenCLEventBase>()
            match buffer.Count.Length with
            | 1 ->
                if count.IsNone then
                    queue.WriteToBuffer(o, buffer, false, evt)
                else
                    queue.WriteToBuffer(o, buffer, false, 0L, 0L, count.Value.[0], evt)
            | 2 ->
                let offset = OpenCL.SysIntX2(0, 0)                
                let region = 
                    if count.IsSome then
                        OpenCL.SysIntX2(count.Value.[0], count.Value.[1])
                    else
                        OpenCL.SysIntX2(o.GetLength(0), o.GetLength(1))
                queue.WriteToBuffer(o, buffer, false, offset, offset, region, evt)
            | _ ->
                let offset = OpenCL.SysIntX3(0, 0, 0)
                let region = 
                    if count.IsSome then
                        OpenCL.SysIntX3(count.Value.[0], count.Value.[1], count.Value.[2])
                    else
                        OpenCL.SysIntX3(o.GetLength(0), o.GetLength(1), o.GetLength(2))
                queue.WriteToBuffer(o, buffer, false, offset, offset, region, evt)
            
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
       