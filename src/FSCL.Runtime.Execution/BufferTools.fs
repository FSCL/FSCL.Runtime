namespace FSCL.Runtime.Managers

open OpenCL
open System
open FSCL
open FSCL.Compiler
open FSCL.Compiler.Util
open FSCL.Language
open FSCL.Runtime
open System.Runtime.InteropServices
open FSCL.Runtime
open FSCL.Compiler.Configuration
open System.Collections.Generic
open Microsoft.FSharp.Reflection

module internal MemoryUtil =     
    [<DllImport("msvcrt.dll", SetLastError=false, CallingConvention=CallingConvention.Cdecl)>]
    extern IntPtr memcpy(IntPtr dest, IntPtr src, UIntPtr len);

open MemoryUtil

type BufferTools() =          
    static member CopyBuffer(q:OpenCLCommandQueue, input:OpenCLBuffer, output:OpenCLBuffer) =
        let events = new List<OpenCLEventBase>()
        q.CopyBuffer(input, output, 0L, 0L, input.TotalCount, null, events)
        q.Wait(events.AsReadOnly())
        events.[0].Dispose()
        
    static member CreateBuffer(t:Type, 
                               count:int64[],
                               c:OpenCLContext, 
                               q:OpenCLCommandQueue, 
                               flags: OpenCLMemoryFlags) =
        //Console.WriteLine("Creating buffer with size " + (Array.reduce(fun a b -> a * b) count).ToString() + " and flags " + flags.ToString())
        let buffer = new OpenCLBuffer(c, flags, t, count)
        buffer
        
    static member CreateBuffer(ptr: IntPtr,
                               t:Type, 
                               count:int64[],
                               c:OpenCLContext, 
                               q:OpenCLCommandQueue, 
                               flags: OpenCLMemoryFlags) =
        //Console.WriteLine("Creating buffer with size " + (Array.reduce(fun a b -> a * b) count).ToString() + " and flags " + flags.ToString())
        let buffer = new OpenCLBuffer(c, flags, t, count, ptr)
        buffer
        
    static member CreateBuffer(a: Array,
                               c:OpenCLContext, 
                               q:OpenCLCommandQueue, 
                               flags: OpenCLMemoryFlags) =
        if (flags &&& (OpenCLMemoryFlags.CopyHostPointer ||| OpenCLMemoryFlags.UseHostPointer) |> int > 0) then
        //Console.WriteLine("Creating buffer with size " + (Array.reduce(fun a b -> a * b) count).ToString() + " and flags " + flags.ToString())
            new OpenCLBuffer(c, flags, a)
        else
            let count = ArrayUtil.GetArrayLengths(a)
            new OpenCLBuffer(c, flags, a.GetType().GetElementType(), count)

    static member WriteBuffer(queue: OpenCLCommandQueue, useMap:bool, buffer:OpenCLBuffer, arr:Array, ?count:int64[]) =
        let evt = null;//new List<OpenCLEventBase>()
        if useMap then
            let tCount = if count.IsSome then Array.reduce (fun a b -> a * b) count.Value else buffer.TotalCount
            let tBytes = tCount * (Marshal.SizeOf(buffer.ElementType) |> int64)
            let sourceHandle = GCHandle.Alloc(arr, GCHandleType.Pinned)
            try 
                let srcPtr = sourceHandle.AddrOfPinnedObject()
                let dstPtr = queue.Map(buffer, true, OpenCLMemoryMappingFlags.Write, 0L, tCount, null, evt)
                memcpy(dstPtr, srcPtr, new UIntPtr(tBytes |> uint64)) |> ignore                           
                queue.Unmap(buffer, ref dstPtr, null, null)                             
            finally
                if (sourceHandle.IsAllocated) then
                    sourceHandle.Free()
        else
            match buffer.Count.Length with
            | 1 ->
                if count.IsNone then
                    queue.WriteToBuffer(arr, buffer, false, null, evt)
                else
                    queue.WriteToBuffer(arr, buffer, false, 0L, 0L, count.Value.[0], null, evt)
            | 2 ->
                let offset = OpenCL.SysIntX2(0, 0)                
                let region = 
                    if count.IsSome then
                        OpenCL.SysIntX2(count.Value.[0], count.Value.[1])
                    else
                        OpenCL.SysIntX2(arr.GetLength(0), arr.GetLength(1))
                queue.WriteToBuffer(arr, buffer, false, offset, offset, region, null, evt)
            | _ ->
                let offset = OpenCL.SysIntX3(0, 0, 0)
                let region = 
                    if count.IsSome then
                        OpenCL.SysIntX3(count.Value.[0], count.Value.[1], count.Value.[2])
                    else
                        OpenCL.SysIntX3(arr.GetLength(0), arr.GetLength(1), arr.GetLength(2))
                queue.WriteToBuffer(arr, buffer, false, offset, offset, region, null, evt)
            //evt.[0].Completed.Add(fun st -> evt.[0].Dispose())
            
    static member WriteBuffer(queue: OpenCLCommandQueue, useMap:bool, buffer:OpenCLBuffer, arr:IntPtr, elementType: Type, count:int64[]) =
        let evt = null;//new List<OpenCLEventBase>()
        if useMap then
            let tCount = Array.reduce (fun a b -> a * b) count
            let tBytes = tCount * (Marshal.SizeOf(buffer.ElementType) |> int64)
            try 
                let dstPtr = queue.Map(buffer, true, OpenCLMemoryMappingFlags.Write, 0L, tCount, null, evt)
                memcpy(dstPtr, arr, new UIntPtr(tBytes |> uint64)) |> ignore                           
                queue.Unmap(buffer, ref dstPtr, null, null)                             
            finally
                ()
        else
            match buffer.Count.Length with
            | 1 ->
                queue.WriteToBuffer(arr, buffer, false, 0L, count.[0], null, evt)
            | 2 ->
                let offset = OpenCL.SysIntX2(0, 0)                
                let region = OpenCL.SysIntX2(count.[0], count.[1])
                queue.WriteToBuffer(arr, buffer, false, offset, region, null, evt)
            | _ ->
                let offset = OpenCL.SysIntX3(0, 0, 0)
                let region = 
                    OpenCL.SysIntX3(count.[0], count.[1], count.[2])
                queue.WriteToBuffer(arr, buffer, false, offset, region, null, evt)
            //evt.[0].Completed.Add(fun st -> evt.[0].Dispose())
            
    static member ReadBuffer(queue: OpenCLCommandQueue, useMap: bool, o: Array, buffer: OpenCLBuffer, ?count:int64[]) =        
        let evt = null;//new List<OpenCLEventBase>()
        if useMap then
            let tCount = if count.IsSome then Array.reduce (fun a b -> a * b) count.Value else buffer.TotalCount
            let tBytes = tCount * (Marshal.SizeOf(buffer.ElementType) |> int64)
            let dstHandle = GCHandle.Alloc(o, GCHandleType.Pinned)
            try 
                let dstPtr = dstHandle.AddrOfPinnedObject()
                let srcPtr = queue.Map(buffer, true, OpenCLMemoryMappingFlags.Read, 0L, tCount, null, evt)
                memcpy(dstPtr, srcPtr, new UIntPtr(tBytes |> uint64)) |> ignore                                                          
                queue.Unmap(buffer, ref srcPtr, null, null)                             
            finally
                if (dstHandle.IsAllocated) then
                    dstHandle.Free()
        else
            match buffer.Count.Length with
            | 1 ->
                if count.IsNone then
                    queue.ReadFromBuffer(buffer, ref o, true, 0L, 0L, o.LongLength, null, evt)           
                else
                    queue.ReadFromBuffer(buffer, ref o, true, 0L, 0L, count.Value.[0], null, evt)
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
                    offset, offset, region, null, null)
            | _ ->
                let offset = OpenCL.SysIntX3(0,0,0)
                let region = 
                    if count.IsSome then
                        OpenCL.SysIntX3(count.Value.[0], count.Value.[1], count.Value.[2])
                    else
                        OpenCL.SysIntX3(o.GetLength(0), o.GetLength(1), o.GetLength(2))
                queue.ReadFromBuffer(buffer, ref o, true, offset, offset, region, null, evt)
        //evt.[0].Completed.Add(fun st -> evt.[0].Dispose())
       
    static member ReadBuffer(queue: OpenCLCommandQueue, useMap: bool, o: IntPtr, buffer: OpenCLBuffer, count:int64[]) =        
        let evt = null;//new List<OpenCLEventBase>()
        if useMap then
            let tCount = Array.reduce (fun a b -> a * b) count
            let tBytes = tCount * (Marshal.SizeOf(buffer.ElementType) |> int64)
            try 
                let srcPtr = queue.Map(buffer, true, OpenCLMemoryMappingFlags.Read, 0L, tCount, null, evt)
                memcpy(o, srcPtr, new UIntPtr(tBytes |> uint64)) |> ignore                                                          
                queue.Unmap(buffer, ref srcPtr, null, null)                             
            finally
                ()
        else
            match buffer.Count.Length with
            | 1 ->
                queue.ReadFromBuffer(buffer, o, true, 0L, count.[0], null, evt)       
            | 2 ->
                let offset = OpenCL.SysIntX2(0,0)
                let region = 
                    OpenCL.SysIntX2(count.[0], count.[1])
                queue.ReadFromBuffer(buffer, 
                    o, 
                    true, 
                    offset, region, null, null)
            | _ ->
                let offset = OpenCL.SysIntX3(0,0,0)
                let region = 
                    OpenCL.SysIntX3(count.[0], count.[1], count.[2])
                queue.ReadFromBuffer(buffer, o, true, offset, region, null, evt)
        //evt.[0].Completed.Add(fun st -> evt.[0].Dispose())
       