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
    extern IntPtr memcpy(IntPtr dest, IntPtr src, UIntPtr len)
    [<DllImport("msvcrt.dll", EntryPoint = "memset", CallingConvention = CallingConvention.Cdecl, SetLastError = false)>]
    extern IntPtr memset(IntPtr dest, int c, int count)

open MemoryUtil

type BufferTools() =          
    static member CopyBuffer(q:OpenCLCommandQueue, input:OpenCLBuffer, output:OpenCLBuffer) =
        // Check if buffers same context
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
        if (flags &&& (OpenCLMemoryFlags.CopyHostPointer ||| OpenCLMemoryFlags.UseHostPointer) |> int > 0) then
            new OpenCLBuffer(c, flags, t, count, ptr)
        else
            new OpenCLBuffer(c, flags, t, count)
                    
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
                queue.WriteToBuffer(arr, buffer, false, offset, offset, region, null, evt)
            | _ ->
                let offset = OpenCL.SysIntX3(0, 0, 0)
                let region = 
                    OpenCL.SysIntX3(count.[0], count.[1], count.[2])
                queue.WriteToBuffer(arr, buffer, false, offset, offset, region, null, evt)
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
                queue.ReadFromBuffer(buffer, o, true, 0L, count.[0] * count.[1], null, evt)  
//                let offset = OpenCL.SysIntX2(0,0)
//                let region = 
//                    OpenCL.SysIntX2(count.[0], count.[1])
//                queue.ReadFromBuffer(buffer, 
//                    o, 
//                    true, 
//                    offset, region, null, null)
            | _ ->
                queue.ReadFromBuffer(buffer, o, true, 0L, count.[0] * count.[1] * count.[2], null, evt)  
//                let offset = OpenCL.SysIntX3(0,0,0)
//                let region = 
//                    OpenCL.SysIntX3(count.[0], count.[1], count.[2])
//                queue.ReadFromBuffer(buffer, o, true, offset, region, null, evt)
        //evt.[0].Completed.Add(fun st -> evt.[0].Dispose())
       