namespace FSCL.Runtime.Managers

open OpenCL
open System
open FSCL.Compiler
open System.Runtime.InteropServices
open FSCL.Compiler.Configuration
        
type BufferTools() =  
    static member AccessModeToFlags(mode: AccessMode) =
        match mode with
        | AccessMode.ReadAccess ->
            OpenCLMemoryFlags.ReadWrite
        | AccessMode.WriteAccess ->
            OpenCLMemoryFlags.ReadWrite
        | _ ->
            OpenCLMemoryFlags.ReadWrite
     
    static member CopyBuffer(q:OpenCLCommandQueue, input:OpenCLBuffer, output:OpenCLBuffer) =
        q.CopyBuffer(input, output, 0L, 0L, input.TotalCount, null)
        
    static member CreateBuffer(t:Type, 
                               count:int64[],
                               c:OpenCLContext, 
                               q:OpenCLCommandQueue, 
                               flags: OpenCLMemoryFlags) =
        let buffer = new OpenCLBuffer(c, flags, t, count)
        buffer

    static member WriteBuffer(queue: OpenCLCommandQueue, buffer:OpenCLBuffer, o:Array, ?count:int[]) =
        match buffer.Count.Length with
        | 1 ->
            if count.IsNone then
                queue.WriteToBuffer(o, buffer, false, null)
            else
                queue.WriteToBuffer(o, buffer, false, 0L, 0L, count.Value.[0] * Marshal.SizeOf(typeof<'T>) |> int64, null)
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
            

    static member ReadBuffer(queue: OpenCLCommandQueue, o: Array, buffer: OpenCLBuffer, ?count:int[]) =    
        match buffer.Count.Length with
        | 1 ->
            if count.IsNone then
                queue.ReadFromBuffer(buffer, ref o, true, 0L, 0L, o.LongLength, null)           
            else
                queue.ReadFromBuffer(buffer, ref o, true, 0L, 0L, count.Value.[0] |> int64, null)
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
       