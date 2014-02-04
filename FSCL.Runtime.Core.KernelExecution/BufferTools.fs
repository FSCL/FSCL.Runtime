namespace FSCL.Runtime.KernelExecution

open Cloo
open System
open FSCL.Compiler
open System.Runtime.InteropServices
        
type internal BufferTools() =  
    static member WriteBuffer<'T when 'T: struct and 'T : (new : unit -> 'T) and 'T :> System.ValueType>(c:ComputeContext, q:ComputeCommandQueue, arg:obj, dims, shouldInit) =
        //let dims = FSCL.Util.GetArrayDimensions(arg.Type)
        match dims with
        | 1 ->
            let actualArg = arg :?> 'T[]
            let buffer = new ComputeBuffer<'T>(c, ComputeMemoryFlags.None, [| actualArg.LongLength |])
            if shouldInit then
                q.WriteToBuffer<'T>(actualArg, buffer, false, null)
            buffer :> ComputeMemory
        | 2 ->
            let actualArg = arg :?> 'T[,]
            let buffer = new ComputeBuffer<'T>(
                            c, 
                            ComputeMemoryFlags.None, 
                            [| actualArg.GetLongLength(0); actualArg.GetLongLength(1) |])
            if shouldInit then
                let offset = Cloo.SysIntX2(0, 0)                
                let region = Cloo.SysIntX2(actualArg.GetLength(0), actualArg.GetLength(1))
                q.WriteToBuffer<'T>(actualArg, buffer, false, offset, offset, region, null)
            buffer :> ComputeMemory
        | _ ->
            let actualArg = arg :?> 'T[,,]
            let buffer = new ComputeBuffer<'T>(
                            c, 
                            ComputeMemoryFlags.None, 
                            [| actualArg.GetLongLength(0); actualArg.GetLongLength(1); actualArg.GetLongLength(2) |])
            if shouldInit then
                let offset = Cloo.SysIntX3(0, 0, 0)
                let region = Cloo.SysIntX3(actualArg.GetLength(0), actualArg.GetLength(1), actualArg.GetLength(2)) 
                q.WriteToBuffer<'T>(actualArg, buffer, false, offset, offset, region, null)
            buffer :> ComputeMemory
            
    static member ReadBuffer<'T when 'T: struct and 'T : (new : unit -> 'T) and 'T :> System.ValueType>(c:ComputeContext, q:ComputeCommandQueue, arg:obj, dims, buffer: ComputeBuffer<'T>) =
        match dims with
        | 1 ->
            let actualArg = arg :?> 'T[]
            q.ReadFromBuffer<'T>(buffer, ref actualArg, true, null)            
        | 2 ->
            let actualArg = arg :?> 'T[,]
            let offset = Cloo.SysIntX2(0,0)
            let region = Cloo.SysIntX2(actualArg.GetLength(0), actualArg.GetLength(1))
            //q.ReadFromBuffer<'T>(buffer, ref actualArg, true, null); 
            q.ReadFromBuffer<'T>(buffer, 
                ref actualArg, 
                true, 
                offset, offset, region, 
                (actualArg.GetLength(0) * Marshal.SizeOf(typeof<'T>)) |> int64, (actualArg.GetLength(0) *Marshal.SizeOf(typeof<'T>)) |> int64, null)
        | _ ->
            let actualArg = arg :?> 'T[,,]
            let offset = Cloo.SysIntX3(0,0,0)
            let region = Cloo.SysIntX3(actualArg.GetLength(0), actualArg.GetLength(1), actualArg.GetLength(2))
            q.ReadFromBuffer<'T>(buffer, ref actualArg, true, offset, offset, region, null)
            
    static member CopyBuffer<'T when 'T: struct and 'T : (new : unit -> 'T) and 'T :> System.ValueType>(c:ComputeContext, q:ComputeCommandQueue, input:ComputeBuffer<'T>) =
        //let dims = FSCL.Util.GetArrayDimensions(arg.Type)
        let buffer = new ComputeBuffer<'T>(c, ComputeMemoryFlags.None, input.Count)
        q.CopyBuffer<'T>(input, buffer, 0L, 0L, input.TotalCount, null)
        buffer :> ComputeMemory
        
    static member CreateBuffer(t:Type, 
                               count:int64[],
                               c:ComputeContext, 
                               q:ComputeCommandQueue, 
                               flags:ComputeMemoryFlags) =
        let mutable buffer = None
        if (t = typeof<uint32>) then
            buffer <- Some(new ComputeBuffer<uint32>(c, flags, count) :> ComputeMemory)
        elif (t = typeof<uint64>) then
            buffer <- Some(new ComputeBuffer<uint64>(c, flags, count) :> ComputeMemory)
        elif (t = typeof<int64>) then
            buffer <- Some(new ComputeBuffer<int64>(c, flags, count) :> ComputeMemory)
        elif (t = typeof<int>) then
            buffer <- Some(new ComputeBuffer<int>(c, flags, count) :> ComputeMemory)
        elif (t = typeof<double>) then
            buffer <- Some(new ComputeBuffer<double>(c, flags, count) :> ComputeMemory)
        elif (t = typeof<float32>) then
            buffer <- Some(new ComputeBuffer<float32>(c, flags, count) :> ComputeMemory)
        elif (t = typeof<bool>) then
            buffer <- Some(new ComputeBuffer<int>(c, flags, count) :> ComputeMemory)

        elif (t = typeof<float2>) then
            buffer <- Some(new ComputeBuffer<float2>(c, flags, count) :> ComputeMemory)
        elif (t = typeof<float3>) then
            buffer <- Some(new ComputeBuffer<float3>(c, flags, count) :> ComputeMemory)
        elif (t = typeof<float4>) then
            buffer <- Some(new ComputeBuffer<float4>(c, flags, count) :> ComputeMemory)
            
        elif (t = typeof<int2>) then
            buffer <- Some(new ComputeBuffer<int2>(c, flags, count) :> ComputeMemory)
        elif (t = typeof<int3>) then
            buffer <- Some(new ComputeBuffer<int3>(c, flags, count) :> ComputeMemory)
        elif (t = typeof<int4>) then
            buffer <- Some(new ComputeBuffer<int4>(c, flags, count) :> ComputeMemory)
            
        elif (t = typeof<double2>) then
            buffer <- Some(new ComputeBuffer<double2>(c, flags, count) :> ComputeMemory)
        elif (t = typeof<double3>) then
            buffer <- Some(new ComputeBuffer<double3>(c, flags, count) :> ComputeMemory)
        elif (t = typeof<double4>) then
            buffer <- Some(new ComputeBuffer<double4>(c, flags, count) :> ComputeMemory)
        buffer

    static member WriteBuffer(t, context, queue, o, dim, mustInitBuffer) =
        let mutable buffer = None
        if (t = typeof<uint32>) then
            buffer <- Some(BufferTools.WriteBuffer<uint32>(context, queue, o, dim, mustInitBuffer))
        elif (t = typeof<uint64>) then
            buffer <- Some(BufferTools.WriteBuffer<uint64>(context, queue, o, dim ,mustInitBuffer))
        elif (t = typeof<int64>) then
            buffer <- Some(BufferTools.WriteBuffer<int64>(context, queue, o, dim, mustInitBuffer))
        elif (t = typeof<int>) then
            buffer <- Some(BufferTools.WriteBuffer<int>(context, queue, o, dim, mustInitBuffer))
        elif (t = typeof<double>) then
            buffer <- Some(BufferTools.WriteBuffer<double>(context, queue, o, dim, mustInitBuffer))
        elif (t = typeof<float32>) then
            buffer <- Some(BufferTools.WriteBuffer<float32>(context, queue, o, dim, mustInitBuffer))
        elif (t = typeof<bool>) then
            buffer <- Some(BufferTools.WriteBuffer<int>(context, queue, o, dim, mustInitBuffer))

        elif (t = typeof<float2>) then
            buffer <- Some(BufferTools.WriteBuffer<float2>(context, queue, o, dim, mustInitBuffer))
        elif (t = typeof<float3>) then
            buffer <- Some(BufferTools.WriteBuffer<float3>(context, queue, o, dim, mustInitBuffer))
        elif (t = typeof<float4>) then
            buffer <- Some(BufferTools.WriteBuffer<float4>(context, queue, o, dim, mustInitBuffer))
            
        elif (t = typeof<int2>) then
            buffer <- Some(BufferTools.WriteBuffer<int2>(context, queue, o, dim, mustInitBuffer))
        elif (t = typeof<int3>) then
            buffer <- Some(BufferTools.WriteBuffer<int3>(context, queue, o, dim, mustInitBuffer))
        elif (t = typeof<int4>) then
            buffer <- Some(BufferTools.WriteBuffer<int4>(context, queue, o, dim, mustInitBuffer))
            
        elif (t = typeof<double2>) then
            buffer <- Some(BufferTools.WriteBuffer<double2>(context, queue, o, dim, mustInitBuffer))
        elif (t = typeof<double3>) then
            buffer <- Some(BufferTools.WriteBuffer<double3>(context, queue, o, dim, mustInitBuffer))
        elif (t = typeof<double4>) then
            buffer <- Some(BufferTools.WriteBuffer<double4>(context, queue, o, dim, mustInitBuffer))
        buffer

    static member ReadBuffer(t, context, queue, o, dim, buffer: ComputeMemory) =    
        if (t = typeof<uint32>) then
            BufferTools.ReadBuffer<uint32>(context, queue, o, dim, buffer :?> ComputeBuffer<uint32>) 
        elif (t = typeof<uint64>) then
            BufferTools.ReadBuffer<uint64>(context, queue, o, dim, buffer :?> ComputeBuffer<uint64>) 
        elif (t = typeof<int64>) then
            BufferTools.ReadBuffer<int64>(context, queue, o, dim, buffer :?> ComputeBuffer<int64>) 
        elif (t = typeof<int>) then
            BufferTools.ReadBuffer<int>(context, queue, o, dim, buffer :?> ComputeBuffer<int>) 
        elif (t = typeof<double>) then
            BufferTools.ReadBuffer<double>(context, queue, o, dim, buffer :?> ComputeBuffer<double>) 
        elif (t = typeof<float32>) then
            BufferTools.ReadBuffer<float32>(context, queue, o, dim, buffer :?> ComputeBuffer<float32>) 
        elif (t = typeof<bool>) then
            BufferTools.ReadBuffer<bool>(context, queue, o, dim, buffer :?> ComputeBuffer<bool>)
            
        elif (t = typeof<float2>) then
            BufferTools.ReadBuffer<float2>(context, queue, o, dim, buffer :?> ComputeBuffer<float2>) 
        elif (t = typeof<float3>) then
            BufferTools.ReadBuffer<float3>(context, queue, o, dim, buffer :?> ComputeBuffer<float3>) 
        elif (t = typeof<float4>) then
            BufferTools.ReadBuffer<float4>(context, queue, o, dim, buffer :?> ComputeBuffer<float4>)
            
        elif (t = typeof<int2>) then
            BufferTools.ReadBuffer<int2>(context, queue, o, dim, buffer :?> ComputeBuffer<int2>) 
        elif (t = typeof<int3>) then
            BufferTools.ReadBuffer<int3>(context, queue, o, dim, buffer :?> ComputeBuffer<int3>) 
        elif (t = typeof<int4>) then
            BufferTools.ReadBuffer<int4>(context, queue, o, dim, buffer :?> ComputeBuffer<int4>) 
            
        elif (t = typeof<double2>) then
            BufferTools.ReadBuffer<double2>(context, queue, o, dim, buffer :?> ComputeBuffer<double2>) 
        elif (t = typeof<double3>) then
            BufferTools.ReadBuffer<double3>(context, queue, o, dim, buffer :?> ComputeBuffer<double3>) 
        elif (t = typeof<double4>) then
            BufferTools.ReadBuffer<double4>(context, queue, o, dim, buffer :?> ComputeBuffer<double4>) 
            
    static member CopyBuffer(t, context, queue, input:ComputeMemory) =
        let t = input.GetType().GetGenericArguments().[0]        
        let mutable buffer = None
        if (t = typeof<uint32>) then
            buffer <- Some(BufferTools.CopyBuffer<uint32>(context, queue, input :?> ComputeBuffer<uint32>))
        elif (t = typeof<uint64>) then
            buffer <- Some(BufferTools.CopyBuffer<uint64>(context, queue, input :?> ComputeBuffer<uint64>))
        elif (t = typeof<int64>) then
            buffer <- Some(BufferTools.CopyBuffer<int64>(context, queue, input :?> ComputeBuffer<int64>))
        elif (t = typeof<int>) then
            buffer <- Some(BufferTools.CopyBuffer<int>(context, queue, input :?> ComputeBuffer<int>))
        elif (t = typeof<double>) then
            buffer <- Some(BufferTools.CopyBuffer<double>(context, queue, input :?> ComputeBuffer<double>))
        elif (t = typeof<float32>) then
            buffer <- Some(BufferTools.CopyBuffer<float32>(context, queue, input :?> ComputeBuffer<float32>))
        elif (t = typeof<bool>) then
            buffer <- Some(BufferTools.CopyBuffer<bool>(context, queue, input :?> ComputeBuffer<bool>))

        elif (t = typeof<float2>) then
            buffer <- Some(BufferTools.CopyBuffer<float2>(context, queue, input :?> ComputeBuffer<float2>))
        elif (t = typeof<float3>) then
            buffer <- Some(BufferTools.CopyBuffer<float3>(context, queue, input :?> ComputeBuffer<float3>))
        elif (t = typeof<float4>) then
            buffer <- Some(BufferTools.CopyBuffer<float4>(context, queue, input :?> ComputeBuffer<float4>))
            
        elif (t = typeof<int2>) then
            buffer <- Some(BufferTools.CopyBuffer<int2>(context, queue, input :?> ComputeBuffer<int2>))
        elif (t = typeof<int3>) then
            buffer <- Some(BufferTools.CopyBuffer<int3>(context, queue, input :?> ComputeBuffer<int3>))
        elif (t = typeof<int4>) then
            buffer <- Some(BufferTools.CopyBuffer<int4>(context, queue, input :?> ComputeBuffer<int4>))
            
        elif (t = typeof<double2>) then
            buffer <- Some(BufferTools.CopyBuffer<double2>(context, queue, input :?> ComputeBuffer<double2>))
        elif (t = typeof<double3>) then
            buffer <- Some(BufferTools.CopyBuffer<double3>(context, queue, input :?> ComputeBuffer<double3>))
        elif (t = typeof<double4>) then
            buffer <- Some(BufferTools.CopyBuffer<double4>(context, queue, input :?> ComputeBuffer<double4>))
        buffer