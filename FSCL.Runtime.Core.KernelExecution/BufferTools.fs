namespace FSCL.Runtime.KernelExecution

open Cloo
open System
open FSCL.Compiler
open System.Runtime.InteropServices
open FSCL.Compiler.Configuration
        
type BufferTools() =  
    static member KernelParameterAccessModeToFlags(mode: KernelParameterAccessMode) =
        match mode with
        | KernelParameterAccessMode.ReadAccess ->
            ComputeMemoryFlags.ReadOnly
        | KernelParameterAccessMode.WriteAccess ->
            ComputeMemoryFlags.WriteOnly
        | _ ->
            ComputeMemoryFlags.ReadWrite

    static member private WriteBuffer<'T when 'T: struct and 'T : (new : unit -> 'T) and 'T :> System.ValueType>(q:ComputeCommandQueue, buff:ComputeBuffer<'T>, arg:obj) =
        //let dims = FSCL.Util.GetArrayDimensions(arg.Type)
        match buff.Count.Length with
        | 1 ->
            let actualArg = arg :?> 'T[]
            q.WriteToBuffer<'T>(actualArg, buff, false, null)
        | 2 ->
            let actualArg = arg :?> 'T[,]
            let offset = Cloo.SysIntX2(0, 0)                
            let region = Cloo.SysIntX2(actualArg.GetLength(0), actualArg.GetLength(1))
            q.WriteToBuffer<'T>(actualArg, buff, false, offset, offset, region, null)
        | _ ->
            let actualArg = arg :?> 'T[,,]
            let offset = Cloo.SysIntX3(0, 0, 0)
            let region = Cloo.SysIntX3(actualArg.GetLength(0), actualArg.GetLength(1), actualArg.GetLength(2)) 
            q.WriteToBuffer<'T>(actualArg, buff, false, offset, offset, region, null)
            
    static member private ReadBuffer<'T when 'T: struct and 'T : (new : unit -> 'T) and 'T :> System.ValueType>(q:ComputeCommandQueue, arg:obj, buffer: ComputeBuffer<'T>) =
        match buffer.Count.Length with
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
            
    static member private CopyBuffer<'T when 'T: struct and 'T : (new : unit -> 'T) and 'T :> System.ValueType>(q:ComputeCommandQueue, input:ComputeBuffer<'T>, output:ComputeBuffer<'T>) =
        //let dims = FSCL.Util.GetArrayDimensions(arg.Type)
        q.CopyBuffer<'T>(input, output, 0L, 0L, input.TotalCount, null)
        
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

    static member WriteBuffer(t, queue, buff:ComputeMemory, o) =
        if (t = typeof<uint32>) then
            BufferTools.WriteBuffer<uint32>(queue, buff :?> ComputeBuffer<uint32>, o)
        elif (t = typeof<uint64>) then
            BufferTools.WriteBuffer<uint64>(queue, buff :?> ComputeBuffer<uint64>, o)
        elif (t = typeof<int64>) then
            BufferTools.WriteBuffer<int64>(queue, buff :?> ComputeBuffer<int64>, o)
        elif (t = typeof<int>) then
            BufferTools.WriteBuffer<int>(queue, buff :?> ComputeBuffer<int>, o)
        elif (t = typeof<double>) then
            BufferTools.WriteBuffer<double>(queue, buff :?> ComputeBuffer<double>, o)
        elif (t = typeof<float32>) then
            BufferTools.WriteBuffer<float32>(queue, buff :?> ComputeBuffer<float32>, o)
        elif (t = typeof<bool>) then
            BufferTools.WriteBuffer<bool>(queue, buff :?> ComputeBuffer<bool>, o)

        elif (t = typeof<float2>) then
            BufferTools.WriteBuffer<float2>(queue, buff :?> ComputeBuffer<float2>, o)
        elif (t = typeof<float3>) then
            BufferTools.WriteBuffer<float3>(queue, buff :?> ComputeBuffer<float3>, o)
        elif (t = typeof<float4>) then
            BufferTools.WriteBuffer<float4>(queue, buff :?> ComputeBuffer<float4>, o)
            
        elif (t = typeof<int2>) then
            BufferTools.WriteBuffer<int2>(queue, buff :?> ComputeBuffer<int2>, o)
        elif (t = typeof<int3>) then
            BufferTools.WriteBuffer<int3>(queue, buff :?> ComputeBuffer<int3>, o)
        elif (t = typeof<int4>) then
            BufferTools.WriteBuffer<int4>(queue, buff :?> ComputeBuffer<int4>, o)
            
        elif (t = typeof<double2>) then
            BufferTools.WriteBuffer<double2>(queue, buff :?> ComputeBuffer<double2>, o)
        elif (t = typeof<double3>) then
            BufferTools.WriteBuffer<double3>(queue, buff :?> ComputeBuffer<double3>, o)
        elif (t = typeof<double4>) then
            BufferTools.WriteBuffer<double4>(queue, buff :?> ComputeBuffer<double4>, o)        

    static member ReadBuffer(t, queue, o, buffer: ComputeMemory) =    
        if (t = typeof<uint32>) then
            BufferTools.ReadBuffer<uint32>(queue, o, buffer :?> ComputeBuffer<uint32>) 
        elif (t = typeof<uint64>) then
            BufferTools.ReadBuffer<uint64>(queue, o, buffer :?> ComputeBuffer<uint64>) 
        elif (t = typeof<int64>) then
            BufferTools.ReadBuffer<int64>(queue, o, buffer :?> ComputeBuffer<int64>) 
        elif (t = typeof<int>) then
            BufferTools.ReadBuffer<int>(queue, o, buffer :?> ComputeBuffer<int>) 
        elif (t = typeof<double>) then
            BufferTools.ReadBuffer<double>(queue, o, buffer :?> ComputeBuffer<double>) 
        elif (t = typeof<float32>) then
            BufferTools.ReadBuffer<float32>(queue, o, buffer :?> ComputeBuffer<float32>) 
        elif (t = typeof<bool>) then
            BufferTools.ReadBuffer<bool>(queue, o, buffer :?> ComputeBuffer<bool>)
            
        elif (t = typeof<float2>) then
            BufferTools.ReadBuffer<float2>(queue, o, buffer :?> ComputeBuffer<float2>) 
        elif (t = typeof<float3>) then
            BufferTools.ReadBuffer<float3>(queue, o, buffer :?> ComputeBuffer<float3>) 
        elif (t = typeof<float4>) then
            BufferTools.ReadBuffer<float4>(queue, o, buffer :?> ComputeBuffer<float4>)
            
        elif (t = typeof<int2>) then
            BufferTools.ReadBuffer<int2>(queue, o, buffer :?> ComputeBuffer<int2>) 
        elif (t = typeof<int3>) then
            BufferTools.ReadBuffer<int3>(queue, o, buffer :?> ComputeBuffer<int3>) 
        elif (t = typeof<int4>) then
            BufferTools.ReadBuffer<int4>(queue, o, buffer :?> ComputeBuffer<int4>) 
            
        elif (t = typeof<double2>) then
            BufferTools.ReadBuffer<double2>(queue, o, buffer :?> ComputeBuffer<double2>) 
        elif (t = typeof<double3>) then
            BufferTools.ReadBuffer<double3>(queue, o, buffer :?> ComputeBuffer<double3>) 
        elif (t = typeof<double4>) then
            BufferTools.ReadBuffer<double4>(queue, o, buffer :?> ComputeBuffer<double4>) 
            
    static member CopyBuffer(queue, input:ComputeMemory, output:ComputeMemory) =
        let t = input.GetType().GetGenericArguments().[0]        
        let mutable buffer = None
        if (t = typeof<uint32>) then
            BufferTools.CopyBuffer<uint32>(queue, input :?> ComputeBuffer<uint32>, output :?> ComputeBuffer<uint32>)
        elif (t = typeof<uint64>) then
            BufferTools.CopyBuffer<uint64>(queue, input :?> ComputeBuffer<uint64>, output :?> ComputeBuffer<uint64>)
        elif (t = typeof<int64>) then
            BufferTools.CopyBuffer<int64>(queue, input :?> ComputeBuffer<int64>, output :?> ComputeBuffer<int64>)
        elif (t = typeof<int>) then
            BufferTools.CopyBuffer<int>(queue, input :?> ComputeBuffer<int>, output :?> ComputeBuffer<int>)
        elif (t = typeof<double>) then
            BufferTools.CopyBuffer<double>(queue, input :?> ComputeBuffer<double>, output :?> ComputeBuffer<double>)
        elif (t = typeof<float32>) then
            BufferTools.CopyBuffer<float32>(queue, input :?> ComputeBuffer<float32>, output :?> ComputeBuffer<float32>)
        elif (t = typeof<bool>) then
            BufferTools.CopyBuffer<bool>(queue, input :?> ComputeBuffer<bool>, output :?> ComputeBuffer<bool>)

        elif (t = typeof<float2>) then
            BufferTools.CopyBuffer<float2>(queue, input :?> ComputeBuffer<float2>, output :?> ComputeBuffer<float2>)
        elif (t = typeof<float3>) then
            BufferTools.CopyBuffer<float3>(queue, input :?> ComputeBuffer<float3>, output :?> ComputeBuffer<float3>)
        elif (t = typeof<float4>) then
            BufferTools.CopyBuffer<float4>(queue, input :?> ComputeBuffer<float4>, output :?> ComputeBuffer<float4>)
            
        elif (t = typeof<int2>) then
            BufferTools.CopyBuffer<int2>(queue, input :?> ComputeBuffer<int2>, output :?> ComputeBuffer<int2>)
        elif (t = typeof<int3>) then
            BufferTools.CopyBuffer<int3>(queue, input :?> ComputeBuffer<int3>, output :?> ComputeBuffer<int3>)
        elif (t = typeof<int4>) then
            BufferTools.CopyBuffer<int4>(queue, input :?> ComputeBuffer<int4>, output :?> ComputeBuffer<int4>)
            
        elif (t = typeof<double2>) then
            BufferTools.CopyBuffer<double2>(queue, input :?> ComputeBuffer<double2>, output :?> ComputeBuffer<double2>)
        elif (t = typeof<double3>) then
            BufferTools.CopyBuffer<double3>(queue, input :?> ComputeBuffer<double3>, output :?> ComputeBuffer<double3>)
        elif (t = typeof<double4>) then
            BufferTools.CopyBuffer<double4>(queue, input :?> ComputeBuffer<double4>, output :?> ComputeBuffer<double4>)
        