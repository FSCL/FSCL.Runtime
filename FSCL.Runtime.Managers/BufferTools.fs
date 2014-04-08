namespace FSCL.Runtime.Managers

open Cloo
open System
open FSCL.Compiler
open System.Runtime.InteropServices
open FSCL.Compiler.Configuration
        
type BufferTools() =  
    static member AccessModeToFlags(mode: AccessMode) =
        match mode with
        | AccessMode.ReadAccess ->
            ComputeMemoryFlags.ReadWrite
        | AccessMode.WriteAccess ->
            ComputeMemoryFlags.ReadWrite
        | _ ->
            ComputeMemoryFlags.ReadWrite

    static member private WriteBuffer<'T when 'T: struct and 'T : (new : unit -> 'T) and 'T :> System.ValueType>(q:ComputeCommandQueue, buff:ComputeBuffer<'T>, arg:obj, ?count:int[]) =
        //let dims = FSCL.Util.GetArrayDimensions(arg.Type)
        match buff.Count.Length with
        | 1 ->
            let actualArg = arg :?> 'T[]
            if count.IsNone then
                q.WriteToBuffer<'T>(actualArg, buff, false, null)
            else
                q.WriteToBuffer<'T>(actualArg, buff, false, 0L, 0L, count.Value.[0] * Marshal.SizeOf(typeof<'T>) |> int64, null)
        | 2 ->
            let actualArg = arg :?> 'T[,]
            let offset = Cloo.SysIntX2(0, 0)                
            let region = 
                if count.IsSome then
                    Cloo.SysIntX2(count.Value.[0], count.Value.[1])
                else
                    Cloo.SysIntX2(actualArg.GetLength(0), actualArg.GetLength(1))
            q.WriteToBuffer<'T>(actualArg, buff, false, offset, offset, region, null)
        | _ ->
            let actualArg = arg :?> 'T[,,]
            let offset = Cloo.SysIntX3(0, 0, 0)
            let region = 
                if count.IsSome then
                    Cloo.SysIntX3(count.Value.[0], count.Value.[1], count.Value.[2])
                else
                    Cloo.SysIntX3(actualArg.GetLength(0), actualArg.GetLength(1), actualArg.GetLength(2))
            q.WriteToBuffer<'T>(actualArg, buff, false, offset, offset, region, null)
            
    static member private ReadBuffer<'T when 'T: struct and 'T : (new : unit -> 'T) and 'T :> System.ValueType>(q:ComputeCommandQueue, arg:obj, buffer: ComputeBuffer<'T>, ?count:int[]) =
        match buffer.Count.Length with
        | 1 ->
            let actualArg = arg :?> 'T[]
            if count.IsNone then
                q.ReadFromBuffer<'T>(buffer, ref actualArg, true, 0L, 0L, actualArg.LongLength, null)           
            else
                q.ReadFromBuffer<'T>(buffer, ref actualArg, true, 0L, 0L, count.Value.[0] |> int64, null)
        | 2 ->
            let actualArg = arg :?> 'T[,]
            let offset = Cloo.SysIntX2(0,0)
            let region = 
                if count.IsSome then
                    Cloo.SysIntX2(count.Value.[0], count.Value.[1])
                else
                    Cloo.SysIntX2(actualArg.GetLength(0), actualArg.GetLength(1))
            //q.ReadFromBuffer<'T>(buffer, ref actualArg, true, null); 
            q.ReadFromBuffer<'T>(buffer, 
                ref actualArg, 
                true, 
                offset, offset, region, 
                (actualArg.GetLength(0) * Marshal.SizeOf(typeof<'T>)) |> int64, (actualArg.GetLength(0) *Marshal.SizeOf(typeof<'T>)) |> int64, null)
        | _ ->
            let actualArg = arg :?> 'T[,,]
            let offset = Cloo.SysIntX3(0,0,0)
            let region = 
                if count.IsSome then
                    Cloo.SysIntX3(count.Value.[0], count.Value.[1], count.Value.[2])
                else
                    Cloo.SysIntX3(actualArg.GetLength(0), actualArg.GetLength(1), actualArg.GetLength(2))
            q.ReadFromBuffer<'T>(buffer, ref actualArg, true, offset, offset, region, null)
            
    static member private CopyBuffer<'T when 'T: struct and 'T : (new : unit -> 'T) and 'T :> System.ValueType>(q:ComputeCommandQueue, input:ComputeBuffer<'T>, output:ComputeBuffer<'T>) =
        //let dims = FSCL.Util.GetArrayDimensions(arg.Type)
        q.CopyBuffer<'T>(input, output, 0L, 0L, input.TotalCount, null)
        
    static member CreateBuffer(t:Type, 
                               count:int64[],
                               c:ComputeContext, 
                               q:ComputeCommandQueue, 
                               flags:ComputeMemoryFlags) =
        let bufferType = typeof<ComputeBuffer<int>>.GetGenericTypeDefinition().MakeGenericType([| t |])
        let buffer = bufferType.GetConstructor([| typeof<ComputeContext>; typeof<ComputeMemoryFlags>; typeof<int64[]> |]).Invoke([| c; flags; count |])
        buffer :?> ComputeMemory

    static member WriteBuffer(t, queue, buff:ComputeMemory, o, ?count:int[]) =
        let m = typeof<BufferTools>.GetMethod("WriteBuffer", Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static)
        m.GetGenericMethodDefinition().MakeGenericMethod([| t |]).Invoke(null, [| queue; buff; o; count |]) |> ignore
            
        (*
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
        *)  

    static member ReadBuffer(t, queue, o, buffer: ComputeMemory, ?count:int[]) =    
        let m = typeof<BufferTools>.GetMethod("ReadBuffer", Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static)
        m.GetGenericMethodDefinition().MakeGenericMethod([| t |]).Invoke(null, [| queue; o; buffer; count |]) |> ignore
        (*
        if (t = typeof<uint32>) then
            BufferTools.ReadBuffer<uint32>(queue, o, buffer :?> ComputeBuffer<uint32>, count) 
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
            *)
    static member CopyBuffer(queue, input:ComputeMemory, output:ComputeMemory) =
        let t = input.GetType().GetGenericArguments().[0]    
        
        let m = typeof<BufferTools>.GetMethod("CopyBuffer", Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Static)
        m.GetGenericMethodDefinition().MakeGenericMethod([| t |]).Invoke(null, [| queue; input; output |]) |> ignore
        (*    
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
        *)