namespace FSCL.Runtime.MetricTools
    
open OpenCL
open System
open Microsoft.FSharp.Reflection
open System.Runtime.InteropServices
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Quotations

type TransferException(msg) =
    inherit System.Exception(msg)
    
type AccessException(msg) =
    inherit System.Exception(msg)

type BufferAccess =
| READ_ONLY
| WRITE_ONLY
| READ_WRITE
| NO_ACCESS

type TransferEndpoint() = 
    let isHostPtr = true
    let flags = OpenCLMemoryFlags.None
    let shouldMap = false

    member val IsHostPtr = isHostPtr with get, set
    member val Flags = flags with get, set
    member val ShouldMap = shouldMap with get, set

    override this.Equals(endpoint: obj) =
        if endpoint.GetType() <> typeof<TransferEndpoint> then
            false
        else
            let endp = endpoint :?> TransferEndpoint
            (this.IsHostPtr = endp.IsHostPtr) && (this.Flags = endp.Flags) && (this.ShouldMap = endp.ShouldMap)

module MemoryUtil =
    [<DllImport("kernel32.dll")>]
    extern void RtlMoveMemory(IntPtr dest, IntPtr src, uint32 len);
    [<DllImport("msvcrl.dll")>]
    extern int memcmp(IntPtr ptr1, IntPtr ptr2, int count);
    
module TransferTools =
    let AllocateHostPtr(currSize) =
        ref (Array.zeroCreate<float32>(currSize / sizeof<float32>))
            
    let AllocateBuffer(computeContext, currSize, info:TransferEndpoint) =
        ref (new OpenCLBuffer(computeContext, info.Flags, typeof<float32>, [| (int64)(currSize / sizeof<float32>) |]))

    let InitializeHostPtr(currSize, source: float32[] ref) =
        // Init source
        Array.fill (!source) 0 ((!source).Length) 2.0f
        
    let InitializeBuffer(computeQueue:OpenCLCommandQueue, currSize, info:TransferEndpoint, src:OpenCLBuffer ref) =
        let initializer = Array.create<float32> (currSize / sizeof<float32>) 2.0f
        computeQueue.WriteToBuffer(initializer, !src, true, null, null)

    // Test copy from host ptr to host ptr
    let HostPtrToHostPtr(currSize, validate, src: float32[] ref, dst: float32[] ref) =         
        // From array to array, via memcpy, including allocation and initialization
        dst := Array.copy(!src)   

    // Test copy from host ptr to buffer
    let HostPtrToBuffer(computeContext:OpenCLContext, computeQueue:OpenCLCommandQueue, currSize, validate, dstInfo:TransferEndpoint, src: float32[] ref, dst: OpenCLBuffer ref) =
        if dstInfo.ShouldMap then
            // From array to buffer, via map and memcpy, including allocation and initialization
            let sourceHandle = GCHandle.Alloc(!src, GCHandleType.Pinned)
            try 
                let sourcePtr = sourceHandle.AddrOfPinnedObject()
                let destPtrFava = computeQueue.Map(!dst, true, OpenCLMemoryMappingFlags.Write, (int64)0, (int64)(currSize / sizeof<float32>), null, null)
                MemoryUtil.RtlMoveMemory(sourcePtr, destPtrFava, (uint32)currSize)                                   
                computeQueue.Unmap(!dst, ref destPtrFava, null, null)                             
            finally
                if (sourceHandle.IsAllocated) then
                    sourceHandle.Free()
        else
            // From array to buffer, via writeBuffer, including allocation and initialization
            computeQueue.WriteToBuffer(!src, !dst, true, null, null)
                        
        // Validate            
        if validate then
            let finalizer = Array.zeroCreate<float32>(currSize / sizeof<float32>)
            computeQueue.ReadFromBuffer(!dst, ref (finalizer :> Array), true, null, null)
            Array.iteri (fun i element -> 
                if element <> finalizer.[i] then
                    raise (new TransferException("Source and destination do not match"))) !src   

    // Test copy from buffer to host ptr
    let BufferToHostPtr<'T>(computeContext:OpenCLContext, computeQueue:OpenCLCommandQueue, currSize, validate, srcInfo:TransferEndpoint, src: OpenCLBuffer ref, dst: Array ref) =
        if srcInfo.ShouldMap then
            // From buffer to array, via map and memcpy, including allocation
            let destHandle = GCHandle.Alloc(dst, GCHandleType.Pinned)
            try 
                let destPtr = destHandle.AddrOfPinnedObject()
                let sourcePtr = computeQueue.Map(!src, true, OpenCLMemoryMappingFlags.Read, (int64)0, (int64)(currSize / sizeof<float32>), null, null)
                MemoryUtil.RtlMoveMemory(sourcePtr, destPtr, (uint32)currSize) 
                computeQueue.Unmap(!src, ref sourcePtr, null, null)                                                                 
            finally
                if (destHandle.IsAllocated) then
                    destHandle.Free() 
        else
            computeQueue.ReadFromBuffer(!src, dst, true, null, null)

        // Validate
        if validate then
            Array.iteri (fun i element -> 
                if element <> 2.0f then
                    raise (new TransferException("Source and destination do not match"))) (!dst :?> float32[])                                   
                
    // Test copy from buffer to buffer
    let BufferToBuffer<'T>(computeContext:OpenCLContext, computeQueue:OpenCLCommandQueue, currSize, validate, srcInfo:TransferEndpoint, dstInfo:TransferEndpoint, src: OpenCLBuffer ref, dst: OpenCLBuffer ref) =
        if srcInfo.ShouldMap && dstInfo.ShouldMap then
            // From buffer to buffer, via map and memcpy, including allocation
            let destPtr = computeQueue.Map(!dst, true, OpenCLMemoryMappingFlags.Write, (int64)0, (int64)(currSize / sizeof<float32>), null, null)
            let sourcePtr = computeQueue.Map(!src, true, OpenCLMemoryMappingFlags.Read, (int64)0, (int64)(currSize / sizeof<float32>), null, null)
            MemoryUtil.RtlMoveMemory(sourcePtr, destPtr, (uint32)currSize) 
            computeQueue.Unmap(!dst, ref destPtr, null, null)            
            computeQueue.Unmap(!src, ref sourcePtr, null, null)    
        elif srcInfo.ShouldMap && (not dstInfo.ShouldMap) then
            // From buffer to buffer, via writeBuffer, including allocation
            let sourcePtr = computeQueue.Map(!src, true, OpenCLMemoryMappingFlags.Read, (int64)0, (int64)(currSize / sizeof<float32>), null, null)
            computeQueue.Write(!dst, true, 0L, (int64)(currSize / sizeof<float32>), sourcePtr, null, null)
            computeQueue.Unmap(!src, ref sourcePtr, null, null)  
        elif (not srcInfo.ShouldMap) && dstInfo.ShouldMap then
            // From buffer to buffer, via readBuffer, including allocation
            let destPtr = computeQueue.Map(!dst, true, OpenCLMemoryMappingFlags.Write, (int64)0, (int64)(currSize / sizeof<float32>), null, null)
            computeQueue.Read(!src, true, 0L, (int64)(currSize / sizeof<float32>), destPtr, null, null)
            computeQueue.Unmap(!src, ref destPtr, null, null)
        else                                                    
            // From buffer to buffer, via buffer copy, including allocation
            computeQueue.CopyBuffer(!src, !dst, null, null)  
            
        // Validate
        if validate then
            let finalizer = Array.zeroCreate<float32>(currSize / sizeof<float32>)
            computeQueue.ReadFromBuffer(!dst, ref (finalizer :> Array), true, null, null)
            Array.iteri (fun i element -> 
                if element <> 2.0f then
                    raise (new TransferException("Source and destination do not match"))) finalizer       
 