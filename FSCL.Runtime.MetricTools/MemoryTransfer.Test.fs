namespace FSCL.Runtime.MetricTools
    
open Cloo
open System
open Microsoft.FSharp.Reflection
open System.Runtime.InteropServices
open System.Collections.Generic

 // Memory transfer public functions
 module MemoryTransferTest =
    let TestTransfer(device: ComputeDevice, srcInfo: TransferEndpoint, dstInfo: TransferEndpoint, duration, currSize, validate) =    
        // Setup CL
        let computePlatform = device.Platform;
        let contextProperties = new ComputeContextPropertyList(computePlatform)
        let devices = new System.Collections.Generic.List<ComputeDevice>();
        devices.Add(device)
        let computeContext = new ComputeContext(devices, contextProperties, null, System.IntPtr.Zero);
        let computeQueue = new ComputeCommandQueue(computeContext, device, ComputeCommandQueueFlags.OutOfOrderExecution)
            
        // Allocate and init src, allocate dst
        let srcPtr = ref None
        let dstPtr = ref None
        let srcBuffer = ref None
        let dstBuffer = ref None
        if srcInfo.IsHostPtr then
            srcPtr := Some(TransferTools.AllocateHostPtr(currSize))
            do TransferTools.InitializeHostPtr(currSize, (!srcPtr).Value)
        else
            srcBuffer := Some(TransferTools.AllocateBuffer(computeContext, currSize, srcInfo))
            do TransferTools.InitializeBuffer(computeQueue, currSize, srcInfo, (!srcBuffer).Value)
        if dstInfo.IsHostPtr then
            dstPtr := Some(TransferTools.AllocateHostPtr(currSize))
        else
            dstBuffer := Some(TransferTools.AllocateBuffer(computeContext, currSize, dstInfo))

        // Run test
        let (_, time, iterations) = Tools.ExcuteFor(duration) (fun () -> "OK") (fun () -> ()) (fun () ->
            if srcInfo.IsHostPtr then
                if dstInfo.IsHostPtr then
                    TransferTools.HostPtrToHostPtr(currSize, validate, (!srcPtr).Value, (!dstPtr).Value)
                else
                    TransferTools.HostPtrToBuffer(computeContext, computeQueue, currSize, validate, dstInfo, (!srcPtr).Value, (!dstBuffer).Value)
            elif dstInfo.IsHostPtr then
                TransferTools.BufferToHostPtr(computeContext, computeQueue, currSize, validate, srcInfo, (!srcBuffer).Value, (!dstPtr).Value)  
            else  
                TransferTools.BufferToBuffer(computeContext, computeQueue, currSize, validate, srcInfo, dstInfo, (!srcBuffer).Value, (!dstBuffer).Value))
        
        (time, iterations * currSize)

    let TestTransfers(device: ComputeDevice, srcAccess: ComputeMemoryFlags, dstAccess: ComputeMemoryFlags, duration, currSize, validate) =   
        let results = new Dictionary<bool, Dictionary<bool, Dictionary<bool, Dictionary<bool, Dictionary<ComputeMemoryFlags, Dictionary<ComputeMemoryFlags, (int64 * int)>>>>>>()
        
        for isSrcHostPtr in [true; false] do
            for isDstHostPtr in [true; false] do
                for shouldSrcMap in [true; false] do
                    for shouldDstMap in [true; false] do
                        for srcFlags in [ComputeMemoryFlags.None; ComputeMemoryFlags.AllocateHostPointer] do
                            for dstFlags in [ComputeMemoryFlags.None; ComputeMemoryFlags.AllocateHostPointer] do
                                let srcInfo = new TransferEndpoint()
                                let dstInfo = new TransferEndpoint()
                                srcInfo.Flags <- srcAccess ||| srcFlags
                                srcInfo.IsHostPtr <- isSrcHostPtr
                                srcInfo.ShouldMap <- shouldSrcMap
                                dstInfo.Flags <- dstAccess ||| dstFlags
                                dstInfo.IsHostPtr <- isDstHostPtr
                                dstInfo.ShouldMap <- shouldDstMap

                                if not (srcInfo.Equals(dstInfo)) then
                                    let test =  TestTransfer(device, srcInfo, dstInfo, duration, currSize, validate)
                                    results.[isSrcHostPtr].[isDstHostPtr].[shouldSrcMap].[shouldDstMap].[srcFlags].[dstFlags] <- test
        results
        
    let GetBestTransferStrategy(device: ComputeDevice, srcAccess: ComputeMemoryFlags, dstAccess: ComputeMemoryFlags, duration, currSize, validate) =   
        let mutable min = 0.0
        let bestSrc = new TransferEndpoint()
        let bestDst = new TransferEndpoint()

        // Run all tests
        let results = TestTransfers(device, srcAccess, dstAccess, duration, currSize, validate)        
        for a in results do
            for b in a.Value do
                for c in b.Value do
                    for d in c.Value do
                        for e in d.Value do
                            for f in e.Value do
                                match f.Value with
                                | time, bytes ->
                                    let timePerByte = (double)time / (double)bytes
                                    if min > timePerByte then
                                       min <- timePerByte 
                                       bestSrc.Flags <- e.Key
                                       bestSrc.IsHostPtr <- a.Key
                                       bestSrc.ShouldMap <- c.Key
                                       bestDst.Flags <- f.Key
                                       bestDst.IsHostPtr <- b.Key
                                       bestDst.ShouldMap <- d.Key
        (bestSrc, bestDst)
