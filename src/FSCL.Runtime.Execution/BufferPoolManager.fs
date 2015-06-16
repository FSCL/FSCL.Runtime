﻿namespace FSCL.Runtime.Managers

open OpenCL
open System.Collections.Generic
open System
open System.Runtime.InteropServices
open FSCL.Compiler
open FSCL
open FSCL.Language
open FSCL.Runtime
open System.Diagnostics
open Microsoft.FSharp.Reflection
open FSCL.Compiler.Util
open FSCL.Compiler.Util.ReflectionUtil

[<AllowNullLiteral>]
type HostSideDataHandle(data: Array) =
    let mutable (gcHandle:GCHandle option) = None
    let mutable (ptr:IntPtr) = IntPtr.Zero                           
    let mutable isDispose = false

    member val ManagedData = data 
        with get
        
    member val ElementType = data.GetType().GetElementType()
        with get

    member val Lenghts = ArrayUtil.GetArrayLengths(data) 
        with get

    member this.Ptr 
        with get() =
            assert(not isDispose)
            ptr
        and set v =
            assert(not isDispose)
            ptr <- v

    member this.HasUnmanagedStorage =
        gcHandle.IsNone && (ptr <> IntPtr.Zero)

    member this.BeforeTransferToDevice() =
        assert(not isDispose)
        if gcHandle = None && ptr = IntPtr.Zero then
            let dataType = data.GetType().GetElementType()
            if dataType.IsValueType then
                let dataPtr = GCHandle.Alloc(data, GCHandleType.Pinned)
                gcHandle <- Some(dataPtr)
                ptr <- gcHandle.Value.AddrOfPinnedObject()
            else
                // Try alloc and copy to unmanaged ptr
                if FSharpType.IsRecord(dataType) then
                    let size = Marshal.SizeOf(dataType)
                    let unmanagedPtr = Marshal.AllocHGlobal(size * data.Length)
                    MemoryUtil.memset(unmanagedPtr, 0, size * data.Length) |> ignore
                    let mutable currentPtr = unmanagedPtr
                    for i = 0 to data.Length - 1 do
                        if data.GetValue(i) <> null then
                            Marshal.StructureToPtr(data.GetValue(i), currentPtr, false)
                        currentPtr <- new IntPtr((int64)currentPtr + (int64)size)
                    ptr <- unmanagedPtr      
                else
                    let size = Marshal.SizeOf(dataType)
                    let unmanagedPtr = Marshal.AllocHGlobal(size * data.Length)
                    MemoryUtil.memset(unmanagedPtr, 0, size * data.Length) |> ignore
                    let mutable currentPtr = unmanagedPtr
                    for i = 0 to data.Length - 1 do
                        if data.GetValue(i) <> null then
                            Marshal.StructureToPtr(data.GetValue(i), currentPtr, false)
                        currentPtr <- new IntPtr((int64)currentPtr + (int64)size)
                    ptr <- unmanagedPtr 
    
    member this.BeforeTransferFromDevice() =
        assert(not isDispose)
        this.BeforeTransferToDevice()
        
    member this.SyncManagedWithUnmanaged() =
        assert(not isDispose)
        if gcHandle = None && ptr <> IntPtr.Zero then
            // Must sync
            let dataType = data.GetType().GetElementType()
            if FSharpType.IsRecord(dataType) then
                let size = Marshal.SizeOf(dataType)
                let mutable currentPtr = ptr
                for i = 0 to data.Length - 1 do
                    if (data.GetValue(i) <> null) then
                        Marshal.PtrToStructure(currentPtr, data.GetValue(i))
                    else
                        let recDefValues = FSharpType.GetRecordFields(dataType) |> Array.map(fun (p:Reflection.PropertyInfo) -> Activator.CreateInstance(p.PropertyType))
                        let defRec = FSharpValue.MakeRecord(dataType, recDefValues)
                        Marshal.PtrToStructure(currentPtr, defRec)
                        data.SetValue(defRec, [| i |])
                    currentPtr <- new IntPtr(((int64)currentPtr) + ((int64)size))
            else
                let size = Marshal.SizeOf(dataType)
                let mutable currentPtr = ptr
                for i = 0 to data.Length - 1 do
                    if (data.GetValue(i) <> null) then
                        Marshal.PtrToStructure(currentPtr, data.GetValue(i))
                    else
                        let defRec = Activator.CreateInstance(dataType)
                        Marshal.PtrToStructure(currentPtr, defRec)
                        data.SetValue(defRec, [| i |])
                    currentPtr <- new IntPtr(((int64)currentPtr) + ((int64)size))

    interface IDisposable with
        member this.Dispose() =
            assert(not isDispose)
            isDispose <- true
            if gcHandle.IsSome then
                gcHandle.Value.Free()
            else
                if ptr <> IntPtr.Zero then
                    Marshal.FreeHGlobal(ptr)

[<AllowNullLiteral>]
type BufferPoolItem(buffer: OpenCLBuffer, 
                    hostDataHandle: HostSideDataHandle option,
                    queue: OpenCLCommandQueue, 
                    analysis: AccessAnalysisResult,
                    flags: MemoryFlags,
                    space: AddressSpace, 
                    hostToDeviceTransfer: TransferMode, 
                    deviceToHostTransfer: TransferMode, 
                    rMode: BufferReadMode, 
                    wMode: BufferWriteMode, 
                    isReturn: bool) =
    member val AccessAnalysis = analysis with get
    member val WriteMode = wMode with get
    member val ReadMode = rMode with get 
    member val HostToDeviceTransferMode = hostToDeviceTransfer with get, set
    member val DeviceToHostTransferMode = deviceToHostTransfer with get, set
    member val AddressSpace = space with get
    member val IsReturned = isReturn with get
    member val Flags = flags with get
     
    member val Buffer = buffer with get
    member val IsAvailable = false with get, set
    member val Queue = queue with get

    member val HostDataHandle = hostDataHandle with get, set
               
// To create a new buffer pool manager from a previous one, so to reuse buffers created in a preceding expression
[<AllowNullLiteral>]
type BufferPoolManager(oldPool: BufferPoolManager) =
    let mutable noObjID = 0
    let locker = new Object()
    let trackedBufferPool = if oldPool <> null then oldPool.GetTrackedBufferPool() else Dictionary<Array, BufferPoolItem>()
    let untrackedBufferPool = if oldPool <> null then oldPool.GetUntrackedBufferPool() else Dictionary<OpenCLContext, Dictionary<OpenCLBuffer, BufferPoolItem>>()
    //let mutable rootReturnBuffer = None
    let reverseTrackedBufferPool = if oldPool <> null then oldPool.GetReverseTrackedBufferPool() else Dictionary<OpenCLBuffer, Array>()
    
    new() =
        new BufferPoolManager(null)        
    
    // To get buffers from an old pool item and merge them into a new one
    member internal this.GetTrackedBufferPool() =
        trackedBufferPool
    member internal this.GetUntrackedBufferPool() =
        untrackedBufferPool
    member internal this.GetReverseTrackedBufferPool() =
        reverseTrackedBufferPool
       
    member this.IsTracked(b:OpenCLBuffer) =
        reverseTrackedBufferPool.ContainsKey(b)             

    // Tracking buffer
    // This is the buffer is amtching an array argument
    member this.CreateTrackedBuffer(context: OpenCLContext,
                                    queue: OpenCLCommandQueue, 
                                    parameter: IFunctionParameter, 
                                    arr: Array, 
                                    isRoot: bool, 
                                    sharePriority: BufferSharePriority) =  
        ////Console.WriteLine("Request to create a TRACKED buffer")  

        // Get parameter meta
        let transferMode = parameter.Meta.Get<TransferModeAttribute>()
        let rMode = parameter.Meta.Get<BufferReadModeAttribute>()
        let wMode = parameter.Meta.Get<BufferWriteModeAttribute>()
        let addressSpace = parameter.Meta.Get<AddressSpaceAttribute>()
        let memoryFlags = parameter.Meta.Get<MemoryFlagsAttribute>()
        let mergedFlags, readMode, writeMode = 
            BufferStrategies.DetermineBestFlagsAndReadWriteMode(
                                                addressSpace.AddressSpace, 
                                                AccessAnalysisResult.ReadAccess ||| AccessAnalysisResult.WriteAccess,
                                                rMode.Mode,
                                                wMode.Mode,
                                                transferMode.HostToDeviceMode,
                                                transferMode.DeviceToHostMode,
                                                memoryFlags.Flags, 
                                                isRoot, 
                                                parameter.IsReturned,
                                                true,
                                                queue.Device)
        ////Console.WriteLine("Access analysis says this parameter is: " + parameter.AccessAnalysis.ToString())
        ////Console.WriteLine("Best Memory Flags: " + mergedFlags.ToString())
        ////Console.WriteLine("Best Read Mode: " + readMode.ToString())
        ////Console.WriteLine("Best Write Mode: " + writeMode.ToString())
        let i = 0
        // Check if there is a buffer bound to the same object
        lock locker (fun () -> 
                        if trackedBufferPool.ContainsKey(arr) then
                            // We are requested to create a buffer matching an object (parameter value) already known
                            let prevBuffer = trackedBufferPool.[arr]
                            // If not used anymore, same context and access we can use it
                            //assert (prevBuffer.IsAvailable)
                            let sameContext = prevBuffer.Buffer.Context = context
                            let sameDevice = true
                            let memFlagsCompatible = BufferStrategies.AreMemoryFlagsCompatible(prevBuffer.Flags, mergedFlags, sharePriority)
                            if sameDevice && sameContext && memFlagsCompatible then    
                                //Console.WriteLine("Buffer is reused with adapteded flags: " + prevBuffer.Flags.ToString())   
                           
                                // Update transfer mode
                                if (prevBuffer.HostToDeviceTransferMode &&& TransferMode.NoTransfer |> int > 0) && (transferMode.HostToDeviceMode &&& TransferMode.NoTransfer |> int = 0) then
                                    prevBuffer.HostToDeviceTransferMode <- prevBuffer.HostToDeviceTransferMode &&& ~~~TransferMode.NoTransfer
                                if (prevBuffer.DeviceToHostTransferMode &&& TransferMode.NoTransfer |> int > 0) && (transferMode.DeviceToHostMode &&& TransferMode.NoTransfer |> int = 0) then
                                    prevBuffer.DeviceToHostTransferMode <- prevBuffer.DeviceToHostTransferMode &&& ~~~TransferMode.NoTransfer
                
                                // If this is the return buffer for root kernel in a kernel expression, remember it cause we will need to read it somewhere at the end
                                //if parameter.IsReturned && isRoot then
                                  //  rootReturnBuffer <- Some(prevBuffer, arr)

                                // Use this buffer
                                prevBuffer.IsAvailable <- false
                                prevBuffer.Buffer
                            else
                                if sameContext then
                                    Trace.WriteLine("FSCL Warning: pre-existing tracked buffer cannot be reused for parameter " + parameter.Name + " cause memory flags are not compatible (pool buffer: " + prevBuffer.Flags.ToString() + ", this parameter: " + mergedFlags.ToString() + ", share priority: " + sharePriority.ToString() + ")") 
                                else
                                    Trace.WriteLine("FSCL Warning: pre-existing tracked buffer cannot be reused for parameter " + parameter.Name + " cause the OpenCL context is different") 
                                let shouldCopyBuffer = BufferStrategies.ShouldCopyBuffer(prevBuffer.AccessAnalysis, prevBuffer.AddressSpace, parameter.AccessAnalysis, addressSpace.AddressSpace)
                                //Console.WriteLine("No adapted flags can be computed, create new buffer")   
                                // Check if record array
                                let dataHandle = prevBuffer.HostDataHandle
                                let bufferHandle = BufferTools.CreateBuffer(dataHandle.Value.Ptr, arr.GetType().GetElementType(), ArrayUtil.GetArrayLengths(arr), context, queue, BufferStrategies.ToOpenCLMemoryFlags(mergedFlags))
             
                                // We must create a new buffer
                                let bufferItem = new BufferPoolItem(
                                                    bufferHandle,
                                                    dataHandle, 
                                                    queue, 
                                                    parameter.AccessAnalysis,
                                                    mergedFlags, 
                                                    addressSpace.AddressSpace, 
                                                    transferMode.HostToDeviceMode,
                                                    transferMode.DeviceToHostMode,
                                                    readMode,
                                                    writeMode,
                                                    parameter.IsReturned)
                                // We need to copy buffer only if is has been potentially changed the one we are compying from
                                if (shouldCopyBuffer) then
                                    if sameContext then
                                        BufferTools.CopyBuffer(queue, prevBuffer.Buffer, bufferItem.Buffer)
                                    else
                                        // Write managed array to new buffer
                                        BufferTools.WriteBuffer(queue, (writeMode = BufferWriteMode.MapBuffer), bufferItem.Buffer, prevBuffer.HostDataHandle.Value.Ptr, arr.GetType().GetElementType(), ArrayUtil.GetArrayLengths(arr))    
                                //else
                                    //Console.WriteLine("Buffer in NOT copied")   

                                // If this is the return buffer for root kernel in a kernel expression, remember it cause we will need to read it somewhere at the end
                                //if parameter.IsReturned && isRoot then
                                  //  rootReturnBuffer <- Some(bufferItem, arr)

                                // Dispose previous buffer and store new one
                                reverseTrackedBufferPool.Remove(prevBuffer.Buffer) |> ignore
                                prevBuffer.Buffer.Dispose()
                                trackedBufferPool.[arr] <- bufferItem          
                                reverseTrackedBufferPool.Add(bufferItem.Buffer, arr)
                                bufferItem.Buffer
                        else 
                            // Create a buffer tracking the parameter
                            let dataHandle = new HostSideDataHandle(arr)

                            // If the ptr to data is required to create the buffer, prepare it
                            if BufferStrategies.IsBufferRequiringHostPtr(BufferStrategies.ToOpenCLMemoryFlags(mergedFlags)) then
                                dataHandle.BeforeTransferToDevice()

                            let bufferHandle = BufferTools.CreateBuffer(dataHandle.Ptr, dataHandle.ManagedData.GetType().GetElementType(), ArrayUtil.GetArrayLengths(dataHandle.ManagedData), context, queue, BufferStrategies.ToOpenCLMemoryFlags(mergedFlags))
          
                            let bufferItem = new BufferPoolItem(
                                                    bufferHandle, 
                                                    Some(dataHandle),
                                                    queue,
                                                    parameter.AccessAnalysis,
                                                    mergedFlags,
                                                    addressSpace.AddressSpace, 
                                                    transferMode.HostToDeviceMode,
                                                    transferMode.DeviceToHostMode,                    
                                                    readMode,
                                                    writeMode,
                                                    parameter.IsReturned)   
                            // If we have to write the buffer explicitely to init it, do it         
                            if BufferStrategies.ShouldExplicitlyWriteToInitBuffer(parameter.AccessAnalysis, BufferStrategies.ToOpenCLMemoryFlags(mergedFlags), addressSpace.AddressSpace, transferMode.HostToDeviceMode) then
                                dataHandle.BeforeTransferToDevice()
                                BufferTools.WriteBuffer(queue, (writeMode = BufferWriteMode.MapBuffer), bufferItem.Buffer, dataHandle.Ptr, arr.GetType().GetElementType(), ArrayUtil.GetArrayLengths(arr))    
                              
                            // If this is the return buffer for root kernel in a kernel expression, remember it cause we will need to read it somewhere at the end
                            //if parameter.IsReturned && isRoot then
                              //  rootReturnBuffer <- Some(bufferItem, arr)

                            // Store buffer          
                            trackedBufferPool.Add(arr, bufferItem)
                            reverseTrackedBufferPool.Add(bufferItem.Buffer, arr)
                            bufferItem.Buffer)
            
    // Non-tracking buffer
    // This is the buffer created for buffers allocated and returned inside kernels and for buffer obtained from the execution of other previos kernels
    member this.CreateUntrackedBuffer(context: OpenCLContext,
                                      queue: OpenCLCommandQueue, 
                                      parameter: IFunctionParameter, 
                                      count: int64[], 
                                      isRoot: bool) =  
        //Console.WriteLine("Request to create an UNTRACKED buffer")
        // Get parameter meta
        let transferMode = parameter.Meta.Get<TransferModeAttribute>()
        let rMode = parameter.Meta.Get<BufferReadModeAttribute>()
        let wMode = parameter.Meta.Get<BufferWriteModeAttribute>()
        let addressSpace = parameter.Meta.Get<AddressSpaceAttribute>()
        let memoryFlags = parameter.Meta.Get<MemoryFlagsAttribute>()
        let mergedFlags, readMode, writeMode = 
            BufferStrategies.DetermineBestFlagsAndReadWriteMode(
                                addressSpace.AddressSpace, 
                                parameter.AccessAnalysis,
                                rMode.Mode,
                                wMode.Mode,
                                transferMode.HostToDeviceMode,
                                transferMode.DeviceToHostMode,
                                memoryFlags.Flags, 
                                isRoot, 
                                parameter.IsReturned,
                                false,
                                queue.Device)
                            
        //Console.WriteLine("Access analysis says this parameter is: " + parameter.AccessAnalysis.ToString())
        //Console.WriteLine("Best Memory Flags: " + mergedFlags.ToString())
        //Console.WriteLine("Best Read Mode: " + readMode.ToString())
        //Console.WriteLine("Best Write Mode: " + writeMode.ToString())
        
        lock locker (fun () -> 
                        if not (untrackedBufferPool.ContainsKey(context)) then
                            untrackedBufferPool.Add(context, new Dictionary<OpenCLBuffer, BufferPoolItem>())
                        // Check if any untracked buffers to use
                        let p = untrackedBufferPool.[context]
                        let mutable compBuffer = null
                        let mutable index = 0
                        let totalSize = (count |> Array.reduce(*)) * (Marshal.SizeOf(parameter.DataType.GetElementType()) |> int64)
                        let compBuffer = untrackedBufferPool.[context] |>  
                                         Seq.tryFind(fun keyVal ->
                                                        keyVal.Value.IsAvailable && keyVal.Value.Buffer.Size >= totalSize && keyVal.Value.Queue.Device = queue.Device)
                        if compBuffer.IsSome then
                            compBuffer.Value.Value.IsAvailable <- false
                            compBuffer.Value.Key
                        else                            
                            let bufferItem = new BufferPoolItem(
                                                BufferTools.CreateBuffer(parameter.DataType.GetElementType(), count, context, queue, BufferStrategies.ToOpenCLMemoryFlags(mergedFlags)), 
                                                None,
                                                queue,
                                                parameter.AccessAnalysis,
                                                mergedFlags,
                                                addressSpace.AddressSpace, 
                                                transferMode.HostToDeviceMode,
                                                transferMode.DeviceToHostMode,
                                                readMode,
                                                writeMode,
                                                parameter.IsReturned)
                            untrackedBufferPool.[context].Add(bufferItem.Buffer, bufferItem)
                            bufferItem.Buffer)
                        
    // This is to create buffers that are returned from a root kernel (so tracked) and bound to calls to subkernels
    member this.CreateTrackedBufferFromReturn(retBuffer:OpenCLBuffer,
                                              //newArr: Array,
                                              context: OpenCLContext, 
                                              queue: OpenCLCommandQueue, 
                                              parameter: IFunctionParameter, 
                                              isRoot: bool,
                                              sharePriority: BufferSharePriority) =
        //Console.WriteLine("Request to create a TRACKED buffer from RETURNED buffer")
        let transferMode = parameter.Meta.Get<TransferModeAttribute>()
        let rMode = parameter.Meta.Get<BufferReadModeAttribute>()
        let wMode = parameter.Meta.Get<BufferWriteModeAttribute>()
        let addressSpace = parameter.Meta.Get<AddressSpaceAttribute>()
        let memoryFlags = parameter.Meta.Get<MemoryFlagsAttribute>()
        let mergedFlags, readMode, writeMode = 
            BufferStrategies.DetermineBestFlagsAndReadWriteMode(
                                addressSpace.AddressSpace, 
                                parameter.AccessAnalysis,
                                rMode.Mode,
                                wMode.Mode,
                                transferMode.HostToDeviceMode,
                                transferMode.DeviceToHostMode,
                                memoryFlags.Flags, 
                                isRoot, 
                                parameter.IsReturned,
                                true,
                                queue.Device)
        //Console.WriteLine("Access analysis says this parameter is: " + parameter.AccessAnalysis.ToString())
        //Console.WriteLine("Best Memory Flags: " + mergedFlags.ToString())
        //Console.WriteLine("Best Read Mode: " + readMode.ToString())
        //Console.WriteLine("Best Write Mode: " + writeMode.ToString())
        lock locker (fun() ->
            // Tracked return
            if (reverseTrackedBufferPool.ContainsKey(retBuffer)) then
                this.CreateTrackedBuffer(context, queue, parameter, reverseTrackedBufferPool.[retBuffer], isRoot, sharePriority)        
            else
                // Return is untracked
                let retItem = untrackedBufferPool.[retBuffer.Context].[retBuffer]
                              
                // Check if this can be used with no copy
                let sameDevice = true
                let sameContext = context = retBuffer.Context
                let memFlagsCompatible = BufferStrategies.AreMemoryFlagsCompatible(retItem.Flags, mergedFlags, sharePriority)
                if sameDevice && sameContext && memFlagsCompatible then    
                    //Console.WriteLine("Returned buffer is promoted to TRACKED with adapteded flags: " + retItem.Flags.ToString())  
                    // Must promote this untracked item to tracked
                    this.PromoteUntrackedToTracked(retItem)
                    retBuffer
                else
                    if sameContext then
                        Trace.WriteLine("FSCL Warning: tracked buffer for parameter " + parameter.Name + " cannot reuse pre-existing returned buffer cause memory flags are not compatible (returned buffer: " + retItem.Flags.ToString() + ", this parameter: " + mergedFlags.ToString() + ", share priority: " + sharePriority.ToString()) 
                    else
                        Trace.WriteLine("FSCL Warning: tracked buffer for parameter " + parameter.Name + " cannot reuse pre-existing returned buffer cause the OpenCL context is different") 
                
                    //Console.WriteLine("No adapted flags can be computed, create new buffer")  
                    // Need to copy
                    let elementCount = retBuffer.Count
                    let elemType = parameter.DataType.GetElementType()
                    let newArr = Array.CreateInstance(elemType, elementCount)
                    let copy = this.CreateTrackedBuffer(context, queue, parameter, newArr, isRoot, sharePriority)
                    let bufferItem = new BufferPoolItem(
                                            copy, 
                                            Some(new HostSideDataHandle(newArr)),
                                            queue,
                                            parameter.AccessAnalysis,
                                            mergedFlags,
                                            addressSpace.AddressSpace, 
                                            transferMode.HostToDeviceMode,
                                            transferMode.DeviceToHostMode,                    
                                            readMode,
                                            writeMode,
                                            parameter.IsReturned) 
                    if BufferStrategies.ShouldCopyBuffer(retItem.AccessAnalysis, retItem.AddressSpace, parameter.AccessAnalysis, addressSpace.AddressSpace) then
                        //Console.WriteLine("Buffer is copied")   
                        if sameContext then
                            BufferTools.CopyBuffer(queue, retBuffer, copy)
                        else
                            let oldArr = this.ReadBuffer(retBuffer, false)
                            let oldHandle = new HostSideDataHandle(oldArr)
                            oldHandle.BeforeTransferToDevice()
                            BufferTools.WriteBuffer(bufferItem.Queue, (writeMode = BufferWriteMode.MapBuffer), bufferItem.Buffer, oldHandle.Ptr, oldArr.GetType().GetElementType(), ArrayUtil.GetArrayLengths(oldArr))                                
                            (oldHandle :> IDisposable).Dispose()
                    trackedBufferPool.Add(newArr, bufferItem)
                    //else
                        //Console.WriteLine("Buffer is NOT copied")   
                    // Dispose old buffer
                    retItem.IsAvailable <- false
                    this.EndUsingBuffer(retItem.Buffer)
                    copy)
             
    // This is called for buffers bound to subkernels and not returned or returned from non-root   
    member this.CreateUntrackedBufferFromReturn(retBuffer:OpenCLBuffer, 
                                                context: OpenCLContext, 
                                                queue: OpenCLCommandQueue, 
                                                parameter: IFunctionParameter, 
                                                isRoot: bool,
                                                sharePriority: BufferSharePriority) =
        //Console.WriteLine("Request to create an UNTRACKED buffer from RETURNED buffer")
        let transferMode = parameter.Meta.Get<TransferModeAttribute>()
        let rMode = parameter.Meta.Get<BufferReadModeAttribute>()
        let wMode = parameter.Meta.Get<BufferWriteModeAttribute>()
        let addressSpace = parameter.Meta.Get<AddressSpaceAttribute>()
        let memoryFlags = parameter.Meta.Get<MemoryFlagsAttribute>()
        let mergedFlags, readMode, writeMode = 
            BufferStrategies.DetermineBestFlagsAndReadWriteMode(
                                    addressSpace.AddressSpace, 
                                    parameter.AccessAnalysis,
                                    rMode.Mode,
                                    wMode.Mode,
                                    transferMode.HostToDeviceMode,
                                    transferMode.DeviceToHostMode,
                                    memoryFlags.Flags, 
                                    isRoot, 
                                    parameter.IsReturned,
                                    false,
                                    queue.Device)
        lock locker (fun() ->
            // Tracked return
            if (reverseTrackedBufferPool.ContainsKey(retBuffer)) then
                this.CreateTrackedBuffer(context, queue, parameter, reverseTrackedBufferPool.[retBuffer], isRoot, sharePriority)        
            else
                //Console.WriteLine("Returned buffer is UNTRACKED")
                // Return is untracked
                let retItem = untrackedBufferPool.[retBuffer.Context].[retBuffer]

                // Check if this can be used with no copy
                let sameDevice = true
                let sameContext = context = retBuffer.Context
                let memFlagsCompatible = BufferStrategies.AreMemoryFlagsCompatible(retItem.Flags, mergedFlags, sharePriority)
                if sameDevice && sameContext && memFlagsCompatible then
                    let temp = this.ReadBuffer(retBuffer, false)
                    //Console.WriteLine("Buffer is reused with adapteded flags: " + retItem.Flags.ToString()) 
                    retItem.IsAvailable <- false
                    retBuffer
                else
                    if sameContext then
                        Trace.WriteLine("FSCL Warning: untracked buffer for parameter " + parameter.Name + " cannot reuse pre-existing returned buffer cause memory flags are not compatible (returned buffer: " + retItem.Flags.ToString() + ", this parameter: " + mergedFlags.ToString() + ", share priority: " + sharePriority.ToString()) 
                    else
                        Trace.WriteLine("FSCL Warning: untracked buffer for parameter " + parameter.Name + " cannot reuse pre-existing returned buffer cause the OpenCL context is different") 
               
                    // Need to copy
                    let copy = this.CreateUntrackedBuffer(context, queue, parameter, retBuffer.Count, isRoot)
                    let bufferItem = new BufferPoolItem(
                                            copy, 
                                            None,
                                            queue,
                                            parameter.AccessAnalysis,
                                            mergedFlags,
                                            addressSpace.AddressSpace, 
                                            transferMode.HostToDeviceMode,
                                            transferMode.DeviceToHostMode,                    
                                            readMode,
                                            writeMode,
                                            parameter.IsReturned) 
                    if BufferStrategies.ShouldCopyBuffer(retItem.AccessAnalysis, retItem.AddressSpace, parameter.AccessAnalysis, addressSpace.AddressSpace) then
                        if sameContext then
                            BufferTools.CopyBuffer(queue, retBuffer, copy)
                        else
                            let oldArr = this.ReadBuffer(retBuffer, false)
                            let oldHandle = new HostSideDataHandle(oldArr)
                            oldHandle.BeforeTransferToDevice()
                            BufferTools.WriteBuffer(bufferItem.Queue, (writeMode = BufferWriteMode.MapBuffer), bufferItem.Buffer, oldHandle.Ptr, oldArr.GetType().GetElementType(), ArrayUtil.GetArrayLengths(oldArr))                                
                            (oldHandle :> IDisposable).Dispose()
                    // Dispose old buffer
                    copy)
        
    member this.TransferBackModifiedBuffers() =
        // Read back tracked modified buffers
        lock locker (fun () -> 
            for item in trackedBufferPool do
                let poolItem = item.Value
                this.ReadBufferToEnsureHostAccess(poolItem, false))
                    
    member this.ReadBuffer(b: OpenCLBuffer, ?promote:bool) =
        lock locker (fun () -> 
            if reverseTrackedBufferPool.ContainsKey(b) then
                let array = reverseTrackedBufferPool.[b]
                let poolItem = trackedBufferPool.[array]
            
                this.ReadBufferToEnsureHostAccess(poolItem, false, TransferMode.TransferIfNeeded)
                array
            else if untrackedBufferPool.ContainsKey(b.Context) && untrackedBufferPool.[b.Context].ContainsKey(b) then
                if promote.IsSome && promote.Value then
                    let poolItem = untrackedBufferPool.[b.Context].[b]
                    this.PromoteUntrackedToTracked(poolItem)
                    this.ReadBufferToEnsureHostAccess(poolItem, true, TransferMode.TransferIfNeeded)
                    poolItem.HostDataHandle.Value.ManagedData
                else
                    let poolItem = untrackedBufferPool.[b.Context].[b]
                    let elementCount = poolItem.Buffer.Count
                    let elemType = poolItem.Buffer.ElementType
                    let array = Array.CreateInstance(elemType, elementCount)
                    let dataHandle = new HostSideDataHandle(array)
                    poolItem.HostDataHandle <- Some(dataHandle)
                    this.ReadBufferToEnsureHostAccess(poolItem, true, TransferMode.TransferIfNeeded)
                    poolItem.HostDataHandle <- None
                    (dataHandle :> IDisposable).Dispose()
                    array                    
            else
                null)

    member this.BeginOperateOnBuffer(b: OpenCLBuffer, willRead: bool) =
        lock locker (fun () -> 
            if reverseTrackedBufferPool.ContainsKey(b) then
                let array = reverseTrackedBufferPool.[b]
                let poolItem = trackedBufferPool.[array]
            
                if willRead then
                    this.ReadBufferToEnsureHostAccess(poolItem, false, TransferMode.TransferIfNeeded)
                array
            else if untrackedBufferPool.ContainsKey(b.Context) && untrackedBufferPool.[b.Context].ContainsKey(b) then
                let poolItem = untrackedBufferPool.[b.Context].[b]
                this.PromoteUntrackedToTracked(poolItem)
                if willRead then
                    this.ReadBufferToEnsureHostAccess(poolItem, true, TransferMode.TransferIfNeeded)
                poolItem.HostDataHandle.Value.ManagedData
            else
                null)
    
     member this.EndOperateOnBuffer(b: OpenCLBuffer, array: Array, hasWritten: bool) =
        lock locker (fun() ->
            let oldArr = reverseTrackedBufferPool.[b]
            if trackedBufferPool.ContainsKey(array) then
                let poolItem = trackedBufferPool.[array]
                if hasWritten && BufferStrategies.ShouldWriteBuffer(poolItem.AccessAnalysis, poolItem.Buffer.Flags, poolItem.AddressSpace, poolItem.HostToDeviceTransferMode) then
                    poolItem.HostDataHandle.Value.BeforeTransferToDevice()
                    BufferTools.WriteBuffer(poolItem.Queue, poolItem.ReadMode = BufferReadMode.MapBuffer, poolItem.Buffer, poolItem.HostDataHandle.Value.Ptr, poolItem.HostDataHandle.Value.ElementType, poolItem.HostDataHandle.Value.Lenghts)   
            else
                // Array ref changed
                let poolItem = trackedBufferPool.[oldArr]
                trackedBufferPool.Remove(oldArr) |> ignore
                reverseTrackedBufferPool.Remove(b) |> ignore

                // Dispose old data handle
                (poolItem.HostDataHandle.Value :> IDisposable).Dispose()

                // Create host side handle
                let dataHandle = new HostSideDataHandle(array)
                dataHandle.BeforeTransferToDevice()
                poolItem.HostDataHandle <- Some(dataHandle)

                // Add data struct to tracked list
                trackedBufferPool.Add(array, poolItem)
                reverseTrackedBufferPool.Add(poolItem.Buffer, array))

    member this.EndUsingBuffer(buffer) = 
        lock locker (fun () ->       
            if reverseTrackedBufferPool.ContainsKey(buffer) then
                // Tracked buffer
                let arr = reverseTrackedBufferPool.[buffer]
                let b = trackedBufferPool.ContainsKey(arr)
                if b then
                    trackedBufferPool.[arr].IsAvailable <- true

            else if untrackedBufferPool.ContainsKey(buffer.Context) then
                // Untracked buffer
                if untrackedBufferPool.[buffer.Context].ContainsKey(buffer) then
                    let item = untrackedBufferPool.[buffer.Context].[buffer]
                    // Check if this is a return buffer. In this case do not dispose if not available
                    // Note that a buffer returned to the user is a tracking buffer
                    //if item.IsReturned then
                    item.IsAvailable <- true
                    ())
//                        else
//                            // This buffer has already been copied, dispose it
//                            buffer.Dispose()
//                            if item.HostDataHandle.IsSome then
//                                (item.HostDataHandle.Value :> IDisposable).Dispose()
//                            untrackedBufferPool.[buffer.Context].Remove(buffer) |> ignore
//                    else 
//                        // A buffer not tracked cannot be used elsewhere by other kernels or applications
//                        buffer.Dispose()
//                        if item.HostDataHandle.IsSome then
//                            (item.HostDataHandle.Value :> IDisposable).Dispose()
//                        untrackedBufferPool.[buffer.Context].Remove(buffer) |> ignore)

    member this.ClearTrackedAndUntrackedPool() =
        lock locker (fun () ->
            for item in untrackedBufferPool do
                for it in item.Value do
                    it.Key.Dispose()
                    if(it.Value.HostDataHandle.IsSome) then
                        (it.Value.HostDataHandle.Value :> IDisposable).Dispose() 
            untrackedBufferPool.Clear()
            for it in trackedBufferPool do
                it.Value.Buffer.Dispose()
                if(it.Value.HostDataHandle.IsSome) then
                    (it.Value.HostDataHandle.Value :> IDisposable).Dispose() 
            trackedBufferPool.Clear()
            reverseTrackedBufferPool.Clear())

    member this.ClearUntrackedPoolOnly() =
        lock locker (fun () ->
            for item in untrackedBufferPool do
                for it in item.Value do
                    it.Key.Dispose()
                    if(it.Value.HostDataHandle.IsSome) then
                        (it.Value.HostDataHandle.Value :> IDisposable).Dispose() 
            untrackedBufferPool.Clear())

    interface IDisposable with
        member this.Dispose() =
            this.ClearTrackedAndUntrackedPool()

    member this.RequireBufferForParameter(par: IFunctionParameter, 
                                          arg: Array option, 
                                          count: int64[],
                                          context: OpenCLContext, 
                                          queue: OpenCLCommandQueue, 
                                          isRoot: bool,
                                          sharePriority: BufferSharePriority,
                                          ?fromOutput: ExecutionOutput) =
        //Console.WriteLine("\n----------------------------------------------")
        //Console.WriteLine("Required buffer for parameter " + par.Name)
        // Check if fromOutput is set
        let bf = 
            if fromOutput.IsNone then
                //Console.WriteLine("The parameter is NOT bound to a subkernel result")
                // We have not to use a returned buffer
                // Check if arg is set (in this case arg is visible to host or in any case we must write back buffer to arg if buffer modified)
                if arg.IsSome then            
                    //Console.WriteLine("The parameter is bound to an actual argument: create tracked buffer")
                    // Create a buffer bound to the argument
                    this.CreateTrackedBuffer(context, queue, par, arg.Value, isRoot, sharePriority)
                else 
                    // This is a paramter generated from an allocation inside kernel
                    // If not returned or returned from a non root kernel, this is not visible outside the computation, so untracked
                    if par.IsReturned && isRoot then
                        //Console.WriteLine("The parameter is allocated inside kernel and it is returned to the host, create tracked buffer")
                        // Tracked
                        let elemType = par.DataType.GetElementType()
                        let storeArray = Array.CreateInstance(elemType, count)
                        let buff = this.CreateTrackedBuffer(context, queue, par, storeArray, isRoot, sharePriority)                       
                        buff
                    else
                        //Console.WriteLine("The parameter is allocated inside kernel but returned or returned in a subkernel create untracked buffer")
                        // Untracked
                        let buff = this.CreateUntrackedBuffer(context, queue, par, count, isRoot)                       
                        buff 
            else
                //Console.WriteLine("The parameter is bound to a subkernel result")
                // We must use the output of another kernel
                // If this paramter is returned and kernel is root then this can be visible to the host
                if par.IsReturned && isRoot then
                    //Console.WriteLine("The parameter is returned to the host")
                    match fromOutput.Value with
                    | ReturnedBuffer(b) ->
                        // Tracked
                        let buff = this.CreateTrackedBufferFromReturn(b, context, queue, par, isRoot, sharePriority)            
                        buff
                    | ReturnedValue(o) ->
                        if o.GetType().IsArray then
                            // Create a tracked buffer as if the array was not coming from any preceding kernel
                            this.CreateTrackedBuffer(context, queue, par, o :?> Array, isRoot, sharePriority)
                        else
                            raise (KernelSetupException("Only root kernel can return a value object that is not an array!"))
                else
                    //Console.WriteLine("The parameter is NOT returned to the host")
                    // Kernel is not root or it's root bu this par is not returned (no visibility by the host)
                    match fromOutput.Value with
                    | ReturnedBuffer(b) ->
                        let buff = this.CreateUntrackedBufferFromReturn(b, context, queue, par, isRoot, sharePriority)            
                        buff
                    | ReturnedValue(o) ->
                        if o.GetType().IsArray then
                            // Create a tracked buffer as if the array was not coming from any preceding kernel
                            this.CreateTrackedBuffer(context, queue, par, o :?> Array, isRoot, sharePriority)
                        else
                            raise (KernelSetupException("Only root kernel can return a value object that is not an array!"))

        //Console.WriteLine("----------------------------------------------\n")
        bf

    // Private methods    
    member private this.ReadBufferToEnsureHostAccess(poolItem: BufferPoolItem, hasBeenPromoted: bool, ?forceDeviceHostTransferMode: TransferMode) =
        let tmode = 
            if forceDeviceHostTransferMode.IsSome then 
                forceDeviceHostTransferMode.Value 
            else 
                poolItem.DeviceToHostTransferMode
        let shouldReadBackBuffer = BufferStrategies.ShouldReadBackBuffer(poolItem.AccessAnalysis, poolItem.Buffer.Flags, poolItem.AddressSpace, tmode) 
        if shouldReadBackBuffer then
            poolItem.HostDataHandle.Value.BeforeTransferFromDevice()
            if hasBeenPromoted || BufferStrategies.ShouldExplicitlyReadToReadBackBuffer(poolItem.AccessAnalysis, poolItem.Buffer.Flags, poolItem.AddressSpace, poolItem.DeviceToHostTransferMode) then
                // No need to READ to unmanaged pointer
                BufferTools.ReadBuffer(poolItem.Queue, poolItem.ReadMode = BufferReadMode.MapBuffer, poolItem.HostDataHandle.Value.Ptr, poolItem.Buffer, poolItem.HostDataHandle.Value.Lenghts)                
            // Now make sure the managed data is in sync with the possible unmanaged ptr
            poolItem.HostDataHandle.Value.SyncManagedWithUnmanaged()
                
    member private this.PromoteUntrackedToTracked(poolItem: BufferPoolItem) =
        // Create tracking array
        let elementCount = poolItem.Buffer.Count
        let elemType = poolItem.Buffer.ElementType
        let array = Array.CreateInstance(elemType, elementCount)
        // Create host side handle
        let dataHandle = new HostSideDataHandle(array)
        poolItem.HostDataHandle <- Some(dataHandle)

        // Move data struct to tracked buffer list   
        untrackedBufferPool.[poolItem.Buffer.Context].Remove(poolItem.Buffer) |> ignore
        trackedBufferPool.Add(array, poolItem)
        reverseTrackedBufferPool.Add(poolItem.Buffer, array)
                

        

