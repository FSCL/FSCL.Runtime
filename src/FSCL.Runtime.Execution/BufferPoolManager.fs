namespace FSCL.Runtime.Managers

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

[<AllowNullLiteral>]
type HostSideDataHandle(data: Array) =
    let mutable (gcHandle:GCHandle option) = None
    let mutable (ptr:IntPtr) = IntPtr.Zero                           
                
    member val ManagedData = data 
        with get
        
    member val ElementType = data.GetType().GetElementType()
        with get

    member val Lenghts = ArrayUtil.GetArrayLengths(data) 
        with get

    member this.Ptr 
        with get() =
            ptr
        and set v =
            ptr <- v

    member this.HasUnmanagedStorage =
        gcHandle.IsNone && (ptr <> IntPtr.Zero)

    member this.BeforeTransferToDevice() =
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
        this.BeforeTransferToDevice()
        
    member this.SyncManagedWithUnmanaged() =
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

    let trackedBufferPool = if oldPool <> null then oldPool.GetTrackedBufferPool() else Dictionary<Array, BufferPoolItem>()
    let untrackedBufferPool = if oldPool <> null then oldPool.GetUntrackedBufferPool() else Dictionary<OpenCLContext, Dictionary<OpenCLBuffer, BufferPoolItem>>()
    let mutable rootReturnBuffer = None
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
                    
    // Create a struct array out of a record

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
        ////Console.WriteLine("Access analysis says this parameter is: " + parameter.AccessAnalysis.ToString())
        ////Console.WriteLine("Best Memory Flags: " + mergedFlags.ToString())
        ////Console.WriteLine("Best Read Mode: " + readMode.ToString())
        ////Console.WriteLine("Best Write Mode: " + writeMode.ToString())
        
        // Check if there is a buffer bound to the same object
        if trackedBufferPool.ContainsKey(arr) then
            // We are requested to create a buffer matching an object (parameter value) already known
            let prevBuffer = trackedBufferPool.[arr]
            // If not used anymore, same context and access we can use it
            //assert (prevBuffer.IsAvailable)
            let sameContext = prevBuffer.Buffer.Context = context
            let memFlagsCompatible = BufferStrategies.AreMemoryFlagsCompatible(prevBuffer.Flags, mergedFlags, sharePriority)
            if sameContext && memFlagsCompatible then    
                //Console.WriteLine("Buffer is reused with adapteded flags: " + prevBuffer.Flags.ToString())   
                           
                // Update transfer mode
                if (prevBuffer.HostToDeviceTransferMode &&& TransferMode.NoTransfer |> int > 0) && (transferMode.HostToDeviceMode &&& TransferMode.NoTransfer |> int = 0) then
                    prevBuffer.HostToDeviceTransferMode <- prevBuffer.HostToDeviceTransferMode &&& ~~~TransferMode.NoTransfer
                if (prevBuffer.DeviceToHostTransferMode &&& TransferMode.NoTransfer |> int > 0) && (transferMode.DeviceToHostMode &&& TransferMode.NoTransfer |> int = 0) then
                    prevBuffer.DeviceToHostTransferMode <- prevBuffer.DeviceToHostTransferMode &&& ~~~TransferMode.NoTransfer
                
                // If this is the return buffer for root kernel in a kernel expression, remember it cause we will need to read it somewhere at the end
                if parameter.IsReturned && isRoot then
                    rootReturnBuffer <- Some(prevBuffer, arr)

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
                    BufferTools.CopyBuffer(queue, prevBuffer.Buffer, bufferItem.Buffer)
                //else
                    //Console.WriteLine("Buffer in NOT copied")   

                // If this is the return buffer for root kernel in a kernel expression, remember it cause we will need to read it somewhere at the end
                if parameter.IsReturned && isRoot then
                    rootReturnBuffer <- Some(bufferItem, arr)

                // Dispose previous buffer and store new one
                reverseTrackedBufferPool.Remove(prevBuffer.Buffer) |> ignore
                prevBuffer.Buffer.Dispose()
                trackedBufferPool.[arr] <- bufferItem          
                reverseTrackedBufferPool.Add(bufferItem.Buffer, arr)
                bufferItem.Buffer
        else
            //Console.WriteLine("No buffer found, create it")   
            // Create a buffer tracking the parameter
            let dataHandle = new HostSideDataHandle(arr)
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
            if BufferStrategies.ShouldExplicitlyWriteToInitBuffer(parameter.AccessAnalysis, BufferStrategies.ToOpenCLMemoryFlags(mergedFlags), addressSpace.AddressSpace, transferMode.HostToDeviceMode) then
                dataHandle.BeforeTransferToDevice()
                BufferTools.WriteBuffer(queue, (writeMode = BufferWriteMode.MapBuffer), bufferItem.Buffer, dataHandle.Ptr, arr.GetType().GetElementType(), ArrayUtil.GetArrayLengths(arr))    
                //else
                //Console.WriteLine("Buffer is NOT initialised")                           
            // If this is the return buffer for root kernel in a kernel expression, remember it cause we will need to read it somewhere at the end
            if parameter.IsReturned && isRoot then
                rootReturnBuffer <- Some(bufferItem, arr)

            // Store buffer          
            trackedBufferPool.Add(arr, bufferItem)
            reverseTrackedBufferPool.Add(bufferItem.Buffer, arr)
            bufferItem.Buffer
            
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

        if not (untrackedBufferPool.ContainsKey(context)) then
            untrackedBufferPool.Add(context, new Dictionary<OpenCLBuffer, BufferPoolItem>())

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
        
        bufferItem.Buffer
                        
    // This is to create buffers that are returned from a root kernel (so tracked) and bound to calls to subkernels
    member this.CreateTrackedBufferFromReturn(retBuffer:OpenCLBuffer, 
                                              retArr: Array option,
                                              newArr: Array,
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
        
        if retArr.IsSome then
            // Return is tracked
            // Create tracked buffer will handle possible no copy
            this.CreateTrackedBuffer(context, queue, parameter, retArr.Value, isRoot, sharePriority)
        else
            // Return is untracked
            let retItem = untrackedBufferPool.[retBuffer.Context].[retBuffer]
            assert(retItem.IsAvailable)  
            //Console.WriteLine("Returned buffer is UNTRACKED")                 
            // Check if this can be used with no copy
            let sameContext = context = retBuffer.Context
            let memFlagsCompatible = BufferStrategies.AreMemoryFlagsCompatible(retItem.Flags, mergedFlags, sharePriority)
            if sameContext && memFlagsCompatible then
                //Console.WriteLine("Returned buffer is promoted to TRACKED with adapteded flags: " + retItem.Flags.ToString())  
                // Must promote this untracked item to tracked
                retItem.IsAvailable <- false
                untrackedBufferPool.[retBuffer.Context].Remove(retBuffer) |> ignore
                trackedBufferPool.Add(newArr, retItem)
                // Since now it's tracked it could be returned
                if parameter.IsReturned && isRoot then
                    rootReturnBuffer <- Some(retItem, newArr)
                retBuffer
            else
                if sameContext then
                    Trace.WriteLine("FSCL Warning: tracked buffer for parameter " + parameter.Name + " cannot reuse pre-existing returned buffer cause memory flags are not compatible (returned buffer: " + retItem.Flags.ToString() + ", this parameter: " + mergedFlags.ToString() + ", share priority: " + sharePriority.ToString()) 
                else
                    Trace.WriteLine("FSCL Warning: tracked buffer for parameter " + parameter.Name + " cannot reuse pre-existing returned buffer cause the OpenCL context is different") 
                
                //Console.WriteLine("No adapted flags can be computed, create new buffer")   
                let poolItem = untrackedBufferPool.[retBuffer.Context].[retBuffer]
                // Need to copy
                let copy = this.CreateTrackedBuffer(context, queue, parameter, newArr, isRoot, sharePriority)
                if BufferStrategies.ShouldCopyBuffer(poolItem.AccessAnalysis, poolItem.AddressSpace, parameter.AccessAnalysis, addressSpace.AddressSpace) then
                    //Console.WriteLine("Buffer is copied")   
                    BufferTools.CopyBuffer(queue, retBuffer, copy)
                //else
                    //Console.WriteLine("Buffer is NOT copied")   
                // Dispose old buffer
                poolItem.IsAvailable <- true
                this.EndUsingBuffer(poolItem.Buffer)
                copy
             
    // This is called for buffers bound to subkernels and not returned or returned from non-root   
    member this.CreateUntrackedBufferFromReturn(retBuffer:OpenCLBuffer, 
                                                retArr: Array option,
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
                                    true,
                                    queue.Device)
        //Console.WriteLine("Access analysis says this parameter is: " + parameter.AccessAnalysis.ToString())
        //Console.WriteLine("Best Memory Flags: " + mergedFlags.ToString())
        //Console.WriteLine("Best Read Mode: " + readMode.ToString())
        //Console.WriteLine("Best Write Mode: " + writeMode.ToString())
        
        if retArr.IsSome then
            //Console.WriteLine("Returned buffer is TRACKED")
            // A tracked buffer remains tracked
            this.CreateTrackedBuffer(context, queue, parameter, retArr.Value, isRoot, sharePriority)
        else
            //Console.WriteLine("Returned buffer is UNTRACKED")
            // Return is untracked
            let retItem = untrackedBufferPool.[retBuffer.Context].[retBuffer]
            assert(retItem.IsAvailable)                   
            // Check if this can be used with no copy
            let sameContext = context = retBuffer.Context
            let memFlagsCompatible = BufferStrategies.AreMemoryFlagsCompatible(retItem.Flags, mergedFlags, sharePriority)
            if sameContext && memFlagsCompatible then
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
                if BufferStrategies.ShouldCopyBuffer(retItem.AccessAnalysis, retItem.AddressSpace, parameter.AccessAnalysis, addressSpace.AddressSpace) then
                    BufferTools.CopyBuffer(queue, retBuffer, copy)
                // Dispose old buffer
                retItem.IsAvailable <- true
                this.EndUsingBuffer(retItem.Buffer)
                copy
        
    member this.TransferBackModifiedBuffers() =
        // Read back tracked modified buffers
        for item in trackedBufferPool do
            let poolItem = item.Value
            this.ReadBufferToEnsureHostAccess(poolItem, false)
                    
    member this.ReadRootBuffer() =
        // Read back root return buffer
        if rootReturnBuffer.IsSome then
            let poolItem, arr = rootReturnBuffer.Value
            this.ReadBufferToEnsureHostAccess(poolItem, false)
            arr
        else
            null

    member this.BeginOperateOnBuffer(b: OpenCLBuffer, willRead: bool) =
        if reverseTrackedBufferPool.ContainsKey(b) then
            let array = reverseTrackedBufferPool.[b]
            let poolItem = trackedBufferPool.[array]
            
            if willRead then
                this.ReadBufferToEnsureHostAccess(poolItem, false)
            array
        else if untrackedBufferPool.ContainsKey(b.Context) && untrackedBufferPool.[b.Context].ContainsKey(b) then
            let poolItem = untrackedBufferPool.[b.Context].[b]
            this.PromoteUntrackedToTracked(poolItem)
            if willRead then
                this.ReadBufferToEnsureHostAccess(poolItem, true)
            poolItem.HostDataHandle.Value.ManagedData
        else
            null      
    
     member this.EndOperateOnBuffer(b: OpenCLBuffer, array: Array, hasWritten: bool) =
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
            reverseTrackedBufferPool.Add(poolItem.Buffer, array)

    member this.EndUsingBuffer(buffer) =        
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
                if item.IsReturned then
                    if not(item.IsAvailable) then
                        item.IsAvailable <- true
                    else
                        // This buffer has already been copied, dispose it
                        buffer.Dispose()
                        if item.HostDataHandle.IsSome then
                            (item.HostDataHandle.Value :> IDisposable).Dispose()
                        untrackedBufferPool.[buffer.Context].Remove(buffer) |> ignore
                else 
                    // A buffer not tracked cannot be used elsewhere by other kernels or applications
                    buffer.Dispose()
                    if item.HostDataHandle.IsSome then
                        (item.HostDataHandle.Value :> IDisposable).Dispose()
                    untrackedBufferPool.[buffer.Context].Remove(buffer) |> ignore

    member this.ClearTrackedAndUntrackedPool() =
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
        reverseTrackedBufferPool.Clear()

    member this.ClearUntrackedPoolOnly() =
        for item in untrackedBufferPool do
            for it in item.Value do
                it.Key.Dispose()
                if(it.Value.HostDataHandle.IsSome) then
                    (it.Value.HostDataHandle.Value :> IDisposable).Dispose() 
        untrackedBufferPool.Clear()

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
                    | ReturnedTrackedBuffer(b, arr) ->
                        // There is already a tracking array, do not allocate another one
                        if arg.IsNone then
                            // Do not have an arg
                            let elemType = par.DataType.GetElementType()
                            let storeArray = Array.CreateInstance(elemType, count)
                            let buff = this.CreateTrackedBufferFromReturn(b, Some(arr), storeArray, context, queue, par, isRoot, sharePriority)            
                            buff
                        else
                            // Do have an arg
                            let buff = this.CreateTrackedBufferFromReturn(b, Some(arr), arg.Value, context, queue, par, isRoot, sharePriority)            
                            buff
                    | ReturnedUntrackedBuffer(b) ->
                        // An untracked buffer is returned, but this must be tracked
                        let elementCount = b.Count
                        let elemType = par.DataType.GetElementType()
                        let storeArray = Array.CreateInstance(elemType, elementCount)
                        let buff = this.CreateTrackedBufferFromReturn(b, None, storeArray, context, queue, par, isRoot, sharePriority)            
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
                    | ReturnedTrackedBuffer(b, arr) ->
                        // There is already a tracking array, do not allocate another one
                        let buff = this.CreateUntrackedBufferFromReturn(b, Some(arr), context, queue, par, isRoot, sharePriority)            
                        buff
                    | ReturnedUntrackedBuffer(b) ->
                        // An untracked buffer is returned
                        let buff = this.CreateUntrackedBufferFromReturn(b, None, context, queue, par, isRoot, sharePriority)            
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
    member private this.ReadBufferToEnsureHostAccess(poolItem: BufferPoolItem, hasBeenPromoted: bool) =
        let shouldReadBackBuffer = BufferStrategies.ShouldReadBackBuffer(poolItem.AccessAnalysis, poolItem.Buffer.Flags, poolItem.AddressSpace, poolItem.DeviceToHostTransferMode) 
        if shouldReadBackBuffer then
            poolItem.HostDataHandle.Value.BeforeTransferFromDevice()
            if hasBeenPromoted || BufferStrategies.ShouldExplicitlyReadToReadBackBuffer(poolItem.AccessAnalysis, poolItem.Buffer.Flags, poolItem.AddressSpace, poolItem.DeviceToHostTransferMode) then
                // No need to READ to unmanaged pointer
                BufferTools.ReadBuffer(poolItem.Queue, poolItem.ReadMode = BufferReadMode.MapBuffer, poolItem.HostDataHandle.Value.Ptr, poolItem.Buffer, poolItem.HostDataHandle.Value.Lenghts)                
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
                

        

