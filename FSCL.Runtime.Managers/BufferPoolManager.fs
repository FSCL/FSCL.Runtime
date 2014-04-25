namespace FSCL.Runtime.Managers

open OpenCL
open System.Collections.Generic
open System
open System.Runtime.InteropServices
open FSCL.Compiler
open FSCL.Compiler.Language
open FSCL.Runtime

[<AllowNullLiteral>]
type BufferPoolItem(buffer: OpenCLBuffer, 
                    queue: OpenCLCommandQueue, 
                    space: AddressSpace, 
                    transfer: TransferMode, 
                    rMode: BufferReadMode, 
                    wMode: BufferWriteMode, 
                    isReturn: bool) =
    member val WriteMode = wMode with get
    member val ReadMode = rMode with get 
    member val Transfer = transfer with get
    member val AddressSpace = space with get
    member val IsReturned = isReturn with get 
    member val Buffer = buffer with get
    member val IsAvailable = false with get, set
    member val CurrentQueue = queue with get, set

[<AllowNullLiteral>]
type BufferPoolManager() =
    let mutable noObjID = 0

    let trackedBufferPool = Dictionary<obj, BufferPoolItem>()
    let untrackedBufferPool = Dictionary<OpenCLContext, Dictionary<OpenCLBuffer, BufferPoolItem>>()
    let mutable rootReturnBuffer = None

    let reverseTrackedBufferPool = Dictionary<OpenCLBuffer, obj>()

    // Non-tracking buffer
    // This is the buffer created for buffers allocated and returned inside kernels and for buffer obtained from the execution of other previos kernels
    member this.CreateUntrackedBuffer(context, queue, parameter:IFunctionParameter, count:int64[], isRoot) =
        // Get parameter meta
        let transferMode = parameter.Meta.Get<TransferModeAttribute>()
        let readMode = parameter.Meta.Get<BufferReadModeAttribute>()
        let writeMode = parameter.Meta.Get<BufferWriteModeAttribute>()
        let addressSpace = parameter.Meta.Get<AddressSpaceAttribute>()
        let memoryFlags = parameter.Meta.Get<MemoryFlagsAttribute>()
        let mergedFlags = BufferTools.MergeAccessAndFlags(addressSpace.AddressSpace, parameter.Access, memoryFlags.Flags, isRoot, parameter.IsReturned)

        if not (untrackedBufferPool.ContainsKey(context)) then
            untrackedBufferPool.Add(context, new Dictionary<OpenCLBuffer, BufferPoolItem>())

        let transferMode = parameter.Meta.Get<TransferModeAttribute>()
        let bufferItem = new BufferPoolItem(
                            BufferTools.CreateBuffer(parameter.DataType.GetElementType(), count, context, queue, mergedFlags), 
                            queue,
                            addressSpace.AddressSpace, 
                            transferMode.Mode,
                            readMode.Mode,
                            writeMode.Mode,
                            parameter.IsReturned)
        untrackedBufferPool.[context].Add(bufferItem.Buffer, bufferItem)

        // If this is the return buffer for root kernel in a kernel expression, remember it cause we will need to read it somewhere at the end
        if parameter.IsReturned && isRoot then
            rootReturnBuffer <- Some(bufferItem)

        bufferItem.Buffer

    member this.DisposeBuffer(buffer) =        
        if reverseTrackedBufferPool.ContainsKey(buffer) then
            // Tracked buffer
            let item = trackedBufferPool.[reverseTrackedBufferPool.[buffer]]
            item.IsAvailable <- true
        else if untrackedBufferPool.ContainsKey(buffer.Context) then
            // Untracked buffer
            let item = untrackedBufferPool.[buffer.Context].[buffer]
            // Check if this is a return buffer. In this case do not dispose if not available
            if item.IsReturned then
                if not(item.IsAvailable) then
                    item.IsAvailable <- true
                else
                    // This buffer has already been copied, dispose it
                    buffer.Dispose()
                    untrackedBufferPool.[buffer.Context].Remove(buffer) |> ignore
            else 
                // A buffer not tracked cannot be used elsewhere by other kernels or applications
                buffer.Dispose()
                untrackedBufferPool.[buffer.Context].Remove(buffer) |> ignore
                
    member this.UseUntrackedBuffer(buffer:OpenCLBuffer, context, queue, parameter: IFunctionParameter, isRoot) =
        let transferMode = parameter.Meta.Get<TransferModeAttribute>()
        let addressSpace = parameter.Meta.Get<AddressSpaceAttribute>()
        let memoryFlags = parameter.Meta.Get<MemoryFlagsAttribute>()
        let mergedFlags = BufferTools.MergeAccessAndFlags(addressSpace.AddressSpace, parameter.Access, memoryFlags.Flags, isRoot, parameter.IsReturned)

        if untrackedBufferPool.ContainsKey(buffer.Context) then
            let item = untrackedBufferPool.[buffer.Context].[buffer]
            // Check if this can be used with no copy
            let mutable avoidCopy = context = buffer.Context
            if context = buffer.Context &&
                BufferTools.AreOpenCLMemoryFlagsCompatible(buffer.Flags, mergedFlags) then
                buffer
            else
                // Need to copy
                let copy = this.CreateUntrackedBuffer(context, queue, parameter, buffer.Count, isRoot)
                if BufferTools.ShouldWriteBuffer(copy, addressSpace.AddressSpace, transferMode.Mode) then
                    BufferTools.CopyBuffer(queue, buffer, copy)
                // Dispose old buffer
                item.IsAvailable <- true
                this.DisposeBuffer(item.Buffer)
                copy
        else
            // Error: you can't use a buffer that is not in the pool
            null

    // Tracking buffer
    // This is the buffer created when an actual argument is provided
    member this.CreateTrackedBuffer(context, queue, parameter:IFunctionParameter, o, strictFlags, isRoot) =    
        // Get parameter meta
        let transferMode = parameter.Meta.Get<TransferModeAttribute>()
        let readMode = parameter.Meta.Get<BufferReadModeAttribute>()
        let writeMode = parameter.Meta.Get<BufferWriteModeAttribute>()
        let addressSpace = parameter.Meta.Get<AddressSpaceAttribute>()
        let memoryFlags = parameter.Meta.Get<MemoryFlagsAttribute>()
        let mergedFlags = BufferTools.MergeAccessAndFlags(addressSpace.AddressSpace, parameter.Access, memoryFlags.Flags, isRoot, parameter.IsReturned)

        // Check if there is a buffer bound to the same object
        if trackedBufferPool.ContainsKey(o) then
            // We are requested to create a buffer matching an object (parameter value) already known
            let prevBuffer = trackedBufferPool.[o]
            // If not used anymore, same context and access we can use it
            assert (prevBuffer.IsAvailable)
            if prevBuffer.Buffer.Context = context &&
                BufferTools.AreOpenCLMemoryFlagsCompatible(prevBuffer.Buffer.Flags, mergedFlags) then
                
                // If this is the return buffer for root kernel in a kernel expression, remember it cause we will need to read it somewhere at the end
                if parameter.IsReturned && isRoot then
                    rootReturnBuffer <- Some(prevBuffer)

                // Use this buffer
                prevBuffer.CurrentQueue <- queue
                prevBuffer.Buffer
            else
                let bufferItem = new BufferPoolItem(
                                    BufferTools.CreateBuffer(o.GetType().GetElementType(), ArrayUtil.GetArrayLengths(o), context, queue, mergedFlags), 
                                    queue, 
                                    addressSpace.AddressSpace, 
                                    transferMode.Mode,
                                    readMode.Mode,
                                    writeMode.Mode,
                                    parameter.IsReturned)
                // We need to copy buffer only if no write-only, host can read and user did not specify NoTransfer                
                if (BufferTools.ShouldWriteBuffer(bufferItem.Buffer, addressSpace.AddressSpace, transferMode.Mode)) then
                    BufferTools.CopyBuffer(queue, prevBuffer.Buffer, bufferItem.Buffer)

                // If this is the return buffer for root kernel in a kernel expression, remember it cause we will need to read it somewhere at the end
                if parameter.IsReturned && isRoot then
                    rootReturnBuffer <- Some(bufferItem)

                // Dispose previous buffer and store new one
                reverseTrackedBufferPool.Remove(prevBuffer.Buffer) |> ignore
                prevBuffer.Buffer.Dispose()
                trackedBufferPool.[o] <- bufferItem          
                bufferItem.Buffer
        else
            // Create a buffer tracking the parameter
            let bufferItem = new BufferPoolItem(
                                    BufferTools.CreateBuffer(o.GetType().GetElementType(), ArrayUtil.GetArrayLengths(o), context, queue, mergedFlags), 
                                    queue, 
                                    addressSpace.AddressSpace, 
                                    transferMode.Mode,                                     
                                    readMode.Mode,
                                    writeMode.Mode,
                                    parameter.IsReturned)            
            if BufferTools.ShouldWriteBuffer(bufferItem.Buffer, addressSpace.AddressSpace, transferMode.Mode) then
                BufferTools.WriteBuffer(queue, writeMode.Mode = BufferWriteMode.MapBuffer, bufferItem.Buffer, o)    
                                           
            // If this is the return buffer for root kernel in a kernel expression, remember it cause we will need to read it somewhere at the end
            if parameter.IsReturned && isRoot then
                rootReturnBuffer <- Some(bufferItem)

            // Store buffer          
            trackedBufferPool.Add(o, bufferItem)
            reverseTrackedBufferPool.Add(bufferItem.Buffer, o)
            bufferItem.Buffer
            
    member this.TransferBackModifiedBuffers() =
        // Read back tracked modified buffers
        for item in trackedBufferPool do
            let poolItem = item.Value
            if BufferTools.ShouldReadBuffer(poolItem.Buffer, poolItem.AddressSpace, poolItem.Transfer) then
                BufferTools.ReadBuffer(poolItem.CurrentQueue, poolItem.ReadMode = BufferReadMode.MapBuffer, item.Key :?> Array, poolItem.Buffer)
        
    member this.ReadRootReturnBuffer() =
        // Read back root return buffer
        let returnValue = 
            if rootReturnBuffer.IsSome && (BufferTools.ShouldReadBuffer(rootReturnBuffer.Value.Buffer, rootReturnBuffer.Value.AddressSpace, rootReturnBuffer.Value.Transfer)) then
                let sizes = rootReturnBuffer.Value.Buffer.Count
                let returnedArray = Array.CreateInstance(rootReturnBuffer.Value.Buffer.ElementType, sizes)
                BufferTools.ReadBuffer(rootReturnBuffer.Value.CurrentQueue, rootReturnBuffer.Value.ReadMode = BufferReadMode.MapBuffer, returnedArray, rootReturnBuffer.Value.Buffer)
                rootReturnBuffer <- None
                returnedArray
            else
                null
        
        // Read back tracked modified buffers
        for item in trackedBufferPool do
            let poolItem = item.Value
            if BufferTools.ShouldReadBuffer(poolItem.Buffer, poolItem.AddressSpace, poolItem.Transfer) then
                BufferTools.ReadBuffer(poolItem.CurrentQueue, poolItem.ReadMode = BufferReadMode.MapBuffer, item.Key :?> Array, poolItem.Buffer)
                        
        returnValue

    member this.Dispose() =
        for item in untrackedBufferPool do
            for it in item.Value do
                it.Key.Dispose()
        untrackedBufferPool.Clear()
        for item in trackedBufferPool do
            item.Value.Buffer.Dispose()
        trackedBufferPool.Clear()
        reverseTrackedBufferPool.Clear()
            
        (*
    member this.DisposeBuffer(context, id) =
        if pool.ContainsKey(context) then
            let mutable found = false
            let mutable i = 0
            let buffers = pool.[context]
            while (not found && (i < buffers.Count)) do
                if buffers.[i].ID = id then
                    buffers.[i].IsAvailable <- true
                    found <- true
                else
                    i <- i + 1

    member this.Dispose() =
        for item in pool do
            for buff in item.Value do
                buff.Buffer.Dispose()
        pool.Clear()
        *)
        
                

        

