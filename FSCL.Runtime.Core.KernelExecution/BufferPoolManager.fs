namespace FSCL.Runtime.KernelExecution

open Cloo
open System.Collections.Generic
open System
open System.Runtime.InteropServices
open FSCL.Compiler
open FSCL.Runtime

[<AllowNullLiteral>]
type BufferPoolItem(buffer: ComputeMemory, queue:ComputeCommandQueue, access: KernelParameterAccessMode, transfer: KernelParameterTransferMode, isReturn: bool) =
    member val Access = access with get
    member val Transfer = transfer with get
    member val IsReturned = isReturn with get 
    member val Buffer = buffer with get
    member val IsAvailable = false with get, set
    member val CurrentQueue = queue with get, set
    member val HasBeenModified = false with get, set

[<AllowNullLiteral>]
type BufferPoolManager() =
    let mutable noObjID = 0

    let trackedBufferPool = Dictionary<obj, BufferPoolItem>()
    let untrackedBufferPool = Dictionary<ComputeContext, Dictionary<ComputeMemory, BufferPoolItem>>()
    let mutable rootReturnBuffer = None

    let reverseTrackedBufferPool = Dictionary<ComputeMemory, obj>()
    (*
    member private this.CreateReplaceBuffer(t, context, queue, count:int64[], flags) =
        if pool.ContainsKey(context) then
            let buffers = pool.[context]
            let size = (Array.reduce(fun a b -> a * b) count) * (Marshal.SizeOf(t) |> int64)
            let mutable id = 0
            if buffers.Count >= maxBuffPerContext then
                // Check an available buffer that has a similar size to the one we are going to create
                let mutable selectedIndex = -1
                let mutable minDiff = Int64.MaxValue
                for i = 0 to buffers.Count - 1 do
                    if buffers.[i].IsAvailable then
                        if Math.Abs(buffers.[i].Buffer.Size - size) < minDiff then
                            minDiff <- Math.Abs(buffers.[i].Buffer.Size - size)
                            selectedIndex <- i
                            id <- buffers.[i].ID
                // Dispose buffer
                buffers.[selectedIndex].Buffer.Dispose()
                // Remove buffer 
                buffers.RemoveAt(selectedIndex)
            else
                id <- poolContextMaxID.[context] + 1
                poolContextMaxID.[context] <- id

            // Create new buffer
            let bufferItem = new BufferPoolItem(id, BufferTools.CreateBuffer(t, count, context, queue, flags).Value)
            pool.[context].Add(bufferItem)
            (id, bufferItem.Buffer)
        else
            // Create new buffer
            let id = 0
            poolContextMaxID.Add(context, id)

            let bufferItem = new BufferPoolItem(id, BufferTools.CreateBuffer(t, count, context, queue, flags).Value)
            pool.Add(context, new List<BufferPoolItem>())
            pool.[context].Add(bufferItem)
            (id, bufferItem.Buffer)
            *)
        
    // Non-tracking buffer
    // This is the buffer created for buffers allocated and returned inside kernels and for buffer obtained from the execution of other previos kernels
    member this.CreateUntrackedBuffer(context, queue, parameter:KernelParameterInfo, count:int64[], flags, isRoot) =
        if not (untrackedBufferPool.ContainsKey(context)) then
            untrackedBufferPool.Add(context, new Dictionary<ComputeMemory, BufferPoolItem>())

        let bufferItem = new BufferPoolItem(BufferTools.CreateBuffer(parameter.Type.GetElementType(), count, context, queue, flags), queue, parameter.Access, parameter.Transfer, parameter.IsReturnParameter)
        untrackedBufferPool.[context].Add(bufferItem.Buffer, bufferItem)

        // If this is the return buffer for root kernel in a kernel expression, remember it cause we will need to read it somewhere at the end
        if parameter.IsReturnParameter && isRoot then
            rootReturnBuffer <- Some(bufferItem)

        bufferItem.Buffer

    member this.DisposeBuffer(buffer:ComputeMemory) =        
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
                
    member this.UseUntrackedBuffer(buffer:ComputeMemory, context, queue, parameter:KernelParameterInfo, flags:ComputeMemoryFlags, isRoot) =
        if untrackedBufferPool.ContainsKey(buffer.Context) then
            let item = untrackedBufferPool.[buffer.Context].[buffer]
            // Check if this can be used with no copy
            if context = buffer.Context &&
                (parameter.Access &&& item.Access = parameter.Access) &&
                (flags &&& item.Buffer.Flags = flags) then
                buffer
            else
                // Need to copy
                let copy = this.CreateUntrackedBuffer(context, queue, parameter, buffer.Count, flags, isRoot) 
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
    member this.CreateTrackedBuffer(context, queue, parameter:KernelParameterInfo, o, flags, strictFlags) =
        // Check if there is a buffer bound to the same object
        if trackedBufferPool.ContainsKey(o) then
            // We are requested to create a buffer matching an object (parameter value) already known
            let prevBuffer = trackedBufferPool.[o]
            // If not used anymore, same context and access we can use it
            assert (prevBuffer.IsAvailable)
            if prevBuffer.Buffer.Context = context &&
                (prevBuffer.Access ||| parameter.Access = prevBuffer.Access) && 
                (prevBuffer.Buffer.Flags ||| flags = prevBuffer.Buffer.Flags) then
                // Use this buffer
                prevBuffer.CurrentQueue <- queue
                prevBuffer.Buffer
            else
                let bufferItem = new BufferPoolItem(BufferTools.CreateBuffer(o.GetType().GetElementType(), ArrayUtil.GetArrayLengths(o), context, queue, flags), queue, parameter.Access, parameter.Transfer, parameter.IsReturnParameter)
                // We need to copy buffer only if no write-only                
                if (parameter.Access &&& KernelParameterAccessMode.ReadAccess |> int > 0) then
                    BufferTools.CopyBuffer(queue, prevBuffer.Buffer, bufferItem.Buffer)

                // Remember if this has been modified
                bufferItem.HasBeenModified <- prevBuffer.HasBeenModified
                if (parameter.Access &&& KernelParameterAccessMode.WriteAccess |> int > 0) then
                    bufferItem.HasBeenModified  <- true

                // Dispose previous buffer and store new one
                reverseTrackedBufferPool.Remove(prevBuffer.Buffer) |> ignore
                prevBuffer.Buffer.Dispose()
                trackedBufferPool.[o] <- bufferItem          
                bufferItem.Buffer
        else
            // Create a buffer tracking the parameter
            let bufferItem = new BufferPoolItem(BufferTools.CreateBuffer(o.GetType().GetElementType(), ArrayUtil.GetArrayLengths(o), context, queue, flags), queue, parameter.Access, parameter.Transfer, parameter.IsReturnParameter)
            // Check if need to initialize
            let mustInitBuffer =
                ((parameter.AddressSpace = KernelParameterAddressSpace.GlobalSpace) ||
                    (parameter.AddressSpace = KernelParameterAddressSpace.ConstantSpace)) &&
                ((parameter.Access &&& KernelParameterAccessMode.ReadAccess |> int > 0))
            
            if mustInitBuffer then
                BufferTools.WriteBuffer(o.GetType().GetElementType(), queue, bufferItem.Buffer, o)    
                
            // Remember if this has been modified
            if (parameter.Access &&& KernelParameterAccessMode.WriteAccess |> int > 0) then
                bufferItem.HasBeenModified  <- true          
                              
            // Store buffer          
            trackedBufferPool.Add(o, bufferItem)
            reverseTrackedBufferPool.Add(bufferItem.Buffer, o)
            bufferItem.Buffer
            
    member this.TransferBackModifiedBuffers() =
        // Read back tracked modified buffers
        for item in trackedBufferPool do
            let poolItem = item.Value
            if poolItem.HasBeenModified && (poolItem.Transfer &&& KernelParameterTransferMode.NoTransferBack |> int = 0) then
                BufferTools.ReadBuffer(item.Key.GetType().GetElementType(), poolItem.CurrentQueue, item.Key, poolItem.Buffer)
        
    member this.ReadRootReturnBuffer() =
        // Read back root return buffer
        let returnValue = 
            if rootReturnBuffer.IsSome then
                let sizes = rootReturnBuffer.Value.Buffer.Count
                let elementType = rootReturnBuffer.Value.Buffer.GetType().GetGenericArguments().[0]
                let returnedArray = Array.CreateInstance(elementType, sizes)
                BufferTools.ReadBuffer(elementType, rootReturnBuffer.Value.CurrentQueue, returnedArray, rootReturnBuffer.Value.Buffer)
                rootReturnBuffer <- None
                returnedArray
            else
                null
        
        // Read back tracked modified buffers
        for item in trackedBufferPool do
            let poolItem = item.Value
            if poolItem.HasBeenModified && (poolItem.Transfer &&& KernelParameterTransferMode.NoTransferBack |> int = 0) then
                BufferTools.ReadBuffer(item.Key.GetType().GetElementType(), poolItem.CurrentQueue, item.Key, poolItem.Buffer)
                        
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
        
                

        

