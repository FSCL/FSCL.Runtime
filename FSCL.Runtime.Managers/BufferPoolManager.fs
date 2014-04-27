namespace FSCL.Runtime.Managers

open OpenCL
open System.Collections.Generic
open System
open System.Runtime.InteropServices
open FSCL.Compiler
open FSCL.Compiler.Language
open FSCL.Runtime
open FSCL.Runtime.Language

[<AllowNullLiteral>]
type BufferPoolItem(buffer: OpenCLBuffer, 
                    queue: OpenCLCommandQueue, 
                    flags: MemoryFlags,
                    space: AddressSpace, 
                    transfer: TransferMode, 
                    rMode: BufferReadMode, 
                    wMode: BufferWriteMode, 
                    isReturn: bool) =
    member val WriteMode = wMode with get
    member val ReadMode = rMode with get 
    member val TransferMode = transfer with get
    member val AddressSpace = space with get
    member val IsReturned = isReturn with get
    member val Flags = flags with get
     
    member val Buffer = buffer with get
    member val IsAvailable = false with get, set
    member val Queue = queue with get

[<AllowNullLiteral>]
type BufferPoolManager() =
    let mutable noObjID = 0

    let trackedBufferPool = Dictionary<obj, BufferPoolItem>()
    let untrackedBufferPool = Dictionary<OpenCLContext, Dictionary<OpenCLBuffer, BufferPoolItem>>()
    let mutable rootReturnBuffer = None
    let reverseTrackedBufferPool = Dictionary<OpenCLBuffer, obj>()
    
    // Tracking buffer
    // This is the buffer is amtching an array argument
    member this.CreateTrackedBuffer(context: OpenCLContext,
                                    queue: OpenCLCommandQueue, 
                                    parameter: IFunctionParameter, 
                                    arr: Array, 
                                    isRoot: bool, 
                                    sharePriority: BufferSharePriority) =  
        Console.WriteLine("Request to create a TRACKED buffer")  
        // Get parameter meta
        let transferMode = parameter.Meta.Get<TransferModeAttribute>()
        let rMode = parameter.Meta.Get<BufferReadModeAttribute>()
        let wMode = parameter.Meta.Get<BufferWriteModeAttribute>()
        let addressSpace = parameter.Meta.Get<AddressSpaceAttribute>()
        let memoryFlags = parameter.Meta.Get<MemoryFlagsAttribute>()
        let mergedFlags, readMode, writeMode = 
            BufferStrategies.DetermineBestFlagsAndReadWriteMode(
                                                addressSpace.AddressSpace, 
                                                parameter.Access,
                                                rMode.Mode,
                                                wMode.Mode,
                                                transferMode.Mode,
                                                memoryFlags.Flags, 
                                                isRoot, 
                                                parameter.IsReturned,
                                                true,
                                                queue.Device)
        Console.WriteLine("Best Memory Flags: " + mergedFlags.ToString())
        Console.WriteLine("Best Read Mode: " + readMode.ToString())
        Console.WriteLine("Best Write Mode: " + writeMode.ToString())
        
        // Check if there is a buffer bound to the same object
        if trackedBufferPool.ContainsKey(arr) then
            // We are requested to create a buffer matching an object (parameter value) already known
            let prevBuffer = trackedBufferPool.[arr]
            // If not used anymore, same context and access we can use it
            assert (prevBuffer.IsAvailable)
            if prevBuffer.Buffer.Context = context && BufferStrategies.AreMemoryFlagsCompatible(prevBuffer.Flags, mergedFlags, sharePriority) then    
                Console.WriteLine("Buffer is reused with adapteded flags: " + prevBuffer.Flags.ToString())   
                           
                // If this is the return buffer for root kernel in a kernel expression, remember it cause we will need to read it somewhere at the end
                if parameter.IsReturned && isRoot then
                    rootReturnBuffer <- Some(prevBuffer, arr)

                // Use this buffer
                prevBuffer.IsAvailable <- false
                prevBuffer.Buffer
            else
                Console.WriteLine("No adapted flags can be computed, create new buffer")   
                // We must create a new buffer
                let bufferItem = new BufferPoolItem(
                                    BufferTools.CreateBuffer(arr.GetType().GetElementType(), ArrayUtil.GetArrayLengths(arr), context, queue, BufferStrategies.ToOpenCLMemoryFlags(mergedFlags)), 
                                    queue,
                                    mergedFlags, 
                                    addressSpace.AddressSpace, 
                                    transferMode.Mode,
                                    readMode,
                                    writeMode,
                                    parameter.IsReturned)
                // We need to copy buffer only if is has been potentially changed the one we are compying from
                if (BufferStrategies.ShouldCopyBuffer(prevBuffer.Buffer, prevBuffer.AddressSpace, bufferItem.Buffer, addressSpace.AddressSpace)) then
                    Console.WriteLine("Buffer is copied")    
                    BufferTools.CopyBuffer(queue, prevBuffer.Buffer, bufferItem.Buffer)
                else
                    Console.WriteLine("Buffer in NOT copied")   

                // If this is the return buffer for root kernel in a kernel expression, remember it cause we will need to read it somewhere at the end
                if parameter.IsReturned && isRoot then
                    rootReturnBuffer <- Some(bufferItem, arr)

                // Dispose previous buffer and store new one
                reverseTrackedBufferPool.Remove(prevBuffer.Buffer) |> ignore
                prevBuffer.Buffer.Dispose()
                trackedBufferPool.[arr] <- bufferItem          
                bufferItem.Buffer
        else
            Console.WriteLine("No buffer found, create it")   
            // Create a buffer tracking the parameter
            let bufferItem = new BufferPoolItem(
                                    BufferTools.CreateBuffer(arr.GetType().GetElementType(), ArrayUtil.GetArrayLengths(arr), context, queue, BufferStrategies.ToOpenCLMemoryFlags(mergedFlags)), 
                                    queue,
                                    mergedFlags,
                                    addressSpace.AddressSpace, 
                                    transferMode.Mode,                                     
                                    readMode,
                                    writeMode,
                                    parameter.IsReturned)            
            if BufferStrategies.ShouldInitBuffer(bufferItem.Buffer, addressSpace.AddressSpace, transferMode.Mode) then
                Console.WriteLine("Buffer is initialised")   
                BufferTools.WriteBuffer(queue, (writeMode = BufferWriteMode.MapBuffer), bufferItem.Buffer, arr)    
            else
                Console.WriteLine("Buffer is NOT initialised")   
                                           
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
        Console.WriteLine("Request to create an UNTRACKED buffer")
        // Get parameter meta
        let transferMode = parameter.Meta.Get<TransferModeAttribute>()
        let rMode = parameter.Meta.Get<BufferReadModeAttribute>()
        let wMode = parameter.Meta.Get<BufferWriteModeAttribute>()
        let addressSpace = parameter.Meta.Get<AddressSpaceAttribute>()
        let memoryFlags = parameter.Meta.Get<MemoryFlagsAttribute>()
        let mergedFlags, readMode, writeMode = 
            BufferStrategies.DetermineBestFlagsAndReadWriteMode(
                                addressSpace.AddressSpace, 
                                parameter.Access,
                                rMode.Mode,
                                wMode.Mode,
                                transferMode.Mode,
                                memoryFlags.Flags, 
                                isRoot, 
                                parameter.IsReturned,
                                false,
                                queue.Device)
                            
        Console.WriteLine("Best Memory Flags: " + mergedFlags.ToString())
        Console.WriteLine("Best Read Mode: " + readMode.ToString())
        Console.WriteLine("Best Write Mode: " + writeMode.ToString())

        if not (untrackedBufferPool.ContainsKey(context)) then
            untrackedBufferPool.Add(context, new Dictionary<OpenCLBuffer, BufferPoolItem>())

        let bufferItem = new BufferPoolItem(
                            BufferTools.CreateBuffer(parameter.DataType.GetElementType(), count, context, queue, BufferStrategies.ToOpenCLMemoryFlags(mergedFlags)), 
                            queue,
                            mergedFlags,
                            addressSpace.AddressSpace, 
                            transferMode.Mode,
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
        Console.WriteLine("Request to create a TRACKED buffer from RETURNED buffer")
        let transferMode = parameter.Meta.Get<TransferModeAttribute>()
        let rMode = parameter.Meta.Get<BufferReadModeAttribute>()
        let wMode = parameter.Meta.Get<BufferWriteModeAttribute>()
        let addressSpace = parameter.Meta.Get<AddressSpaceAttribute>()
        let memoryFlags = parameter.Meta.Get<MemoryFlagsAttribute>()
        let mergedFlags, readMode, writeMode = 
            BufferStrategies.DetermineBestFlagsAndReadWriteMode(
                                addressSpace.AddressSpace, 
                                parameter.Access,
                                rMode.Mode,
                                wMode.Mode,
                                transferMode.Mode,
                                memoryFlags.Flags, 
                                isRoot, 
                                parameter.IsReturned,
                                true,
                                queue.Device)
        Console.WriteLine("Best Memory Flags: " + mergedFlags.ToString())
        Console.WriteLine("Best Read Mode: " + readMode.ToString())
        Console.WriteLine("Best Write Mode: " + writeMode.ToString())
        
        if retArr.IsSome then
            // Return is tracked
            let retItem = trackedBufferPool.[retArr.Value]
            assert(retItem.IsAvailable)                   
            // Check if this can be used with no copy
            let mutable avoidCopy = context = retBuffer.Context
            Console.WriteLine("Returned buffer is TRACKED")
            if context = retBuffer.Context &&
                BufferStrategies.AreMemoryFlagsCompatible(retItem.Flags, mergedFlags, sharePriority) then                
                Console.WriteLine("Returned buffer is reused with adapteded flags: " + retItem.Flags.ToString())   
                trackedBufferPool.[retBuffer].IsAvailable <- false
                // Since it's tracked it could be returned
                if parameter.IsReturned && isRoot then
                    rootReturnBuffer <- Some(retItem, newArr)
                retBuffer
            else
                // Need to copy
                Console.WriteLine("No adapted flags can be computed, create new buffer")   
                let copy = this.CreateTrackedBuffer(context, queue, parameter, retArr.Value, isRoot, sharePriority)
                if BufferStrategies.ShouldCopyBuffer(retItem.Buffer, retItem.AddressSpace, copy, addressSpace.AddressSpace) then
                    Console.WriteLine("Buffer is copied")   
                    BufferTools.CopyBuffer(queue, retBuffer, copy)
                else
                    Console.WriteLine("Buffer is NOT copied")   
                // Dispose old buffer
                trackedBufferPool.[retBuffer].IsAvailable <- true
                this.EndUsingBuffer(trackedBufferPool.[retBuffer].Buffer)
                copy
        else
            // Return is untracked
            let retItem = untrackedBufferPool.[retBuffer.Context].[retBuffer]
            assert(retItem.IsAvailable)  
            Console.WriteLine("Returned buffer is UNTRACKED")                 
            // Check if this can be used with no copy
            let mutable avoidCopy = context = retBuffer.Context
            if context = retBuffer.Context &&
                BufferStrategies.AreMemoryFlagsCompatible(retItem.Flags, mergedFlags, sharePriority) then
                Console.WriteLine("Returned buffer is promoted to TRACKED with adapteded flags: " + retItem.Flags.ToString())  
                // Must promote this untracked item to tracked
                retItem.IsAvailable <- false
                untrackedBufferPool.[retBuffer.Context].Remove(retBuffer) |> ignore
                trackedBufferPool.Add(newArr, retItem)
                // Since now it's tracked it could be returned
                if parameter.IsReturned && isRoot then
                    rootReturnBuffer <- Some(retItem, newArr)
                retBuffer
            else
                Console.WriteLine("No adapted flags can be computed, create new buffer")   
                let poolItem = untrackedBufferPool.[retBuffer.Context].[retBuffer]
                // Need to copy
                let copy = this.CreateTrackedBuffer(context, queue, parameter, newArr, isRoot, sharePriority)
                if BufferStrategies.ShouldCopyBuffer(poolItem.Buffer, poolItem.AddressSpace, copy, addressSpace.AddressSpace) then
                    Console.WriteLine("Buffer is copied")   
                    BufferTools.CopyBuffer(queue, retBuffer, copy)
                else
                    Console.WriteLine("Buffer is NOT copied")   
                // Dispose old buffer
                poolItem.IsAvailable <- true
                this.EndUsingBuffer(trackedBufferPool.[retBuffer].Buffer)
                copy
             
    // This is called for buffers bound to subkernels and not returned or returned from non-root   
    member this.CreateUntrackedBufferFromReturn(retBuffer:OpenCLBuffer, 
                                                retArr: Array option,
                                                context: OpenCLContext, 
                                                queue: OpenCLCommandQueue, 
                                                parameter: IFunctionParameter, 
                                                isRoot: bool,
                                                sharePriority: BufferSharePriority) =
        Console.WriteLine("Request to create an UNTRACKED buffer from RETURNED buffer")
        let transferMode = parameter.Meta.Get<TransferModeAttribute>()
        let rMode = parameter.Meta.Get<BufferReadModeAttribute>()
        let wMode = parameter.Meta.Get<BufferWriteModeAttribute>()
        let addressSpace = parameter.Meta.Get<AddressSpaceAttribute>()
        let memoryFlags = parameter.Meta.Get<MemoryFlagsAttribute>()
        let mergedFlags, readMode, writeMode = 
            BufferStrategies.DetermineBestFlagsAndReadWriteMode(
                                    addressSpace.AddressSpace, 
                                    parameter.Access,
                                    rMode.Mode,
                                    wMode.Mode,
                                    transferMode.Mode,
                                    memoryFlags.Flags, 
                                    isRoot, 
                                    parameter.IsReturned,
                                    true,
                                    queue.Device)
        
        if retArr.IsSome then
            Console.WriteLine("Returned buffer is TRACKED")
            // Return is tracked
            let retItem = trackedBufferPool.[retArr.Value]
            assert(retItem.IsAvailable)                   
            // Check if this can be used with no copy
            let mutable avoidCopy = context = retBuffer.Context
            if context = retBuffer.Context &&
                BufferStrategies.AreMemoryFlagsCompatible(retItem.Flags, mergedFlags, sharePriority) then
                Console.WriteLine("This buffer is promoted to TRACKED with adapteded flags: " + retItem.Flags.ToString())  
                // An untracked using a tracked remains tracked, so simply returns
                trackedBufferPool.[retArr.Value].IsAvailable <- false
                retBuffer
            else
                Console.WriteLine("No adapted flags can be computed, create new buffer")   
                // Need to copy
                let copy = this.CreateUntrackedBuffer(context, queue, parameter, retBuffer.Count, isRoot)
                if BufferStrategies.ShouldCopyBuffer(retBuffer, retItem.AddressSpace, copy, addressSpace.AddressSpace) then
                    Console.WriteLine("Buffer is copied")    
                    BufferTools.CopyBuffer(queue, retBuffer, copy)
                else
                    Console.WriteLine("Buffer is not copied")
                // Dispose old buffer
                trackedBufferPool.[retBuffer].IsAvailable <- true
                this.EndUsingBuffer(trackedBufferPool.[retBuffer].Buffer)
                copy
        else
            Console.WriteLine("Returned buffer is UNTRACKED")
            // Return is untracked
            let retItem = untrackedBufferPool.[retBuffer.Context].[retBuffer]
            assert(retItem.IsAvailable)                   
            // Check if this can be used with no copy
            let mutable avoidCopy = context = retBuffer.Context
            if context = retBuffer.Context &&
                BufferStrategies.AreMemoryFlagsCompatible(retItem.Flags, mergedFlags, sharePriority) then
                Console.WriteLine("Buffer is reused with adapteded flags: " + retItem.Flags.ToString()) 
                retItem.IsAvailable <- false
                retBuffer
            else
                Console.WriteLine("No adapted flags can be computed, create new buffer")  
                // Need to copy
                let copy = this.CreateUntrackedBuffer(context, queue, parameter, retBuffer.Count, isRoot)
                if BufferStrategies.ShouldCopyBuffer(retItem.Buffer, retItem.AddressSpace, copy, addressSpace.AddressSpace) then
                    BufferTools.CopyBuffer(queue, retBuffer, copy)
                // Dispose old buffer
                retItem.IsAvailable <- true
                this.EndUsingBuffer(trackedBufferPool.[retBuffer].Buffer)
                copy

    member this.TransferBackModifiedBuffers() =
        // Read back tracked modified buffers
        for item in trackedBufferPool do
            let poolItem = item.Value
            if BufferStrategies.ShouldReadBackBuffer(poolItem.Buffer, poolItem.AddressSpace, poolItem.TransferMode) then
                BufferTools.ReadBuffer(poolItem.Queue, poolItem.ReadMode = BufferReadMode.MapBuffer, item.Key :?> Array, poolItem.Buffer)
        
    member this.ReadRootBuffer() =
        // Read back root return buffer
        if rootReturnBuffer.IsSome then
            let buff, arr = rootReturnBuffer.Value
            if (BufferStrategies.ShouldReadBackBuffer(buff.Buffer, buff.AddressSpace, buff.TransferMode)) then
                // If it's using UseHostPointer then arr already contains the result
                if buff.Flags &&& MemoryFlags.UseHostPointer |> int = 0 then
                    BufferTools.ReadBuffer(buff.Queue, buff.ReadMode = BufferReadMode.MapBuffer, arr, buff.Buffer)
                arr
            else
                arr
        else
            null
        
    member this.EndUsingBuffer(buffer) =        
        if reverseTrackedBufferPool.ContainsKey(buffer) then
            // Tracked buffer
            let item = trackedBufferPool.[reverseTrackedBufferPool.[buffer]]
            item.IsAvailable <- true
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
                        untrackedBufferPool.[buffer.Context].Remove(buffer) |> ignore
                else 
                    // A buffer not tracked cannot be used elsewhere by other kernels or applications
                    buffer.Dispose()
                    untrackedBufferPool.[buffer.Context].Remove(buffer) |> ignore

    member this.Dispose() =
        for item in untrackedBufferPool do
            for it in item.Value do
                it.Key.Dispose()
        untrackedBufferPool.Clear()
        for item in trackedBufferPool do
            item.Value.Buffer.Dispose()
        trackedBufferPool.Clear()
        reverseTrackedBufferPool.Clear()
        
    member this.RequireBufferForParameter(par: IFunctionParameter, 
                                          arg: Array option, 
                                          count: int64[],
                                          context: OpenCLContext, 
                                          queue: OpenCLCommandQueue, 
                                          isRoot: bool,
                                          sharePriority: BufferSharePriority,
                                          ?fromOutput: ExecutionOutput) =
        Console.WriteLine("\n----------------------------------------------")
        Console.WriteLine("Required buffer for parameter " + par.Name)
        // Check if fromOutput is set
        let bf = 
            if fromOutput.IsNone then
                Console.WriteLine("The parameter is NOT bound to a subkernel result")
                // We have not to use a returned buffer
                // Check if arg is set (in this case arg is visible to host or in any case we must write back buffer to arg if buffer modified)
                if arg.IsSome then            
                    Console.WriteLine("The parameter is bound to an actual argument: create tracked buffer")
                    // Create a buffer bound to the argument
                    this.CreateTrackedBuffer(context, queue, par, arg.Value, isRoot, sharePriority)
                else if par.IsDynamicParameter then
                    // This is a paramter generated from an allocation inside kernel
                    // If not returned or returned from a non root kernel, this is not visible outside the computation, so untracked
                    if par.IsReturned && isRoot then
                        Console.WriteLine("The parameter is allocated inside kernel and it is returned to the host, create tracked buffer")
                        // Tracked
                        let elemType = par.DataType.GetElementType()
                        let storeArray = Array.CreateInstance(elemType, count)
                        let buff = this.CreateTrackedBuffer(context, queue, par, storeArray, isRoot, sharePriority)                       
                        buff
                    else
                        Console.WriteLine("The parameter is allocated inside kernel but returned or returned in a subkernel create untracked buffer")
                        // Untracked
                        let buff = this.CreateUntrackedBuffer(context, queue, par, count, isRoot)                       
                        buff
                else
                    raise (KernelSetupException("Cannot create a buffer for paramter " + par.Name))
            else
                Console.WriteLine("The parameter is bound to a subkernel result")
                // We must use the output of another kernel
                // If this paramter is returned and kernel is root then this can be visible to the host
                if par.IsReturned && isRoot then
                    Console.WriteLine("The parameter is returned to the host")
                    match fromOutput.Value with
                    | ReturnedTrackedBuffer(b, arr) ->
                        // There is already a tracking array, do not allocate another one
                        if arg.IsNone then
                            // Do not have an arg
                            let elemType = par.DataType.GetElementType()
                            let storeArray = Array.CreateInstance(elemType, count)
                            let buff = this.CreateTrackedBufferFromReturn(b, Some(arr), null, context, queue, par, isRoot, sharePriority)            
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
                        raise (KernelSetupException("Only root kernel can return a value object (ReturnedValue(o))!"))
                else
                    Console.WriteLine("The parameter is NOT returned to the host")
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
                        raise (KernelSetupException("Only root kernel can return a value object (ReturnedValue(o))!"))

        Console.WriteLine("----------------------------------------------\n")
        bf
                

        

