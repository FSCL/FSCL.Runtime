namespace FSCL.Runtime.Managers

open FSCL.Compiler
open FSCL.Compiler.Language
open FSCL.Runtime
open FSCL.Runtime.Language
open System
open OpenCL
open Microsoft.FSharp.Core.LanguagePrimitives

type BufferStrategies() =
    static member ToOpenCLMemoryFlags(flags: MemoryFlags) =
        // Zero out the MemoryFlags.None cause in OpenCL this flag is 0L
        EnumOfValue<int64, OpenCL.OpenCLMemoryFlags> ((flags &&& (~~~ MemoryFlags.None)) |> int64)

    static member AreMemoryFlagsCompatible(given: MemoryFlags, required: MemoryFlags, sharePriority: BufferSharePriority) =
        if given = required then
            true
        else
            // Remove readonly, readwrite, writeonly, hostreadonly, hostwriteonly and hostnoaccess bits
            let givenWithNoAccess = MemoryFlagsUtil.WithNoAccessFlags(given)
            let requiredWithNoAccess = MemoryFlagsUtil.WithNoAccessFlags(required)
            // Check that the remaining bits match
            if givenWithNoAccess <> requiredWithNoAccess && (sharePriority = BufferSharePriority.PriorityToFlags) then
                // If priority to flags then not compatible, otherwise yes
                false
            else 
                // Read/Write unrelated bits are equal, let's check if related ones are compatible
                let kernelAccessOk =
                    // If a readwrite is given, then ok
                    if MemoryFlagsUtil.CanKernelReadAndWrite(given) then
                        true
                    // Otherwise must be both read only or both write only
                    else 
                        MemoryFlagsUtil.OnlyKernelAccessFlags(given) = MemoryFlagsUtil.OnlyKernelAccessFlags(required)                    
                // Host-side access
                let hostAccessOk =
                    // If nothing is given then ok (the given host is readwrite)
                    if MemoryFlagsUtil.CanHostReadAndWrite(given) then
                        true
                    // If given is readonly or writeonly and the required is noaccess then ok
                    else if (MemoryFlagsUtil.CanHostRead(given) || MemoryFlagsUtil.CanHostWrite(given)) && (required &&& MemoryFlags.HostNoAccess |> int > 0) then
                        true
                    // Otherwise must be both read only or both write only
                    else 
                        MemoryFlagsUtil.OnlyHostAccessFlags(given) = MemoryFlagsUtil.OnlyHostAccessFlags(required)                    
                kernelAccessOk && hostAccessOk
                    
    static member DetermineBestFlagsAndReadWriteMode(space: AddressSpace, 
                                                     accessMode: AccessAnalysisResult, 
                                                     readMode: BufferReadMode, 
                                                     writeMode: BufferWriteMode, 
                                                     transferMode: TransferMode,
                                                     flags: MemoryFlags, 
                                                     isRoot: bool, 
                                                     isReturn: bool,
                                                     isVisibleToHost: bool,
                                                     dev: OpenCLDevice) =

        // Convert wrapper-independant flags to wrapper-dependant flags
        let mutable optFlags = flags
        let mutable optReadMode = readMode
        let mutable optWriteMode = writeMode

        // Access strategies
        // Override with flags from access analysis is no read/write/readwrite access set
        if MemoryFlagsUtil.OnlyKernelAccessFlags(optFlags) |> int = 0 then
            // If the address space is Constant we set always ReadOnly cause kernel cannot write the buffer
            if space = AddressSpace.Constant then 
                optFlags <- optFlags ||| MemoryFlags.ReadOnly
            else
                // If not isRoot && isReturn, then it is expected that this buffer will be written by the current kernel and read by another
                // To avoid copy cause incompatible flags, we set ReadWrite for this buffer
                if (not isRoot) && isReturn then
                    optFlags <- optFlags ||| MemoryFlags.ReadWrite
                // Otherwise let's look at access analysis
                else 
                    match accessMode with
                    | AccessAnalysisResult.ReadAccess ->
                        optFlags <- optFlags ||| MemoryFlags.ReadOnly
                    | AccessAnalysisResult.WriteAccess ->
                        optFlags <- optFlags ||| MemoryFlags.WriteOnly
                    | _ ->
                        optFlags <- optFlags ||| MemoryFlags.ReadWrite
        
        // Allocation and read/write mode strategies
        // #1: If the buffer is visible to host, is writable, user
        //     did not request to prevent transfer back and the read strategy is map, then UseHostPointer
        //     This is because the host will need to access a T[] after completion, but mapping in managed
        //     environment requires a copy from IntPtr to T[]. It's better for the managed env to allocate T[] and
        //     pass the IntPtr, so there is no need to copy back.
        if (transferMode &&& TransferMode.NoTransferBack |> int = 0) &&
           (MemoryFlagsUtil.WithNoAccessFlags(optFlags) |> int = 0) &&
           MemoryFlagsUtil.CanKernelWrite(optFlags) then
            // Programmer did not prevent transferring back this buffer
            // Programmer did not force a memory flag
            // The buffer is potentially written
            if isVisibleToHost && (optReadMode = BufferReadMode.MapBuffer) then
                // Buffer will be visible to host and programmer asks to map it to read it
                optFlags <- optFlags ||| MemoryFlags.UseHostPointer

        // #2: If the device is a CPU it's better to use UseHostPointer if the buffer is visible to host, 
        //     AllocHostPointer otherwise
        if dev.Type = OpenCLDeviceType.Cpu then
            if MemoryFlagsUtil.WithNoAccessFlags(optFlags) |> int = 0 then
                // Programmer did not force any alloc flags
                if isVisibleToHost then
                    // The buffer is visible to host
                    optFlags <- MemoryFlagsUtil.OnlyAccessFlags(optFlags) ||| MemoryFlags.UseHostPointer
                else
                    // The buffer is visible to host
                    optFlags <- MemoryFlagsUtil.OnlyAccessFlags(optFlags) ||| MemoryFlags.AllocHostPointer
                
            // If read mode is auto set it to map buffer (same for write)
            if optReadMode |> int = 0 then
                optReadMode <- BufferReadMode.MapBuffer
            if optWriteMode |> int  = 0 then
                optWriteMode <- BufferWriteMode.MapBuffer

        optFlags, optReadMode, optWriteMode

    static member ShouldInitBuffer(analysis: AccessAnalysisResult, flags: OpenCLMemoryFlags, space: AddressSpace, transferMode: TransferMode) =
        analysis &&& AccessAnalysisResult.ReadAccess |> int > 0 && 
        space <> AddressSpace.Private &&
        space <> AddressSpace.Local &&
        (flags &&& (OpenCLMemoryFlags.CopyHostPointer ||| OpenCLMemoryFlags.UseHostPointer) |> int = 0) &&
        ((transferMode &&& TransferMode.NoTransfer) |> int = 0)
        
    static member ShouldReadBackBuffer(analysis: AccessAnalysisResult, flags: OpenCLMemoryFlags, space: AddressSpace, transferMode: TransferMode) =
        analysis &&& AccessAnalysisResult.WriteAccess |> int > 0 &&
        space <> AddressSpace.Local &&
        space <> AddressSpace.Private &&
        space <> AddressSpace.Constant &&
        (flags &&& OpenCLMemoryFlags.UseHostPointer) |> int = 0 &&
        ((transferMode &&& TransferMode.NoTransferBack) |> int = 0)   
        
    static member ShouldCopyBuffer(fromAnalysis:AccessAnalysisResult, fromSpace: AddressSpace,
                                   toAnalysis: AccessAnalysisResult, toSpace: AddressSpace) =
        fromAnalysis &&& AccessAnalysisResult.WriteAccess |> int > 0  && 
        toAnalysis &&& AccessAnalysisResult.ReadAccess |> int > 0 &&
        fromSpace <> AddressSpace.Local &&
        fromSpace <> AddressSpace.Private &&
        fromSpace <> AddressSpace.Constant &&
        toSpace <> AddressSpace.Local &&
        toSpace <> AddressSpace.Private 