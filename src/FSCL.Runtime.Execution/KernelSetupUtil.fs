namespace FSCL.Runtime.RuntimeSteps

open OpenCL
open FSCL
open FSCL.Compiler.Util
open FSCL.Runtime
open FSCL.Language
open FSCL.Runtime.Managers
open Microsoft.FSharp.Quotations
open System
open System.Reflection
open System.Collections.Generic
open Microsoft.FSharp.Linq.RuntimeHelpers
open System.Threading

type MultithreadWorkItemInfo(globalID: int64[], globalSize: int64[], globalOffset: int64[], localBarrier: Barrier) =
    inherit WorkSize(globalSize, globalSize, globalOffset)
    
    override this.GlobalID(idx) =
        globalID.[idx] |> int      
    override this.LocalID(idx) =
        globalID.[idx] |> int      
    override this.Barrier(memFence) =
        localBarrier.SignalAndWait()

module KernelSetupUtil =
    let ComputeLocalSizeWithGlobalSize(k: OpenCLKernel, d:OpenCLDevice, globalSize: int64[]) =
        let localSize = Array.create globalSize.Length (k.GetWorkGroupSize(d))
        for i = 0 to globalSize.Length - 1 do
            while globalSize.[i] % localSize.[i] <> 0L do
                localSize.[i] <- localSize.[i] - 1L
        localSize
        
    let ComputeGlobalSizeWithLocalSize(k: OpenCLKernel, d:OpenCLDevice, globalSize: int64[], localSize: int64[]) =
        let finalGlobalSize = Array.init localSize.Length (fun i -> ((((globalSize.[i] - 1L) / localSize.[i])) + 1L) * localSize.[i])
        finalGlobalSize

    let NormalizeWorkSize(globalSize: int64[], localSize: int64[], globalOffset: int64[]) =        
        let gs = Array.init (3) (fun i -> 
                                    if globalSize <> null && i < globalSize.Length then
                                        globalSize.[i] 
                                    else 
                                        1L)
        let go = Array.init (3) (fun i -> 
                                    if globalOffset <> null && i < globalOffset.Length then
                                        globalOffset.[i] 
                                    else 
                                        0L)
        let ls = Array.init (3) (fun i -> 
                                    if localSize <> null && i < localSize.Length then
                                        localSize.[i] 
                                    else 
                                        1L)
        gs, ls, go
        
    let PrepareKernelAguments(node: OpenCLKernelFlowGraphNode, 
                              nodeInput: IReadOnlyDictionary<string, FlowGraphNodeInput>, 
                              inputFromOtherKernels: IReadOnlyDictionary<string, ExecutionOutput>,
                              workSize: WorkSize option,
                              pool: BufferPoolManager,
                              isRoot: bool,
                              sharePriority: BufferSharePriority) =   
        let buffers = new Dictionary<string, OpenCLBuffer>()
        let arguments = new Dictionary<string, obj>()
        let mutable outputFromThisKernel = ReturnedValue(())

        let mutable argIndex = 0            
        // Foreach argument of the kernel
        for par in node.KernelData.Kernel.Parameters do      
            if par.DataType.IsArray then     
                // If the parameter is an array the argument value can be:
                // 1) ActualArgument: the argument value is given by the user that invokes the kernel
                // 2) KernelOutput: in this case it has been already evaluated and stored in arguments
                // 3) CompilerPrecomputedValue: the argument value is established by the compiler (local arguments)
                // 4) ReturnedBufferAllocationSize: the argument is the size of a buffer to be returned
                // 5) ImplicitValue: the runner should be able to determine which kind of argument is this        
                match nodeInput.[par.Name] with
                | BufferAllocationSize(sizeFunction) ->
                    // Buffer allocated in the body of the kernel are traslated into additional arguments
                    // To setup these arguments in OpenCL, the buffers must be pre-allocated
                    let globalSize, localSize, globalOffset = 
                        if workSize.IsNone then
                            null, null, null
                        else
                            workSize.Value.GlobalSize(), workSize.Value.LocalSize(), workSize.Value.GlobalOffset()
                    let elementCount = 
                        try
                            sizeFunction(arguments, 
                                         globalSize, localSize, globalOffset)                            
                        with 
                        | ex ->
                            raise (new KernelSetupException("Cannot evaluate a BufferAllocationSize input for kernels whose WorkSize is not explicit"))
                    let elementType = par.DataType.GetElementType()
                                        
                    // Store dim sizes
                    let sizeParameters = par.SizeParameters
                    for i = 0 to sizeParameters.Count - 1 do
                        arguments.Add(sizeParameters.[i].Name, elementCount.[i])
                        
                    // Allocate the buffer
                    let buffer = pool.RequireBufferForParameter(par, None, elementCount, node.DeviceData.Context, node.DeviceData.Queue, isRoot, sharePriority)
                    
                    // Set kernel arg
                    node.CompiledKernelData.Kernel.SetMemoryArgument(argIndex, buffer)                      
                    // Store buffer/object data
                    arguments.Add(par.Name, buffer)
                    buffers.Add(par.Name, buffer)
            
                    // Set if returned
                    if par.IsReturned then
                        outputFromThisKernel <- ReturnedUntrackedBuffer(buffer)

                | ActualArgument(expr) ->
                    // Input from an actual argument
                    let o = 
                        match expr with
                        | Patterns.Call(o, mi, a) ->
                            if mi.GetCustomAttribute<FSCL.VectorTypeArrayReinterpretAttribute>() <> null then
                                // Reinterpretation of non-vector array 
                                LeafExpressionConverter.EvaluateQuotation(a.[0])
                            else
                                LeafExpressionConverter.EvaluateQuotation(expr)
                        | _ ->
                            LeafExpressionConverter.EvaluateQuotation(expr)
                    // Create buffer if needed (no local address space)
                    let addressSpace = par.Meta.Get<AddressSpaceAttribute>()
                    if addressSpace.AddressSpace = AddressSpace.Local then
                        node.CompiledKernelData.Kernel.SetLocalArgument(argIndex, ArrayUtil.GetArrayAllocationSize(o) |> int64) 
                        // Store dim sizes
                        let sizeParameters = par.SizeParameters
                        //bufferSizes.Add(par.Name, new Dictionary<string, int>())
                        let lengths = ArrayUtil.GetArrayLengths(o)
                        for i = 0 to sizeParameters.Count - 1 do
                            //bufferSizes.[par.Name]
                            arguments.Add(sizeParameters.[i].Name, lengths.[i])
                    else
                        let lengths = ArrayUtil.GetArrayLengths(o)

                        // Check if read or read_write mode
                        let buffer = pool.RequireBufferForParameter(par, Some(o :?> Array), lengths, node.DeviceData.Context, node.DeviceData.Queue, isRoot, sharePriority) 
                        
                        // Set kernel arg
                        node.CompiledKernelData.Kernel.SetMemoryArgument(argIndex, buffer)                      
                        // Store dim sizes
                        let sizeParameters = par.SizeParameters
                        for i = 0 to sizeParameters.Count - 1 do
                            arguments.Add(sizeParameters.[i].Name, lengths.[i])
                        // Store argument and buffer
                        arguments.Add(par.Name, o)
                        buffers.Add(par.Name, buffer)
                        // Check if this is returned
                        if par.IsReturned then
                            outputFromThisKernel <- ReturnedTrackedBuffer(buffer, o :?> Array)
                           
                | KernelOutput(othNode, a) ->
                    // Copy the output buffer of the input kernel     
                    let elementType = par.DataType.GetElementType()  
                    let lengths = 
                        match inputFromOtherKernels.[par.Name] with
                        | ReturnedUntrackedBuffer(b)
                        | ReturnedTrackedBuffer(b,_) ->
                            b.Count
                        | ReturnedValue(o) ->
                            if o.GetType().IsArray then
                                ArrayUtil.GetArrayLengths(o)
                            else
                                [||]
                                
                    let buffer = pool.RequireBufferForParameter(par, None, lengths, node.DeviceData.Context, node.DeviceData.Queue, isRoot, sharePriority, inputFromOtherKernels.[par.Name]) 
                            
                    // Store dim sizes
                    let sizeParameters = par.SizeParameters
                    //bufferSizes.Add(par.Name, new Dictionary<string, int>())
                    for i = 0 to sizeParameters.Count - 1 do
                        arguments.Add(sizeParameters.[i].Name, buffer.Count.[i])
                                
                    node.CompiledKernelData.Kernel.SetMemoryArgument(argIndex, buffer)                      
                    // Store buffer/object data
                    arguments.Add(par.Name, buffer)
                    buffers.Add(par.Name, buffer)
                    // Check if this is returned
                    if par.IsReturned then
                        outputFromThisKernel <- ReturnedUntrackedBuffer(buffer)
                | _ ->
                    raise (new KernelSetupException("The parameter " + par.Name + " is considered as implicit, which means that the runtime should be able to provide its value automatically, but this can't be done for array parameters"))
            // Scalar parameter
            else
                // Check if this is an argument automatically inserted to represent the length af an array parameter
                if par.IsSizeParameter then
                    let v = arguments.[par.Name]             
                    node.CompiledKernelData.Kernel.SetValueArgument<int>(argIndex, v :?> int64 |> int)
                else
                    // If the parameter is not an array nor a size parameter, it can be:
                    // 1) ActualArgument: a simples scalar value given by the user
                    // 2) KernelOutput: NOT POSSIBLE
                    // 4) ReturnedBufferAllocationSize: NOT POSSIBLE
                    // 5) ImplicitValue: NOT POSSIBLE       
                    match nodeInput.[par.Name] with
                    | ActualArgument(e) ->
                        let o = LeafExpressionConverter.EvaluateQuotation(e)
                        node.CompiledKernelData.Kernel.SetValueArgumentAsObject(argIndex, o)
                    // | CompilerPrecomputedValue(computeFunction) ->
                        //   let o = computeFunction(arguments, workSize.GlobalSize, workSize.LocalSize)
                        // node.CompiledKernelData.Kernel.SetValueArgumentAsObject(argIndex, o)
                    | KernelOutput(_, _) ->
                        raise (new KernelSetupException("The scalar parameter " + par.Name + " is bound to the output of a kernel, but only vector parameters (arrays) can be bound to the output of a kernel"))
                    | _ ->
                        raise (new KernelSetupException("The parameter " + par.Name + " is considered as implicit, which means that the runtime should be able to provide its value automatically, but this is not valid except for size parameters"))
                        
            // Process next parameter
            argIndex <- argIndex + 1
        arguments, buffers, outputFromThisKernel
