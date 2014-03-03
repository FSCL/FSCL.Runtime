namespace FSCL.Runtime.KernelExecution

open System
open FSCL.Compiler
open FSCL.Runtime
open System.Collections.Generic
open System.Reflection
open Cloo
open Microsoft.FSharp.Linq.RuntimeHelpers

[<assembly:DefaultComponentAssembly>]
do()

[<StepProcessor("FSCL_RUNTIME_EXECUTION_DEFAULT_PROCESSOR", "FSCL_RUNTIME_EXECUTION_STEP")>]
type DefaultKerernelExecutionProcessor() =      
    inherit CompilerStepProcessor<KernelExecutionInput, KernelExecutionOutput option>()
    
    override this.Run(input, s, opts) =
        let step = s :?> KernelExecutionStep
        let pool = step.BufferPoolManager

        let isRoot = input.IsRoot
        let node = input.Node
        let deviceData, kernelData, compiledData = input.RuntimeInfo.[input.Node.KernelID]
        let gSize = input.GlobalSize
        let lSize = input.LocalSize

        // If globalSize or localSize are 0-length they should be retrieved in the kernel expression, i.e. in the graph node custom info
        let globalSize =
            if gSize.Length = 0 then
                if node.CustomInfo.ContainsKey("GLOBAL_SIZE") then
                    node.CustomInfo.["GLOBAL_SIZE"] :?> int64 array
                else
                    raise (new KernelSetupException("No runtime global size value has been specified and no global size information can be found inside the kernel expression"))
            else
                gSize
        let localSize =
            if lSize.Length = 0 then
                if node.CustomInfo.ContainsKey("LOCAL_SIZE") then
                    node.CustomInfo.["LOCAL_SIZE"] :?> int64 array
                else
                    raise (new KernelSetupException("No runtime local size value has been specified and no local size information can be found inside the kernel expression"))
            else
                lSize
                
        // Input (coming from preceding kernels) buffers
        let inputFromOtherKernelsBuffers = new Dictionary<String, ComputeMemory>()
        let buffers = new Dictionary<String, ComputeMemory>()
        let mutable returnBuffer = null
        let arguments = new Dictionary<string, obj>()
        
        // Get node input binding
        let nodeInput = FlowGraphManager.GetNodeInput(node)
        // Check which parameters are bound to the output of a kernel
        for par in kernelData.Info.Parameters do
            if nodeInput.ContainsKey(par.Name) then
                match nodeInput.[par.Name] with
                | KernelOutput(otherKernel, otherParIndex) ->
                    let kernelOutput = step.Process(new KernelExecutionInput(false, otherKernel, input.RuntimeInfo, [||], [||]))
                    inputFromOtherKernelsBuffers.Add(par.Name, kernelOutput.ReturnBuffer.Value)    
                | _ ->
                    ()
                    
        // Now that we have executed all the preceding kernels, complete the evaluation of the arguments
        // We do this in a second time to avoid allocating many buffers ina recursive function, risking stack overflow
        let mutable argIndex = 0            
        // Foreach argument of the kernel
        for par in kernelData.Info.Parameters do      
            if par.Type.IsArray then     
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
                    let size = sizeFunction(arguments, globalSize, localSize)
                    let elementType = par.Type.GetElementType()
                    let elementCount = 
                        match size with
                        | ExplicitAllocationSize(sizes) ->                 
                            sizes
                        | BufferReferenceAllocationExpression(bufferName) ->                                    
                            let oth = arguments.[bufferName]
                            // oth can be either an array passed by the user or a buffer (ComputeMemory) passed by a previous kernel
                            if oth :? ComputeMemory then
                                let othBuffer = arguments.[bufferName] :?> ComputeMemory                                    
                                othBuffer.Count
                            else
                                ArrayUtil.GetArrayLengths(oth)
                    
                    // Store dim sizes
                    let sizeParameters = par.SizeParameters
                    //bufferSizes.Add(par.Name, new Dictionary<string, int>())
                    for i = 0 to sizeParameters.Count - 1 do
                        arguments.Add(sizeParameters.[i].Name, elementCount.[i] |> int64)
                        
                    // Allocate the buffer
                    // Since returned, the buffer must not be initialized and it is obiously written
                    let buffer = pool.CreateUntrackedBuffer(deviceData.Context, deviceData.Queue, par, elementCount, BufferTools.KernelParameterAccessModeToFlags(par.Access), isRoot)                            
                    // Set kernel arg
                    compiledData.Kernel.SetMemoryArgument(argIndex, buffer)                      
                    // Store buffer/object data
                    arguments.Add(par.Name, buffer)
                    buffers.Add(par.Name, buffer)
                    // Check if this is returned
                    if par.IsReturnParameter then
                        returnBuffer <- buffer
                | ActualArgument(expr) ->
                    // Input from an actual argument
                    let o = LeafExpressionConverter.EvaluateQuotation(expr)
                    // Create buffer if needed (no local address space)
                    if par.AddressSpace = KernelParameterAddressSpace.LocalSpace then
                        compiledData.Kernel.SetLocalArgument(argIndex, ArrayUtil.GetArrayAllocationSize(o) |> int64) 
                        // Store dim sizes
                        let sizeParameters = par.SizeParameters
                        //bufferSizes.Add(par.Name, new Dictionary<string, int>())
                        let getLengthMethod = o.GetType().GetMethod("GetLongLength")
                        for i = 0 to sizeParameters.Count - 1 do
                            //bufferSizes.[par.Name]
                            arguments.Add(sizeParameters.[i].Name, getLengthMethod.Invoke(o, [| i |]))
                    else
                        // Check if read or read_write mode
                        let access = par.Access
                        let buffer = pool.CreateTrackedBuffer(deviceData.Context, deviceData.Queue, par, o, BufferTools.KernelParameterAccessModeToFlags(access), false) 
                        
                        // Set kernel arg
                        compiledData.Kernel.SetMemoryArgument(argIndex, buffer)                      
                        // Store dim sizes
                        let sizeParameters = par.SizeParameters
                        //bufferSizes.Add(par.Name, new Dictionary<string, int>())
                        let lengths = ArrayUtil.GetArrayLengths(o)
                        for i = 0 to sizeParameters.Count - 1 do
                            //bufferSizes.[par.Name]
                            arguments.Add(sizeParameters.[i].Name, lengths.[i])
                        // Store argument and buffer
                        arguments.Add(par.Name, o)
                        buffers.Add(par.Name, buffer)
                        // Check if this is returned
                        if par.IsReturnParameter then
                            returnBuffer <- buffer
                | KernelOutput(node, a) ->
                    // WE SHOULD AVOID COPY!!!
                    // Copy the output buffer of the input kernel
                    let buffer = pool.UseUntrackedBuffer(inputFromOtherKernelsBuffers.[par.Name], deviceData.Context, deviceData.Queue, par, BufferTools.KernelParameterAccessModeToFlags(par.Access), isRoot)            
                    // Store dim sizes
                    let sizeParameters = par.SizeParameters
                    //bufferSizes.Add(par.Name, new Dictionary<string, int>())
                    for i = 0 to sizeParameters.Count - 1 do
                        arguments.Add(sizeParameters.[i].Name, buffer.Count.[i])
                                
                    compiledData.Kernel.SetMemoryArgument(argIndex, buffer)                      
                    // Store buffer/object data
                    arguments.Add(par.Name, buffer)
                    buffers.Add(par.Name, buffer)
                    // Check if this is returned
                    if par.IsReturnParameter then
                        returnBuffer <- buffer
                | _ ->
                    raise (new KernelSetupException("The parameter " + par.Name + " is considered as implicit, which means that the runtime should be able to provide its value automatically, but this can't be done for array parameters"))
            // Scalar parameter
            else
                // Check if this is an argument automatically inserted to represent the length af an array parameter
                if par.IsSizeParameter then
                    let v = arguments.[par.Name]                      
                    compiledData.Kernel.SetValueArgument<int>(argIndex, v :?> int64 |> int)
                else
                    // If the parameter is not an array nor a size parameter, it can be:
                    // 1) ActualArgument: a simples scalar value given by the user
                    // 2) KernelOutput: NOT POSSIBLE
                    // 3) CompilerPrecomputedValue: a scalar value computed by the compiler
                    // 4) ReturnedBufferAllocationSize: NOT POSSIBLE
                    // 5) ImplicitValue: NOT POSSIBLE       
                    match nodeInput.[par.Name] with
                    | ActualArgument(e) ->
                        let o = LeafExpressionConverter.EvaluateQuotation(e)
                        compiledData.Kernel.SetValueArgumentAsObject(argIndex, o)
                    | CompilerPrecomputedValue(computeFunction) ->
                        let o = computeFunction(arguments, globalSize, localSize)
                        compiledData.Kernel.SetValueArgumentAsObject(argIndex, o)
                    | KernelOutput(_, _) ->
                        raise (new KernelSetupException("The scalar parameter " + par.Name + " is bound to the output of a kernel, but only vector parameters (arrays) can be bound to the output of a kernel"))
                    | _ ->
                        raise (new KernelSetupException("The parameter " + par.Name + " is considered as implicit, which means that the runtime should be able to provide its value automatically, but this is not valid except for size parameters"))
                        
            // Process next parameter
            argIndex <- argIndex + 1

        // Cool, we processed the input and now all the arguments have been set
        // Run kernel
        let offset = Array.zeroCreate<int64>(globalSize.Length)
        // 32 bit enought for size_t. Kernel uses size_t like int without cast. 
        // We cannot put case into F# kernels each time the user does operations with get_global_id and similar!
        deviceData.Queue.Execute(compiledData.Kernel, offset, Array.map(fun el -> int64(el)) globalSize, Array.map(fun el -> int64(el)) localSize, null)

        // Dispose buffers
        for b in buffers do
            pool.DisposeBuffer(b.Value)
        (*
        let returnedObjects = new List<obj>()
        for par in kernelData.Info.Parameters do      
            // Check if this must be read
            // This happens if the buffer have been stored in the outputBuffers dictionary
            // and, if this is a returned buffer, the kernel is the call graph root (value returned to the user)
            if outputBuffers.ContainsKey(par.Name) then
                let buffer, obj = outputBuffers.[par.Name]
                               
                // TEST
                //let o = Array2D.create<float32> 64 64 0.0f
                //BufferTools.ReadBuffer(typeof<float32>, deviceData.Context, deviceData.Queue, o, 2, buffer);
                // If obj is null then the buffer should be returned as part of the F# kernel return value
                // This can happen only if the kernel is the root
                if obj = null then
                    if isRoot then
                        // Allocate array (since if this is a return parameter it has no .NET array matching it)
                        let sizes = 
                            par.SizeParameters |> Seq.map(fun (pInfo:KernelParameterInfo) -> arguments.[pInfo.Name] :?> int64) |> Seq.toArray
                        let obj = Array.CreateInstance(par.Type.GetElementType(), sizes)
                        BufferTools.ReadBuffer(par.Type.GetElementType(), deviceData.Context, deviceData.Queue, obj, par.SizeParameters.Count, fst(outputBuffers.[par.Name]))
                        outputBuffers.Remove(par.Name) |> ignore
                        returnedObjects.Add(obj)
                else
                    // This is not returned but might be read from the user (host side)
                    let buffer, obj = outputBuffers.[par.Name]
                    // Read buffer if not marked wit NoReadBack
                    if not (par.ShouldNoReadBack) then
                        BufferTools.ReadBuffer(par.Type.GetElementType(), deviceData.Context, deviceData.Queue, obj, par.SizeParameters.Count, buffer)
        
        for buff in inputFromOtherKernelBuffers do
            if not (outputBuffers.ContainsKey(buff.Key)) then
                buff.Value.Dispose()
           
        // Also return written buffers
        let outBuffers = new List<ComputeMemory>()
        for k in outputBuffers do
            match k.Value with
            | cb, co ->
                outBuffers.Add(cb) 
        *)           
        // Return the objects that the F# kernels eventually returns as a tuple (if more than 1)
        if returnBuffer <> null then
            Some(new KernelExecutionOutput(returnBuffer))
        else
            Some(new KernelExecutionOutput())
                  