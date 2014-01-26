namespace FSCL.Runtime

open Cloo
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open System.Reflection
open Microsoft.FSharp.Linq.QuotationEvaluation
open FSCL.Compiler
open FSCL.Compiler.KernelLanguage
open System
open System.Collections.Generic
open System.Threading
open System.Collections.ObjectModel
open System.Runtime.InteropServices

module KernelRunner =
    // The Kernel runner
    type internal Runner(compiler, metric) =    
        member val KernelManager = new KernelManager(compiler, metric) with get
                        
        member this.RunOpenCL(isRoot: bool,
                              node: FlowGraphNode,
                              runtimeInfo: Dictionary<MethodInfo, RuntimeDeviceData * RuntimeKernelData * RuntimeCompiledKernelData>,
                              gSize: int array, 
                              lSize: int array) =


            // If globalSize or localSize are 0-length they should be retrieved in the kernel expression, i.e. in the graph node custom info
            let globalSize =
                if gSize.Length = 0 then
                    if node.CustomInfo.ContainsKey("GLOBAL_SIZE") then
                        node.CustomInfo.["GLOBAL_SIZE"] :?> int array
                    else
                        raise (new KernelSetupException("No runtime global size value has been specified and no global size information can be found inside the kernel expression"))
                else
                    gSize
            let localSize =
                if lSize.Length = 0 then
                    if node.CustomInfo.ContainsKey("LOCAL_SIZE") then
                        node.CustomInfo.["LOCAL_SIZE"] :?> int array
                    else
                        raise (new KernelSetupException("No runtime local size value has been specified and no local size information can be found inside the kernel expression"))
                else
                    lSize

            // Get interesting data
            let deviceData, kernelData, compiledData = runtimeInfo.[node.KernelID]

            // Input (coming from preceding kernels) buffers
            let inputBuffers = new Dictionary<String, ComputeMemory>()
            // Output (returned or simply written) buffers
            let outputBuffers = new Dictionary<String, ComputeMemory * obj>()
            // Arguments
            let arguments = new Dictionary<string, obj>()
            // Returned buffers
            let returnedBuffers = new List<ComputeMemory>()
            //let bufferSizes = new Dictionary<string, Dictionary<string, int>>()

            // Get node input binding
            let nodeInput = FlowGraphManager.GetNodeInput(node)
            // Check which parameters are bound to the output of a kernel
            for par in kernelData.Info.Parameters do
                if nodeInput.ContainsKey(par.Name) then
                    match nodeInput.[par.Name] with
                    | KernelOutput(otherKernel, otherParIndex) ->
                        let kernelOutput = this.RunOpenCL(false, otherKernel, runtimeInfo, [||], [||]) :> obj
                        // MUST HANDLE MULTIPLE BUFFERS RETURNED: POSSIBLE?
                        inputBuffers.Add(par.Name, (kernelOutput :?> List<ComputeMemory>).[0])    
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
                    | ReturnedBufferAllocationSize(sizeFunction) ->
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
                                    KernelManagerTools.GetArrayLengths(oth)
                        
                        // Store dim sizes
                        let sizeParameters = par.SizeParameters
                        //bufferSizes.Add(par.Name, new Dictionary<string, int>())
                        for i = 0 to sizeParameters.Count - 1 do
                            arguments.Add(sizeParameters.[i].Name, elementCount.[i] |> int64)
                            
                        // Allocate the buffer
                        // Since returned, the buffer must not be initialized and it is obiously written
                        let buffer = BufferTools.CreateBuffer(elementType, elementCount, deviceData.Context, deviceData.Queue, ComputeMemoryFlags.WriteOnly)                            
                        // Set kernel arg
                        compiledData.Kernel.SetMemoryArgument(argIndex, buffer.Value)                      
                        // Store buffer/object data
                        outputBuffers.Add(par.Name, (buffer.Value, null))
                        arguments.Add(par.Name, buffer.Value)
                        if(par.IsReturnParameter) then
                            returnedBuffers.Add(buffer.Value)
                    | ActualArgument(expr) ->
                        // Input from an actual argument
                        let o = expr.EvalUntyped()
                        // Create buffer if needed (no local address space)
                        if par.AddressSpace = KernelParameterAddressSpace.LocalSpace then
                            compiledData.Kernel.SetLocalArgument(argIndex, KernelManagerTools.GetArrayAllocationSize(o) |> int64) 
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
                            let mustInitBuffer =
                                ((par.AddressSpace = KernelParameterAddressSpace.GlobalSpace) ||
                                    (par.AddressSpace = KernelParameterAddressSpace.ConstantSpace)) &&
                                ((access = KernelParameterAccessMode.ReadOnly) || 
                                    (access = KernelParameterAccessMode.ReadWrite))
                            // Create buffer and eventually init it
                            let elementType = par.Type.GetElementType()
                            let buffer = BufferTools.WriteBuffer(elementType, deviceData.Context, deviceData.Queue, o, par.SizeParameters.Count, mustInitBuffer)                            
                            // Set kernel arg
                            compiledData.Kernel.SetMemoryArgument(argIndex, buffer.Value)                      
                            // Store dim sizes
                            let sizeParameters = par.SizeParameters
                            //bufferSizes.Add(par.Name, new Dictionary<string, int>())
                            let getLengthMethod = o.GetType().GetMethod("GetLongLength")
                            for i = 0 to sizeParameters.Count - 1 do
                                //bufferSizes.[par.Name]
                                arguments.Add(sizeParameters.[i].Name, getLengthMethod.Invoke(o, [| i |]))
                            // Store argument and buffer
                            arguments.Add(par.Name, o)
                            if((par.AddressSpace = KernelParameterAddressSpace.GlobalSpace) &&
                                ((access = KernelParameterAccessMode.WriteOnly) || 
                                 (access = KernelParameterAccessMode.ReadWrite))) then
                                outputBuffers.Add(par.Name, (buffer.Value, o))
                            if(par.IsReturnParameter) then
                                returnedBuffers.Add(buffer.Value)
                    | KernelOutput(node, a) ->
                        // WE SHOULD AVOID COPY!!!
                        // Copy the output buffer of the input kernel
                        let buffer = BufferTools.CopyBuffer(par.Type.GetElementType(), deviceData.Context, deviceData.Queue, inputBuffers.[par.Name])            
                        // Store dim sizes
                        let sizeParameters = par.SizeParameters
                        //bufferSizes.Add(par.Name, new Dictionary<string, int>())
                        for i = 0 to sizeParameters.Count - 1 do
                            arguments.Add(sizeParameters.[i].Name, buffer.Value.Count.[i])
                                    
                        compiledData.Kernel.SetMemoryArgument(argIndex, buffer.Value)                      
                        // Store buffer/object data
                        arguments.Add(par.Name, buffer.Value)
                        if(par.Access = KernelParameterAccessMode.WriteOnly || 
                           par.Access = KernelParameterAccessMode.ReadWrite) then
                            outputBuffers.Add(par.Name, (buffer.Value, null))
                        if(par.IsReturnParameter) then
                            returnedBuffers.Add(buffer.Value)
                    | _ ->
                        raise (new KernelSetupException("The parameter " + par.Name + " is considered as implicit, which means that the runtime should be able to provide its value automatically, but this can't be done for array parameters"))
                // Scalar parameter
                else
                    // Check if this is an argument automatically inserted to represent the length af an array parameter
                    if par.IsSizeParameter then
                        let v = arguments.[par.Name]
                        // Array length should be int64 (the type returned from LongLength method and from Marshal.SizeOf-based operations)
                        // Users might provide int32, therefore check
                        //try                          
                            // Value of this argument stored when the buffer was evaluated (buffer must appear before!)
                          //  compiledData.Kernel.SetValueArgument<int>(argIndex, v :?> int)
                        //with
                        //| :? InvalidCastException ->                            
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
                            let o = e.EvalUntyped()
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

            // Foreach argument of the kernel
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
                            returnedObjects.Add(obj)
                    else
                        // This is not returned but might be read from the user (host side)
                        let buffer, obj = outputBuffers.[par.Name]
                        // Read buffer
                        BufferTools.ReadBuffer(par.Type.GetElementType(), deviceData.Context, deviceData.Queue, obj, par.SizeParameters.Count, buffer)
                        let i = 0
                        ()
                        
            // Return the objects that the F# kernels eventually returns as a tuple (if more than 1)
            if isRoot then
                // Return values to be given back to host side
                if returnedObjects.Count = 0 then
                    () :> obj
                else if returnedObjects.Count = 1 then
                    returnedObjects.[0]
                else
                    FSharpValue.MakeTuple(returnedObjects.ToArray(), FSharpType.MakeTupleType(Array.ofSeq(Seq.map(fun (o:obj) -> o.GetType()) returnedObjects)))
            else
                // Return buffers to be used by other kernels
                returnedBuffers :> obj
                            
        member this.RunMultithread(kernelData: RuntimeKernelData,
                                   compiledData: RuntimeCompiledKernelData,
                                   globalSize: int array, 
                                   localSize: int array, 
                                   multithread: bool) =
            raise (KernelSetupException("Multithreading support under development"))
            (*                                               
            // Normalize dimensions of workspace
            // If the workspace is one dim, treansform into 3 dims with the second and the third equals to 1 (thread)
            let normalizedGlobalSize, normalizedLocalSize = 
                match globalSize.Rank with
                | 1 ->
                    ([| globalSize.[0]; 1; 1 |], [| localSize.[0]; 1; 1 |])
                | 2 ->
                    ([| globalSize.[0]; globalSize.[1]; 1 |], [| localSize.[0]; localSize.[1]; 1 |])
                | _ ->
                    (globalSize, localSize)

            // Analyze connections to build a parameter-based indexed structure
            let ic, oc = this.BuildParameterBasedConnections(inputConnections, outputConnections)

            // The list of arguments of the kernel (that must be passed to each cpu thread)
            let arguments = new List<Object>()
            // The storage for buffers potentially used by successive kernels in the call graph flow
            let mutable argIndex = 0
            // To remember the array associated to the length parameters
            let sizeParametersBinding = new Dictionary<string, int>()
            // Foreach argument of the kernel
            for par in kernelData.Info.Parameters do      
                if par.Type.IsArray then             
                    // Check if this is the input coming from a previously executed kernel
                    if not (ic.ContainsKey(par.Name)) then
                        // No input from a  kernel, there must be an input from an actual argument
                        if par.ArgumentExpression.IsNone then
                            raise (new KernelSetupException("Cannot setup the parameter " + par.Name + " for the kernel " + kernelData.Info.Name + ": the parameter is not bound to any other kernel and no associated actual argument has been found"))
                        let o = par.ArgumentExpression.Value.EvalUntyped()                        
                        // Store buffer/object data
                        if not (bufferBinding.ContainsKey(kernelData.Info.ID)) then
                            bufferBinding.Add(kernelData.Info.ID, new Dictionary<string, ComputeMemory * Object * int array>())
                        bufferBinding.[kernelData.Info.ID].Add(par.Name, (null, o, null))
                        // Add the argument value to the list
                        arguments.Add(o)
                    // We get the input from a previously-executed kernel
                    else
                        let _, inputBuffer, _ = bufferBinding.[fst (ic.[par.Name])].[snd (ic.[par.Name])]
                        // Check if this can be written to be the input of a successive kernel
                        if not (bufferBinding.ContainsKey(kernelData.Info.ID)) then
                            bufferBinding.Add(kernelData.Info.ID, new Dictionary<string, ComputeMemory * Object * int array>())
                        bufferBinding.[kernelData.Info.ID].Add(par.Name, (null, inputBuffer, null))      
                        // Add the argument value to the list
                        arguments.Add(inputBuffer)          
                else
                    // Add the argument value to the list
                    arguments.Add(par.ArgumentExpression.Value.EvalUntyped())
            // Process next parameter
            argIndex <- argIndex + 1

            // Finalize the arguments (List -> array)            
            // Add fake additional element to the list of arguments. It will be replaced with the appropriate instance of WorkItemIdContainer foreach thread executed
            arguments.Add(new Object())
            let finalArguments = Array.ofSeq(arguments)

            // Run kernel
            // Launch threads or execute sequential
            let work = kernelData.MultithreadVersion.Value
            for i = 0 to normalizedGlobalSize.[0] - 1 do
                for j = 0 to normalizedGlobalSize.[1] - 1 do
                    for k = 0 to normalizedGlobalSize.[2] - 1 do
                        // Create a ids container for each thread and run the thread
                        let container = new WorkItemIdContainer(globalSize, 
                                                                localSize, 
                                                                [| i; j; k |], 
                                                                [| i / normalizedGlobalSize.[0]; j / normalizedGlobalSize.[1]; k / normalizedGlobalSize.[2] |],
                                                                [| 0; 0; 0 |])
                        // Set container as last paramenter
                        finalArguments.[finalArguments.Length - 1] <- container :> obj
                        if multithread then
                            // Create thread
                            let t = new Thread(new ThreadStart(fun () -> work.Invoke(null, finalArguments) |> ignore))
                            t.Start()
                        else
                            work.Invoke(null, finalArguments) |> ignore            
            () :> obj
            *)
        
        // Run a kernel through a quoted kernel call        
        member this.Run(expr: Expr, 
                        globalSize: int array, 
                        localSize: int array, 
                        mode: KernelRunningMode, 
                        fallback: bool) =
            // If global or local size empty theyshould be embedded in kernel expression
            let runtimeInfo, callGraphRoot = this.KernelManager.Process(expr, mode, fallback)    
            match mode with
            | KernelRunningMode.OpenCL ->                
                this.RunOpenCL(true, callGraphRoot, runtimeInfo, globalSize, localSize)
            | KernelRunningMode.Multithread ->
                this.RunMultithread(null, null, globalSize, localSize, true)
            | _ ->              
                this.RunMultithread(null, null, globalSize, localSize, false)
       
    // Global kernel runner
    let mutable internal kernelRunner = new Runner(new Compiler(), None)

    // Function to set custom kernel manager
    let Init(compiler, metric) =
        kernelRunner <- new Runner(compiler, metric)

    // List available devices
    let ListDevices() = 
        List.ofSeq(seq {
                        for platform in Cloo.ComputePlatform.Platforms do
                            yield List.ofSeq(seq {
                                                    for device in platform.Devices do
                                                        yield (device.VendorId, device.Name)
                                             })
                   })
                               
    // Extension methods to run a quoted kernel
    type Expr<'T> with
        member this.Run() =
            kernelRunner.Run(this, [||], [||], KernelRunningMode.OpenCL, true) :?> 'T
        member this.Run(globalSize: int, localSize: int) =
            kernelRunner.Run(this, [| globalSize |], [| localSize |], KernelRunningMode.OpenCL, true) :?> 'T
        member this.Run(globalSize: int array, localSize: int array) =
            kernelRunner.Run(this, globalSize, localSize, KernelRunningMode.OpenCL, true) :?> 'T
            
        member this.RunOpenCL() =
            kernelRunner.Run(this, [||], [||], KernelRunningMode.OpenCL, true) :?> 'T
        member this.RunOpenCL(globalSize: int, localSize: int) =
            kernelRunner.Run(this, [| globalSize |], [| localSize |], KernelRunningMode.OpenCL, false) :?> 'T
        member this.RunOpenCL(globalSize: int array, localSize: int array) =
            kernelRunner.Run(this, globalSize, localSize, KernelRunningMode.OpenCL, false) :?> 'T
            
        member this.RunMultithread(globalSize: int, localSize: int) =
            kernelRunner.Run(this, [| globalSize |], [| localSize |], KernelRunningMode.Multithread, true) :?> 'T
        member this.RunMultithread(globalSize: int array, localSize: int array) =
            kernelRunner.Run(this, globalSize, localSize, KernelRunningMode.Multithread, true) :?> 'T
            
        member this.RunSequential(globalSize: int, localSize: int) =
            kernelRunner.Run(this, [| globalSize |], [| localSize |], KernelRunningMode.Sequential, true) :?> 'T
        member this.RunSequential(globalSize: int array, localSize: int array) =
            kernelRunner.Run(this, globalSize, localSize, KernelRunningMode.Sequential, true) :?> 'T
            

