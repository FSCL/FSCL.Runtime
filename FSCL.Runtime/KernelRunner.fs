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

module KernelRunner =
    // The Kernel runner
    type internal Runner(compiler, metric) =    

        
        member val KernelManager = new KernelManager(compiler, metric) with get
                        
        member this.RunOpenCL(node: FlowGraphNode,
                              deviceData: RuntimeDeviceData,
                              kernelData: RuntimeKernelData,
                              compiledData: RuntimeCompiledKernelData,
                              bufferBinding: Dictionary<MethodInfo, Dictionary<String, ComputeMemory * Object * int array>>,
                              globalSize: int array, 
                              localSize: int array) =

            let mutable argIndex = 0
            // To remember the array associated to the length parameters
            //let sizeParametersBinding = new Dictionary<string, int>()
            // Get node input binding
            let nodeInput = FlowGraphManager.GetNodeInput(node)
            // To store the value of aready-evaluated arguments
            let evaluatedArgs = new Dictionary<string, obj>()
            
            // Foreach argument of the kernel
            for par in kernelData.Info.Parameters do      
                if par.Type.IsArray then     
                    // If the parameter is an array the argument value can be:
                    // 1) ActualArgument: the argument value is given by the user that invokes the kernel
                    // 2) KernelOutput: the argument value is the output of a kernel already executed
                    // 3) CompilerPrecomputedValue: the argument value is established by the compiler (local arguments)
                    // 4) ReturnExpression:        
                    // Check if this is the input coming from a previously executed kernel or from a runtime (actual) value
                    match nodeInput.[par.Name] with
                    | ActualArgument(expr) ->
                        // Input from an actual argument
                        let o = expr.EvalUntyped()
                        // Create buffer if needed (no local address space)
                        if par.AddressSpace = KernelParameterAddressSpace.LocalSpace then
                            compiledData.Kernel.SetLocalArgument(argIndex, o :?> int64) 
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
                            // Store buffer/object data
                            if not (bufferBinding.ContainsKey(kernelData.Info.ID)) then
                                bufferBinding.Add(kernelData.Info.ID, new Dictionary<string, ComputeMemory * Object * int array>())
                            bufferBinding.[kernelData.Info.ID].Add(par.Name, (buffer.Value, o, Array.ofSeq([])))
                    | KernelOutput(node, point) ->
                        match point with
                        | ReturnValue(i) ->
                            raise (new KernelSetupException("The parameter " + par.Name + " is bound to the return value of a kernel, but return values should be lifted during compilation and replaced with additional parameters"))
                        | OutArgument(a) ->
                            // WE SHOULD AVOID COPY!!!
                            // Get buffer to be used as (an) input of the new kernel
                            let inputBuffer, _, sizes = bufferBinding.[node.KernelID].[a]
                            let buffer = BufferTools.CopyBuffer(par.Type.GetElementType(), deviceData.Context, deviceData.Queue, inputBuffer)                        
                            // Remember the size parameters for this array
                            let sizeParametersValue = new List<int>()
                            for spIndex = 0 to par.SizeParameters.Count - 1 do
                                let sizeOfDim = sizes.[spIndex]
                                sizeParametersBinding.Add(par.SizeParameters.[spIndex].Name, sizeOfDim)  
                                sizeParametersValue.Add(sizeOfDim)  
                            // Set kernel arg
                            compiledData.Kernel.SetMemoryArgument(argIndex, buffer.Value)                      
                            // Check if this can be written to be the input of a successive kernel
                            if not (bufferBinding.ContainsKey(kernelData.Info.ID)) then
                                bufferBinding.Add(kernelData.Info.ID, new Dictionary<string, ComputeMemory * Object * int array>())
                            bufferBinding.[kernelData.Info.ID].Add(par.Name, (buffer.Value, null, Array.ofSeq(sizeParametersValue)))
                    | _ ->
                        raise (new KernelSetupException("The parameter " + par.Name + " is considered as implicit, which means that the runtime should be able to provide its value automatically, but this can't be done for array parameters"))
                // Scalar parameter
                else
                    // Check if this is an argument automatically inserted to represent the length af an array parameter
                    if par.IsSizeParameter then
                        // Get the length of the proper dimension of the array
                        let value = sizeParametersBinding.[par.Name]
                        compiledData.Kernel.SetValueArgument<int>(argIndex, value)
                    else
                        if not (callGraphNode.Arguments.ContainsKey(par.Name)) then
                            raise (new KernelSetupException("The parameter " + par.Name + " has not been set for the kernel " + kernelData.Info.Name + " in the call graph"))
                        // Check if this is the input coming from a previously executed kernel or from a runtime (actual) value
                        match callGraphNode.Arguments.[par.Name] with
                        | RuntimeValue(e) ->
                            let o = e.EvalUntyped()        
                            compiledData.Kernel.SetValueArgumentAsObject(argIndex, o)
                        | KernelOutput(_, _) ->
                            raise (new KernelSetupException("The scalar parameter " + par.Name + " is bound to the output of a kernel, but only vector parameters (arrays) can be bound to the output of a kernel"))
                        | _ ->
                            raise (new KernelSetupException("The parameter " + par.Name + " is considered as implicit, which means that the runtime should be able to provide its value automatically, but this is not valid except for size parameters"))
                            
                // Process next parameter
                argIndex <- argIndex + 1

            // Run kernel
            let offset = Array.zeroCreate<int64>(globalSize.Length)
            // 32 bit enought for size_t. Kernel uses size_t like int without cast. 
            // We cannot put case into F# kernels each time the user does operations with get_global_id and similar!
            deviceData.Queue.Execute(compiledData.Kernel, offset, Array.map(fun el -> int64(el)) globalSize, Array.map(fun el -> int64(el)) localSize, null)
            
            // Read result if needed
            let returnedObjects = new List<Object>()
            // Foreach argument of the kernel
            for par in kernelData.Info.Parameters do      
                if par.Type.IsArray then             
                    // Check if must read (no connection with any successive kernel)
                    if not (oc.ContainsKey(par.Name)) then
                        let buffer, obj, _ = bufferBinding.[kernelData.Info.ID].[par.Name]
                        // Check if it is actually possible and meaningful to read the buffer
                        // I.e. No local or constant or read-only buffer 
                        // Get association between parameter, array and buffer object
                        // Check if write or read_write mode
                        let mutable mustReadBuffer =            
                            obj <> null &&
                            ((par.AddressSpace = KernelParameterAddressSpace.GlobalSpace)) &&
                            ((par.Access = KernelParameterAccessMode.WriteOnly) || 
                                (par.Access = KernelParameterAccessMode.ReadWrite))

                        if(mustReadBuffer) then                 
                            BufferTools.ReadBuffer(par.Type.GetElementType(), deviceData.Context, deviceData.Queue, obj, par.SizeParameters.Count, buffer)
                            // Store the obj if it is returned by the F# kernel (exploiting return values in kernels)
                            if par.IsReturnParameter then
                                returnedObjects.Add(obj)

            // Return the objects that the F# kernels eventually returns as a tuple (if more than 1)
            if returnedObjects.Count = 0 then
                () :> obj
            else if returnedObjects.Count = 1 then
                returnedObjects.[0]
            else
                FSharpValue.MakeTuple(returnedObjects.ToArray(), FSharpType.MakeTupleType(Array.ofSeq(Seq.map(fun (o:obj) -> o.GetType()) returnedObjects)))
                            
        member this.RunMultithread(kernelData: RuntimeKernelData,
                                   compiledData: RuntimeCompiledKernelData,
                                   inputConnections: ConnectionTable,
                                   outputConnections: ConnectionTable,
                                   bufferBinding: Dictionary<MethodInfo, Dictionary<String, ComputeMemory * Object * int array>>,
                                   globalSize: int array, 
                                   localSize: int array, 
                                   multithread: bool) =                                               
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
        
        // Run a kernel through a quoted kernel call        
        member this.Run(expr: Expr, 
                        globalSize: int array, 
                        localSize: int array, 
                        mode: KernelRunningMode, 
                        fallback: bool) =
            let runtimeInfo, flatGraph = this.KernelManager.Process(expr, mode, fallback)                                 
            let mutable returnObject = new Object() 
            match mode with
            | KernelRunningMode.OpenCL ->                
                // Create storage for buffers to be used multiple times in the flow
                let bufferBinding = new Dictionary<MethodInfo, Dictionary<String, ComputeMemory * Object * int array>>()
                // Execute kernels for the last one to the first of the flat graph (dependency-based ordered)
                for nodeIndex = flatGraph.Length - 1 downto 0 do
                    let deviceData, kernelData, compiledData = runtimeInfo.[flatGraph.[nodeIndex].KernelID]
                    returnObject <- this.RunOpenCL(flatGraph.[nodeIndex], deviceData, kernelData, compiledData, bufferBinding, globalSize, localSize)
            | KernelRunningMode.Multithread ->
                let bufferBinding = new Dictionary<MethodInfo, Dictionary<String, ComputeMemory * Object * int array>>()
                for (deviceData, kernelData, compiledData), inputConn, outputConn in data do
                    returnObject <- this.RunMultithread(kernelData, compiledData, inputConn, outputConn, bufferBinding, globalSize, localSize, true)
            | _ ->
                let bufferBinding = new Dictionary<MethodInfo, Dictionary<String, ComputeMemory * Object * int array>>()
                for (deviceData, kernelData, compiledData), inputConn, outputConn in data do
                    returnObject <- this.RunMultithread(kernelData, compiledData, inputConn, outputConn, bufferBinding, globalSize, localSize, false)
            returnObject
       
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
    type Expr with
        member this.Run(globalSize: int, localSize: int) =
            kernelRunner.Run(this, [| globalSize |], [| localSize |], KernelRunningMode.OpenCL, true)
        member this.Run(globalSize: int array, localSize: int array) =
            kernelRunner.Run(this, globalSize, localSize, KernelRunningMode.OpenCL, true)
            
        member this.RunOpenCL(globalSize: int, localSize: int) =
            kernelRunner.Run(this, [| globalSize |], [| localSize |], KernelRunningMode.OpenCL, false)
        member this.RunOpenCL(globalSize: int array, localSize: int array) =
            kernelRunner.Run(this, globalSize, localSize, KernelRunningMode.OpenCL, false)
            
        member this.RunMultithread(globalSize: int, localSize: int) =
            kernelRunner.Run(this, [| globalSize |], [| localSize |], KernelRunningMode.Multithread, true)
        member this.RunMultithread(globalSize: int array, localSize: int array) =
            kernelRunner.Run(this, globalSize, localSize, KernelRunningMode.Multithread, true)
            
        member this.RunSequential(globalSize: int, localSize: int) =
            kernelRunner.Run(this, [| globalSize |], [| localSize |], KernelRunningMode.Sequential, true)
        member this.RunSequential(globalSize: int array, localSize: int array) =
            kernelRunner.Run(this, globalSize, localSize, KernelRunningMode.Sequential, true)
            
    // Extension methods to run a quoted kernel
    type Expr<'T> with
        member this.Run(globalSize: int, localSize: int) =
            kernelRunner.Run(this, [| globalSize |], [| localSize |], KernelRunningMode.OpenCL, true) :?> 'T
        member this.Run(globalSize: int array, localSize: int array) =
            kernelRunner.Run(this, globalSize, localSize, KernelRunningMode.OpenCL, true) :?> 'T
            
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
            
            

