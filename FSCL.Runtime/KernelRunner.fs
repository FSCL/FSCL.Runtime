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

type ConnectionTable = ReadOnlyDictionary<MethodInfo, ReadOnlyDictionary<KernelConnection, KernelConnection>>

module KernelRunner =
    // The Kernel runner
    type internal Runner(compiler, metric) =    
        
        member val KernelManager = new KernelManager(compiler, metric) with get
        
        member private this.BuildParameterBasedConnections(inputConnections: ConnectionTable,
                                                           outputConnections: ConnectionTable) =       
            let ic = new Dictionary<String, MethodInfo * String>()
            let oc = new Dictionary<String, List<MethodInfo * String>>()
            for inputSrc in inputConnections do
                for inputConn in inputSrc.Value do
                    match inputConn.Value with
                    | ParameterConnection(par) ->
                        match inputConn.Key with
                        | ParameterConnection(srcPar) ->
                            ic.Add(par, (inputSrc.Key, srcPar))
                        | _ ->
                            ()
                    | _ ->
                        ()
            for outputDst in outputConnections do
                for outputConn in outputDst.Value do
                    match outputConn.Key with
                    | ParameterConnection(par) ->
                        if not(oc.ContainsKey(par)) then
                            oc.Add(par, new List<MethodInfo * String>())
                        match outputConn.Value with
                        | ParameterConnection(dstPar) ->
                            oc.[par].Add((outputDst.Key, dstPar))
                        | _ ->
                            ()
                    | _ ->
                        ()
            (ic, oc)

        member this.RunOpenCL(deviceData: RuntimeDeviceData,
                              kernelData: RuntimeKernelData,
                              compiledData: RuntimeCompiledKernelData,
                              inputConnections: ConnectionTable,
                              outputConnections: ConnectionTable,
                              bufferBinding: Dictionary<MethodInfo, Dictionary<String, ComputeMemory * Object * int array>>,
                              globalSize: int array, 
                              localSize: int array) =
            // Analyze connections to build a parameter-based indexed structure
            let ic, oc = this.BuildParameterBasedConnections(inputConnections, outputConnections)

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
                        // Remember the size parameters for this array
                        let sizeParametersValue = new List<int>()
                        for spIndex = 0 to par.SizeParameters.Count - 1 do
                            let sizeOfDim = o.GetType().GetMethod("GetLength").Invoke(o, [| spIndex |]) :?> int
                            sizeParametersBinding.Add(par.SizeParameters.[spIndex].Name, sizeOfDim)  
                            sizeParametersValue.Add(sizeOfDim)
                        // Create buffer if needed (no local address space)
                        if par.AddressSpace = KernelParameterAddressSpace.LocalSpace then
                            let size = (o.GetType().GetProperty("LongLength").GetValue(o) :?> int64) * 
                                        (int64 (System.Runtime.InteropServices.Marshal.SizeOf(o.GetType().GetElementType())))
                            // Set kernel arg
                            compiledData.Kernel.SetLocalArgument(argIndex, size) 
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
                            bufferBinding.[kernelData.Info.ID].Add(par.Name, (buffer.Value, o, Array.ofSeq(sizeParametersValue)))
                    // We get the input from a previously-executed kernel
                    else
                        // WE SHOULD AVOID COPY!!!
                        // Get buffer to be used as (an) input of the new kernel
                        let inputBuffer, _, sizes = bufferBinding.[fst (ic.[par.Name])].[snd (ic.[par.Name])]
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
                // Scalar parameter
                else
                    // Check if this is an argument automatically inserted to represent the length af an array parameter
                    if par.IsSizeParameter then
                        // Get the length of the proper dimension of the array
                        let value = sizeParametersBinding.[par.Name]
                        compiledData.Kernel.SetValueArgument<int>(argIndex, value)
                    else
                        let o = par.ArgumentExpression.Value.EvalUntyped()        
                        compiledData.Kernel.SetValueArgumentAsObject(argIndex, o)
                // Process next parameter
                argIndex <- argIndex + 1

            // Run kernel
            let offset = Array.zeroCreate<int64>(globalSize.Length)
            // 32 bit enought for size_t. Kernel uses size_t like int without cast. 
            // We cannot put case into F# kernels each time the user does operations with get_global_id and similar!
            deviceData.Queue.Execute(compiledData.Kernel, offset, Array.map(fun el -> int64(el)) globalSize, Array.map(fun el -> int64(el)) localSize, null)
            
            // Read result if needed
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
        
        // Run a kernel through a quoted kernel call        
        member this.Run(expr: Expr, 
                        globalSize: int array, 
                        localSize: int array, 
                        mode: KernelRunningMode, 
                        fallback: bool) =
            let data = this.KernelManager.Process(expr, mode, fallback)                                 
            match mode with
            | KernelRunningMode.OpenCL ->                
                let bufferBinding = new Dictionary<MethodInfo, Dictionary<String, ComputeMemory * Object * int array>>()
                for (deviceData, kernelData, compiledData), inputConn, outputConn in data do
                    this.RunOpenCL(deviceData, kernelData, compiledData, inputConn, outputConn, bufferBinding, globalSize, localSize)
            | KernelRunningMode.Multithread ->
                let bufferBinding = new Dictionary<MethodInfo, Dictionary<String, ComputeMemory * Object * int array>>()
                for (deviceData, kernelData, compiledData), inputConn, outputConn in data do
                    this.RunMultithread(kernelData, compiledData, inputConn, outputConn, bufferBinding, globalSize, localSize, true)
            | _ ->
                let bufferBinding = new Dictionary<MethodInfo, Dictionary<String, ComputeMemory * Object * int array>>()
                for (deviceData, kernelData, compiledData), inputConn, outputConn in data do
                    this.RunMultithread(kernelData, compiledData, inputConn, outputConn, bufferBinding, globalSize, localSize, false)
       
    // Global kernel runner
    let internal kernelRunner = new Runner(new Compiler(), None)

    // Function to set custom kernel manager
    let Init(compiler, metric) =
        kernelRunner = new Runner(compiler, metric)

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
            
            

