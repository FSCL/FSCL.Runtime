namespace FSCL.Runtime

open Cloo
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open System.Reflection
open Microsoft.FSharp.Linq.QuotationEvaluation
open FSCL.Compiler
open System.Threading

module KernelRunner =
    // The Kernel runner
    type internal Runner(compiler, metric) =    
        
        member val KernelManager = new KernelManager(compiler, metric) with get
        
        member this.RunOpenCL(kcg:ModuleCallGraph,
                              globalSize:int array, 
                              localSize:int array) =
            let globalDataStorage = this.KernelManager.GlobalDataStorage

            for kernelID in kcg.KernelIDs do
                let kernelInfo = kcg.GetKernel(kernelID)
                // Evaluate the arguments
                let arguments = List.ofSeq(Seq.map (fun (p:KernelParameterInfo) -> p.ArgumentExpression) (kernelInfo.ParameterInfo.Values))

                // Fix: here to be called INSTANTIATE on a metric to get the device to use
                let kernelInstance = kernel.Instance
                let queue = globalDataStorage.Devices.[kernelInstance.DeviceIndex.Value].Queue
                let context = globalDataStorage.Devices.[kernelInstance.DeviceIndex.Value].Context
                // FIX: determine best read/write strategy

                // For each parameter, create buffer (if array), write it and set kernel arg   
                let additionalArgCount = ref 0     
                let paramObjectBufferMap = new System.Collections.Generic.Dictionary<string, (System.Object * ComputeMemory)>()

                let argIndex = ref 0
                Array.iteri (fun pIndex (par:ParameterInfo, dim:int, a:Expr) ->
                    if par.ParameterType.IsArray then
                        let o = arguments.[pIndex]
                        // Check if constant buffer. In this case we pass the dimension (sizeof) the array and not a real buffer
                        if kernel.Info.ParameterInfo.[par.Name].AddressSpace = KernelParameterAddressSpace.LocalSpace then
                            let size = (o.GetType().GetProperty("LongLength").GetValue(o) :?> int64) * 
                                        (int64 (System.Runtime.InteropServices.Marshal.SizeOf(o.GetType().GetElementType())))
                            // Set kernel arg
                            kernelInstance.Kernel.SetLocalArgument(!argIndex, size) 
                        else
                            // Check if read or read_write modeS
                            let matchingParameter = kernel.Info.ParameterInfo.[par.Name]
                            let access = matchingParameter.Access
                            let mustInitBuffer =
                                ((matchingParameter.AddressSpace = KernelParameterAddressSpace.GlobalSpace) ||
                                    (matchingParameter.AddressSpace = KernelParameterAddressSpace.ConstantSpace)) &&
                                ((access = KernelParameterAccessMode.ReadOnly) || 
                                    (access = KernelParameterAccessMode.ReadWrite))

                            // Create buffer and eventually init it
                            let t = par.ParameterType.GetElementType()
                            let buffer = BufferTools.WriteBuffer(t, context, queue, o, dim, mustInitBuffer)                            
                 
                            // Store association between parameter, array and buffer object
                            paramObjectBufferMap.Add(par.Name, (o, buffer.Value))

                            // Set kernel arg
                            kernelInstance.Kernel.SetMemoryArgument(!argIndex, buffer.Value)  

                        // Set additional args for array params (dimensions) 
                        for dimension = 0 to dim - 1 do
                            let sizeOfDim = o.GetType().GetMethod("GetLength").Invoke(o, [| dimension |]) :?> int
                            kernelInstance.Kernel.SetValueArgument<int>(argumentsInfo.Length + !additionalArgCount + dimension, sizeOfDim)
                        additionalArgCount := !additionalArgCount + dim
                    else
                        kernelInstance.Kernel.SetValueArgumentAsObject(!argIndex, arguments.[pIndex])
            
                    argIndex := !argIndex + 1) (argumentsInfo)

                // Run kernel
                let offset = Array.zeroCreate<int64>(globalSize.Length)
                // 32 bit enought for size_t. Kernel uses size_t like int withour cast. We cannot put case into F# kernels each time the user does operations with get_global_id and similar!
                queue.Execute(kernelInstance.Kernel, offset, Array.map(fun el -> int64(el)) globalSize, Array.map(fun el -> int64(el)) localSize, null)

                // Read result if needed
                Array.iteri (fun index (par:ParameterInfo, dim:int, arg:Expr) ->
                    if par.ParameterType.IsArray then
                        if kernel.Info.ParameterInfo.[par.Name].AddressSpace <> KernelParameterAddressSpace.LocalSpace then
                            // Get association between parameter, array and buffer object
                            let (o, buffer) = paramObjectBufferMap.[par.Name]

                            // Check if write or read_write mode
                            let mutable mustReadBuffer = false
                            let matchingParameter = kernel.Info.ParameterInfo.[par.Name]
                            let access = matchingParameter.Access
                            mustReadBuffer <-                     
                                ((matchingParameter.AddressSpace = KernelParameterAddressSpace.GlobalSpace)) &&
                                ((access = KernelParameterAccessMode.WriteOnly) || 
                                    (access = KernelParameterAccessMode.ReadWrite))

                            if(mustReadBuffer) then
                                // Create buffer and eventually init it
                                let t = par.ParameterType.GetElementType()                          
                                BufferTools.ReadBuffer(t, context, queue, o, dim, buffer)) argumentsInfo 

        member this.RunMultithread(kernel: FSCLKernelData, argumentsInfo: (ParameterInfo * int * Expr)[], globalSize: int array, localSize: int array, multithread: bool) =
            let globalDataStorage = this.KernelManager.GlobalDataStorage

            let arguments = Array.map (fun (p, d, e:Expr) -> e.EvalUntyped()) argumentsInfo 
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

            // Launch threads or execute sequential
            let work = kernel.Info.Source
            for i = 0 to normalizedGlobalSize.[0] - 1 do
                for j = 0 to normalizedGlobalSize.[1] - 1 do
                    for k = 0 to normalizedGlobalSize.[2] - 1 do
                        // Create a ids container for each thread and run the thread
                        let container = new WorkItemIdContainer(globalSize, 
                                                                localSize, 
                                                                [| i; j; k |], 
                                                                [| i / normalizedGlobalSize.[0]; j / normalizedGlobalSize.[1]; k / normalizedGlobalSize.[2] |],
                                                                [| 0; 0; 0 |])
                        if multithread then
                            // Create thread
                            let t = new Thread(new ThreadStart(fun () -> work.Invoke(null, Array.append arguments [| container |]) |> ignore))
                            t.Start()
                        else
                            work.Invoke(null, Array.append arguments [| container |]) |> ignore
        
        // Run a kernel through a quoted kernel call        
        member this.Run(expr: Expr, 
                        globalSize: int array, 
                        localSize: int array, 
                        mode: KernelRunningMode, 
                        fallback: bool) =                     
            // Compile the expression (TODO: pass the global cache to prevent compilation of already-compiler kernels)
            let kernelModule = this.KernelManager.Compiler.Compile(expr) :?> KernelModule
            // Ask kernel manager to retrieve the proper executable kernel instance or to create it
            let kernelInstance = this.KernelManager.Store(kernelModule, mode, fallback)
            match mode with
            | KernelRunningMode.OpenCL ->
                this.RunOpenCL(kernelInstance, 0, globalSize, localSize)
            | KernelRunningMode.Multithread ->
                this.RunMultithread(kernelInstance, globalSize, localSize, true)
            | _ ->
                this.RunMultithread(kernelInstance, globalSize, localSize, false)
          
    
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
            
            

