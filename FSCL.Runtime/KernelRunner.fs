namespace FSCL.Runtime

open Cloo
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open System.Reflection
open Microsoft.FSharp.Linq.QuotationEvaluation
open FSCL.Compiler
        
module KernelRunner =
    // Kernel run mode
    type internal KernelRunningMode =
    | OpenCL
    | Multithread
    | Sequential

    // The Kernel runner
    type internal Runner(compiler, metric) =    
        
        member val KernelManager = new KernelManager(compiler, metric) with get
        
        member this.Run(kernelInfo: MethodInfo, arguments: obj[], argumentsInfo: (ParameterInfo * int * Expr)[], globalSize: int array, localSize: int array, mode: KernelRunningMode, fallback: bool) =
            let globalDataStorage = this.KernelManager.GlobalDataStorage

            // Found a kernel in global data matching the call
            let matchingKernel = ref (this.KernelManager.FindMatchingKernelInfo(kernelInfo, Array.map (fun a -> a.GetType()) arguments))
            if (!matchingKernel).IsNone then
                // Try add it to the compiler
                this.KernelManager.Add(kernelInfo) |> ignore
                matchingKernel := this.KernelManager.FindMatchingKernelInfo(kernelInfo, Array.map (fun a -> a.GetType()) arguments)
            
            // Fix: here to be called INSTANTIATE on a metric to get the device to use
            let kernelInstance = (!matchingKernel).Value.Instances.[0]
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
                    if (!matchingKernel).Value.Info.ParameterInfo.[par.Name].AddressSpace = KernelParameterAddressSpace.LocalSpace then
                        let size = (o.GetType().GetProperty("LongLength").GetValue(o) :?> int64) * 
                                   (int64 (System.Runtime.InteropServices.Marshal.SizeOf(o.GetType().GetElementType())))
                        // Set kernel arg
                        kernelInstance.Kernel.SetLocalArgument(!argIndex, size) 
                    else
                        // Check if read or read_write modeS
                        let matchingParameter = (!matchingKernel).Value.Info.ParameterInfo.[par.Name]
                        let access = matchingParameter.Access
                        let mustInitBuffer =
                            ((matchingParameter.AddressSpace = KernelParameterAddressSpace.GlobalSpace) ||
                             (matchingParameter.AddressSpace = KernelParameterAddressSpace.ConstantSpace)) &&
                            ((access = KernelParameterAccessMode.ReadOnly) || 
                             (access = KernelParameterAccessMode.ReadWrite))

                        // Create buffer and eventually init it
                        let t = par.ParameterType.GetElementType()
                        let mutable buffer = None
                        if (t = typeof<uint32>) then
                            buffer <- Some(BufferTools.WriteBuffer<uint32>(context, queue, o, dim, mustInitBuffer))
                        elif (t = typeof<uint64>) then
                            buffer <- Some(BufferTools.WriteBuffer<uint64>(context, queue, o, dim ,mustInitBuffer))
                        elif (t = typeof<int64>) then
                            buffer <- Some(BufferTools.WriteBuffer<int64>(context, queue, o, dim, mustInitBuffer))
                        elif (t = typeof<int>) then
                            buffer <- Some(BufferTools.WriteBuffer<int>(context, queue, o, dim, mustInitBuffer))
                        elif (t = typeof<double>) then
                            buffer <- Some(BufferTools.WriteBuffer<double>(context, queue, o, dim, mustInitBuffer))
                        elif (t = typeof<float32>) then
                            buffer <- Some(BufferTools.WriteBuffer<float32>(context, queue, o, dim, mustInitBuffer))
                        elif (t = typeof<bool>) then
                            buffer <- Some(BufferTools.WriteBuffer<int>(context, queue, o, dim, mustInitBuffer))
                 
                        // Stor association between parameter, array and buffer object
                        paramObjectBufferMap.Add(par.Name, (o, buffer.Value))

                        // Set kernel arg
                        kernelInstance.Kernel.SetMemoryArgument(!argIndex, buffer.Value)  

                    // Set additional args for array params (dimensions) 
                    for dimension = 0 to dim - 1 do
                        let sizeOfDim = o.GetType().GetMethod("GetLength").Invoke(o, [| dimension |]) :?> int
                        kernelInstance.Kernel.SetValueArgument<int>(argumentsInfo.Length + !additionalArgCount + dimension, sizeOfDim)
                    additionalArgCount := !additionalArgCount + dim
                else
                    let t = par.ParameterType
                    if (t = typeof<uint32>) then
                        kernelInstance.Kernel.SetValueArgument<uint32>(!argIndex, arguments.[pIndex] :?> uint32)
                    elif (t = typeof<uint64>) then
                        kernelInstance.Kernel.SetValueArgument<uint64>(!argIndex, arguments.[pIndex] :?> uint64)
                    elif (t = typeof<int64>) then
                        kernelInstance.Kernel.SetValueArgument<int64>(!argIndex, arguments.[pIndex] :?> int64)
                    elif (t = typeof<int>) then
                        kernelInstance.Kernel.SetValueArgument<int>(!argIndex, arguments.[pIndex] :?> int)
                    elif (t = typeof<double>) then
                        kernelInstance.Kernel.SetValueArgument<double>(!argIndex, arguments.[pIndex] :?> double)
                    elif (t = typeof<float32>) then
                        kernelInstance.Kernel.SetValueArgument<float32>(!argIndex, arguments.[pIndex] :?> float32)
                    elif (t = typeof<bool>) then
                        kernelInstance.Kernel.SetValueArgument<bool>(!argIndex, arguments.[pIndex] :?> bool)
            
                argIndex := !argIndex + 1) (argumentsInfo)

            // Run kernel
            let offset = Array.zeroCreate<int64>(globalSize.Length)
            // 32 bit enought for size_t. Kernel uses size_t like int withour cast. We cannot put case into F# kernels each time the user does operations with get_global_id and similar!
            queue.Execute(kernelInstance.Kernel, offset, Array.map(fun el -> int64(el)) globalSize, Array.map(fun el -> int64(el)) localSize, null)

            // Read result if needed
            Array.iteri (fun index (par:ParameterInfo, dim:int, arg:Expr) ->
                if par.ParameterType.IsArray then
                    if (!matchingKernel).Value.Info.ParameterInfo.[par.Name].AddressSpace <> KernelParameterAddressSpace.LocalSpace then
                        // Get association between parameter, array and buffer object
                        let (o, buffer) = paramObjectBufferMap.[par.Name]

                        // Check if write or read_write mode
                        let mutable mustReadBuffer = false
                        let matchingParameter = (!matchingKernel).Value.Info.ParameterInfo.[par.Name]
                        let access = matchingParameter.Access
                        mustReadBuffer <-                     
                            ((matchingParameter.AddressSpace = KernelParameterAddressSpace.GlobalSpace)) &&
                            ((access = KernelParameterAccessMode.WriteOnly) || 
                             (access = KernelParameterAccessMode.ReadWrite))

                        if(mustReadBuffer) then
                            // Create buffer and eventually init it
                            let t = par.ParameterType.GetElementType()
                            if (t = typeof<uint32>) then
                                BufferTools.ReadBuffer<uint32>(context, queue, o, dim, buffer :?> ComputeBuffer<uint32>) 
                            elif (t = typeof<uint64>) then
                                BufferTools.ReadBuffer<uint64>(context, queue, o, dim, buffer :?> ComputeBuffer<uint64>) 
                            elif (t = typeof<int64>) then
                                BufferTools.ReadBuffer<int64>(context, queue, o, dim, buffer :?> ComputeBuffer<int64>) 
                            elif (t = typeof<int>) then
                                BufferTools.ReadBuffer<int>(context, queue, o, dim, buffer :?> ComputeBuffer<int>) 
                            elif (t = typeof<double>) then
                                BufferTools.ReadBuffer<double>(context, queue, o, dim, buffer :?> ComputeBuffer<double>) 
                            elif (t = typeof<float32>) then
                                BufferTools.ReadBuffer<float32>(context, queue, o, dim, buffer :?> ComputeBuffer<float32>) 
                            elif (t = typeof<bool>) then
                                BufferTools.ReadBuffer<bool>(context, queue, o, dim, buffer :?> ComputeBuffer<bool>)) argumentsInfo 


        // Run a kernel through a quoted kernel call        
        member this.Run(expr: Expr, globalSize: int array, localSize: int array, mode: KernelRunningMode, fallback: bool) =                     
            let (c, kernelInfo, args) = KernelManagerTools.ExtractMethodInfo(expr)
            let arguments = Array.map (fun (p, d, e:Expr) -> e.EvalUntyped()) args
            this.Run(kernelInfo, arguments, args, globalSize, localSize, mode, fallback)
    
    // Global kernel runner
    let internal kernelRunner = new Runner(new Compiler(), None)

    // Function to set custom kernel manager
    let Init(compiler, metric) =
        kernelRunner = new Runner(compiler, metric)

    // List available device 
    let ListDevices() = 
        List.ofSeq(seq {
                        for platform in Cloo.ComputePlatform.Platforms do
                            yield List.ofSeq(seq {
                                                    for device in platform.Devices do
                                                        yield (device.VendorId, device.Name)
                                             })
                   })

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
            

