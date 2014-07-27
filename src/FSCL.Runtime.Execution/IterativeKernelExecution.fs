namespace FSCL.Runtime.RuntimeSteps

open System
open FSCL.Compiler
open FSCL.Compiler.Util
open FSCL.Language
open FSCL.Runtime
open FSCL.Runtime.Managers
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Linq.RuntimeHelpers
open OpenCL
open Microsoft.FSharp.Quotations
open System.Threading

[<assembly:DefaultComponentAssembly>]
do()

[<StepProcessor("FSCL_RUNTIME_EXECUTION_ITERATIVE_PROCESSOR", "FSCL_RUNTIME_EXECUTION_STEP")>]
type IterativeKernelExecutionProcessor() =      
    inherit CompilerStepProcessor<FlowGraphNode * bool, ExecutionOutput option>()
    
    override this.Run(input, s, opts) =
        let step = s :?> KernelExecutionStep
        let pool = step.BufferPoolManager

        let fnode, isRoot = input
        
        let iterativeSetup = 
            if opts.ContainsKey("IterativeSetup") then
                Some(opts.["IterativeSetup"] :?> int -> obj list option)
            else
                None
        if iterativeSetup.IsSome then
            match fnode with
            | :? OpenCLKernelFlowGraphNode ->
                let node = fnode :?> OpenCLKernelFlowGraphNode
                let sharePriority = 
                    if opts.ContainsKey(RuntimeOptions.BufferSharePriority) then
                        opts.[RuntimeOptions.BufferSharePriority] :?> BufferSharePriority
                    else
                        BufferSharePriority.PriorityToFlags
                        
                // Input (coming from preceding kernels) buffers
                let inputFromOtherKernels = new Dictionary<String, ExecutionOutput>()
                let mutable outputFromThisKernel = ReturnedValue(())
                let buffers = new Dictionary<String, OpenCLBuffer>()
                let arguments = new Dictionary<string, obj>()
        
                // Get node input binding
                let nodeInp = FlowGraphUtil.GetNodeInput(node)
                // Check which parameters are bound to the output of a kernel
                for par in node.KernelData.Kernel.Parameters do
                    if nodeInp.ContainsKey(par.Name) then
                        match nodeInp.[par.Name] with
                        | KernelOutput(otherKernel, otherParIndex) ->
                            raise (new KernelExecutionException("Cannot iterate the execution of a multi-kernel expression"))
                        | _ ->
                            ()
                 
                // Start iterating   
                let mutable iteration = 0
                let mutable iterationData = iterativeSetup.Value(iteration)
                while iterationData.IsSome do
                    let actualArgs = iterationData.Value

                    // Now that we have executed all the preceding kernels, complete the evaluation of the arguments
                    // We do this in a second time to avoid allocating many buffers ina recursive function, risking stack overflow
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
                            match nodeInp.[par.Name] with
                            | BufferAllocationSize(sizeFunction) ->
                                raise (new KernelExecutionException("Iterative kernels cannot return an type different from unit (void)"))
                            | ActualArgument(expr) ->
                                // Input from an actual argument
                                let o = actualArgs.[argIndex]
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
                                        raise (new KernelExecutionException("Iterative kernels cannot return an type different from unit (void)"))
                                        
                            | KernelOutput(othNode, a) ->
                                raise (new KernelExecutionException("Cannot iterate the execution of a multi-kernel expression"))
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
                                match nodeInp.[par.Name] with
                                | ActualArgument(e) ->
                                    node.CompiledKernelData.Kernel.SetValueArgumentAsObject(argIndex, actualArgs.[argIndex])
                                // | CompilerPrecomputedValue(computeFunction) ->
                                    //   let o = computeFunction(arguments, workSize.GlobalSize, workSize.LocalSize)
                                    // node.CompiledKernelData.Kernel.SetValueArgumentAsObject(argIndex, o)
                                | KernelOutput(_, _) ->
                                    raise (new KernelExecutionException("Cannot iterate the execution of a multi-kernel expression"))
                                | _ ->
                                    raise (new KernelSetupException("The parameter " + par.Name + " is considered as implicit, which means that the runtime should be able to provide its value automatically, but this is not valid except for size parameters"))
                        
                        // Process next parameter
                        argIndex <- argIndex + 1

                    // Cool, we processed the input and now all the arguments have been set
                    // Run kernel
                    // 32 bit enought for size_t. Kernel uses size_t like int without cast. 
                    // We cannot put case into F# kernels each time the user does operations with get_global_id and similar!
                    let workSize = actualArgs.[actualArgs.Length - 1] :?> WorkItemInfo
                    let globalSize = Array.init (workSize.WorkDim()) (fun i -> workSize.GlobalSize(i) |> int64)
                    let localSize = Array.init (workSize.WorkDim()) (fun i -> workSize.LocalSize(i) |> int64)
                    let globalOffset = Array.init (workSize.WorkDim()) (fun i -> workSize.GlobalOffset(i) |> int64)

                    node.DeviceData.Queue.Execute(node.CompiledKernelData.Kernel, globalOffset |> Array.map int64, globalSize |> Array.map int64, localSize |> Array.map int64, null)
                    node.DeviceData.Queue.Finish()

                    // Dispose buffers
                    for b in buffers do
                        pool.EndUsingBuffer(b.Value)

                    // Return the objects that the F# kernels eventually returns as a tuple (if more than 1)
                    iteration <- iteration + 1
                    iterationData <- iterativeSetup.Value(iteration)
                            
            // Multithread execution
            | :? MultithreadKernelFlowGraphNode ->
                let node = fnode :?> MultithreadKernelFlowGraphNode
                
                let mutable iteration = 0
                let mutable iterationData = iterativeSetup.Value(iteration)
                let globalSize = Array.zeroCreate<int64> 3
                let globalOffset = Array.zeroCreate<int64> 3

                while iterationData.IsSome do
                    let actualArgs = iterationData.Value
                    let workSizeIndex = actualArgs|> List.findIndex(fun a -> a :? WorkItemInfo)
                    let workSize = actualArgs.[workSizeIndex] :?> WorkSize
                            
                    // Setup workitem info
                    let globalSize, _, globalOffset = KernelSetupUtil.NormalizeWorkSize(workSize.GlobalSize(), workSize.LocalSize(), workSize.GlobalOffset())

                    // Create barrier
                    let mutable totalGlobalSize = 1L
                    for i in 0.. globalSize.Length - 1 do
                        totalGlobalSize <- totalGlobalSize * globalSize.[i]
                    let barrier = ref null
                    let recreateBarrier = ref true
                    let lockObj = new Object()

                    // Spawn threads
                    let methodToExecute = node.KernelData.ParsedSignature      
                    let threads = new List<Thread>()          
                    for i = globalOffset.[2] |> int to globalSize.[2] - 1L |> int do
                        for j = globalOffset.[1] |> int to globalSize.[1] - 1L |> int do
                            for k = globalOffset.[0] |> int to globalSize.[0] - 1L |> int do
                                let globalId = [| k |> int64; j |> int64; i |> int64 |]
                                let workItemInfo = new MultithreadWorkItemInfo(globalId, globalSize, globalOffset, lockObj, recreateBarrier, barrier)
                                let data = actualArgs |> List.mapi (fun i a -> 
                                                                            if i = workSizeIndex then 
                                                                                box workItemInfo 
                                                                            else
                                                                                a) |> List.toArray
                                let t = new Thread(fun () -> methodToExecute.Invoke(null, data) |> ignore)
                                threads.Add(t)
                                t.Start()

                    // Wait to complete
                    for t in threads do
                        t.Join()
                                                        
                    iteration <- iteration + 1
                    iterationData <- iterativeSetup.Value(iteration)
                              
            // Regular function
            | _ -> 
                let node = fnode :?> RegularFunctionFlowGraphNode
                    
                let mutable iteration = 0
                let mutable iterationData = iterativeSetup.Value(iteration)
                while iterationData.IsSome do
                    let actualArgs = iterationData.Value
                    // Evaluate arguments
                    let inst =
                        if node.Object.IsSome then
                            LeafExpressionConverter.EvaluateQuotation(node.Object.Value)
                        else
                            null

                    // Run
                    node.MethodInfo.Invoke(inst, actualArgs |> List.toArray) |> ignore

                    iteration <- iteration + 1
                    iterationData <- iterativeSetup.Value(iteration)                                          

            Some(ExecutionOutput.ReturnedValue(()))                    
        else
            None
                  