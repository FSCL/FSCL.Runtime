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

[<assembly:DefaultComponentAssembly>]
do()

[<StepProcessor("FSCL_RUNTIME_EXECUTION_DEFAULT_PROCESSOR", "FSCL_RUNTIME_EXECUTION_STEP")>]
type DefaultKerernelExecutionProcessor() =      
    inherit CompilerStepProcessor<FlowGraphNode * bool, ExecutionOutput option>()
    
    override this.Run(input, s, opts) =
        let step = s :?> KernelExecutionStep
        let pool = step.BufferPoolManager

        let fnode, isRoot = input

        match fnode with
        | :? KernelFlowGraphNode ->
            let node = fnode :?> KernelFlowGraphNode
            let sharePriority = 
                if opts.ContainsKey(RuntimeOptions.BufferSharePriority) then
                    opts.[RuntimeOptions.BufferSharePriority] :?> BufferSharePriority
                else
                    BufferSharePriority.PriorityToFlags

            // If globalSize or localSize are 0-length they should be retrieved in the kernel expression, i.e. in the graph node custom info
            let workSize = node.KernelData.Kernel.Meta.KernelMeta.Get<WorkSizeAttribute>()
            if workSize.GlobalSize.Length = 1 && workSize.GlobalSize.[0] = 0L then
                raise (new KernelSetupException("No runtime global size value has been specified and no global and local size information can be found inside the kernel expression"))            
                
            // Input (coming from preceding kernels) buffers
            let inputFromOtherKernels = new Dictionary<String, ExecutionOutput>()
            let mutable outputFromThisKernel = ReturnedValue(())
            let buffers = new Dictionary<String, OpenCLBuffer>()
            let arguments = new Dictionary<string, obj>()
        
            // Get node input binding
            let nodeInput = FlowGraphUtil.GetNodeInput(node)
            // Check which parameters are bound to the output of a kernel
            for par in node.KernelData.Kernel.Parameters do
                if nodeInput.ContainsKey(par.Name) then
                    match nodeInput.[par.Name] with
                    | KernelOutput(otherKernel, otherParIndex) ->
                        let kernelOutput = step.Process(otherKernel, false)
                        inputFromOtherKernels.Add(par.Name, kernelOutput)    
                    | _ ->
                        ()
                    
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
                    match nodeInput.[par.Name] with
                    | BufferAllocationSize(sizeFunction) ->
                        // Buffer allocated in the body of the kernel are traslated into additional arguments
                        // To setup these arguments in OpenCL, the buffers must be pre-allocated
                        let elementCount = sizeFunction(arguments, workSize.GlobalSize, workSize.LocalSize)
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
                        let o = LeafExpressionConverter.EvaluateQuotation(expr)
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

            // Cool, we processed the input and now all the arguments have been set
            // Run kernel
            let offset = Array.zeroCreate<int64>(workSize.GlobalSize.Length)
            // 32 bit enought for size_t. Kernel uses size_t like int without cast. 
            // We cannot put case into F# kernels each time the user does operations with get_global_id and similar!
            node.DeviceData.Queue.Execute(node.CompiledKernelData.Kernel, offset, workSize.GlobalSize, workSize.LocalSize, null)
            node.DeviceData.Queue.Finish()

            // Dispose buffers
            for b in buffers do
                pool.EndUsingBuffer(b.Value)

            // Return the objects that the F# kernels eventually returns as a tuple (if more than 1)
            Some(outputFromThisKernel)

        | _ ->
            None
                  