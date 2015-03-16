namespace FSCL.Runtime.RuntimeSteps

open OpenCL
open FSCL
open FSCL.Compiler
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
        
    let rec LiftArgumentsAndKernelCalls(e: Expr,
                                        args: Dictionary<string, obj>,
                                        wi: WorkItemInfo option) =
        match e with
        // Return allocation expression can contain a call to global_size, local_size, num_groups or work_dim
        | Patterns.Call(o, m, arguments) ->
            if m.DeclaringType.IsAssignableFrom(typeof<WorkItemInfo>) && (m.Name = "GlobalSize") then
                if wi.IsSome then
                    Expr.Value(wi.Value.GlobalSize(LeafExpressionConverter.EvaluateQuotation(arguments.[0]) :?> int))
                else
                    raise (new KernelCompilationException("Cannot evaluate buffer allocation size. The size depends on the work-item space but this space is not specified"))
            else if m.DeclaringType.IsAssignableFrom(typeof<WorkItemInfo>) && (m.Name = "GlobalOffset") then
                if wi.IsSome then
                    Expr.Value(wi.Value.GlobalOffset(LeafExpressionConverter.EvaluateQuotation(arguments.[0]) :?> int))
                else
                    raise (new KernelCompilationException("Cannot evaluate buffer allocation size. The size depends on the work-item space but this space is not specified"))
            else if m.DeclaringType.IsAssignableFrom(typeof<WorkItemInfo>) && (m.Name = "LocalSize") then
                if wi.IsSome then
                    Expr.Value(wi.Value.LocalSize(LeafExpressionConverter.EvaluateQuotation(arguments.[0]) :?> int))
                else
                    raise (new KernelCompilationException("Cannot evaluate buffer allocation size. The size depends on the work-item space but this space is not specified"))
            else if m.DeclaringType.IsAssignableFrom(typeof<WorkItemInfo>) && (m.Name = "NumGroups") then
                if wi.IsSome then
                    Expr.Value(wi.Value.NumGroups(LeafExpressionConverter.EvaluateQuotation(arguments.[0]) :?> int))   
                else
                    raise (new KernelCompilationException("Cannot evaluate buffer allocation size. The size depends on the work-item space but this space is not specified"))         
            else if m.DeclaringType.IsAssignableFrom(typeof<WorkItemInfo>) && (m.Name = "WorkDim") then
                if wi.IsSome then
                    Expr.Value(wi.Value.WorkDim())
                else
                    raise (new KernelCompilationException("Cannot evaluate buffer allocation size. The size depends on the work-item space but this space is not specified"))
            else
                if o.IsSome then
                    let evaluatedInstance = LiftArgumentsAndKernelCalls(o.Value, args, wi)
                    let liftedArgs = List.map(fun (e: Expr) -> LiftArgumentsAndKernelCalls(e, args, wi)) arguments
                    // Check if we need to tranform array method to openclbuffer method
                    if evaluatedInstance.Type = typeof<OpenCLBuffer> && o.Value.Type.IsArray then
                        Expr.Call(evaluatedInstance, evaluatedInstance.Type.GetMethod(m.Name), liftedArgs)
                    else
                        Expr.Call(
                            evaluatedInstance,
                            m, 
                            liftedArgs)
                else
                    let liftedArgs = List.map(fun (e: Expr) -> LiftArgumentsAndKernelCalls(e, args, wi)) arguments
                    Expr.Call(m, liftedArgs) 
        // Return allocation expression can contain references to arguments
        | Patterns.Var(v) ->
            if (args.ContainsKey(v.Name)) then
                let t = args.[v.Name].GetType()
                Expr.Value(args.[v.Name], t)
            else
                e                         
        // Array.Rank, Array.Length, Array.LongLength       
        | Patterns.PropertyGet(o, pi, arguments) ->
            if o.IsSome then
                let evaluatedInstance = LiftArgumentsAndKernelCalls(o.Value, args, wi)
                let liftedArgs = List.map(fun (e: Expr) -> LiftArgumentsAndKernelCalls(e, args, wi)) arguments
                // Check if we need to tranform array property to openclbuffer property
                if evaluatedInstance.Type = typeof<OpenCLBuffer> && o.Value.Type.IsArray then
                    Expr.PropertyGet(evaluatedInstance, evaluatedInstance.Type.GetProperty(pi.Name), liftedArgs)
                else
                    Expr.PropertyGet(evaluatedInstance, pi, liftedArgs)
            else
                let liftedArgs = List.map(fun (e: Expr) -> LiftArgumentsAndKernelCalls(e, args, wi)) arguments
                Expr.PropertyGet(pi, liftedArgs)                
        | ExprShape.ShapeVar(v) ->
            e
        | ExprShape.ShapeLambda(l, b) ->
            failwith "Error in substituting parameters"
        | ExprShape.ShapeCombination(c, argsList) ->
            let ev = List.map(fun (e: Expr) -> LiftArgumentsAndKernelCalls(e, args, wi)) argsList
            ExprShape.RebuildShapeCombination(c, ev)

    let EvaluateBufferAllocationSize(sizes: Expr array,
                                     args: Dictionary<string, obj>, 
                                     wi: WorkItemInfo option) =   
        let intSizes = new List<int64>()    
        for exp in sizes do
            let lifted = LiftArgumentsAndKernelCalls(exp, args, wi)
            let evaluated = LeafExpressionConverter.EvaluateQuotation(lifted)
            if evaluated :? int32 then
                intSizes.Add(evaluated :?> int32 |> int64)
            else
                intSizes.Add(evaluated :?> int64)
        intSizes |> Seq.toArray

    let PrepareKernelAguments(rk: OpenCLKernelCreationResult, 
                              input: ExecutionOutput[], 
                              collectionVars: Dictionary<Var, obj>,
                              workSize: WorkSize option,
                              pool: BufferPoolManager,
                              isRoot: bool,
                              sharePriority: BufferSharePriority) =   
        let buffers = new Dictionary<string, OpenCLBuffer>()
        let arguments = new Dictionary<string, obj>()
        let mutable outputFromThisKernel = ReturnedValue(())
       
        let mutable inputIndex = 0
        let mutable outsiderIndex = 0
        let mutable argIndex = 0

        // Foreach parameter of the kernel
        let parameters = rk.KernelData.Kernel.Parameters
        for par in parameters do   
            match par.ParameterType with
            // A buffer returned declared locally
            | FunctionParameterType.DynamicArrayParameter(sizes) ->                 
                let elementCount = EvaluateBufferAllocationSize(sizes, arguments, 
                                                                if workSize.IsNone then
                                                                    None
                                                                else
                                                                    Some(workSize.Value :> WorkItemInfo))
                // Buffer allocated in the body of the kernel are traslated into additional arguments
                // To setup these arguments in OpenCL, the buffers must be pre-allocated
                let globalSize, localSize, globalOffset = 
                    if workSize.IsNone then
                        null, null, null
                    else
                        workSize.Value.GlobalSize(), workSize.Value.LocalSize(), workSize.Value.GlobalOffset()
                let elementType = par.DataType.GetElementType()
                                        
                // Store dim sizes
                let sizeParameters = par.SizeParameters
                for i = 0 to sizeParameters.Count - 1 do
                    arguments.Add(sizeParameters.[i].Name, elementCount.[i])
                        
                // Allocate the buffer
                let buffer = pool.RequireBufferForParameter(par, None, elementCount, rk.DeviceData.Context, rk.DeviceData.Queue, isRoot, sharePriority)
                    
                // Set kernel arg
                rk.CompiledKernelData.Kernel.SetMemoryArgument(argIndex, buffer)    
                                  
                // Store buffer/object data
                arguments.Add(par.Name, buffer)
                buffers.Add(par.Name, buffer)
            
                // Set if returned
                if par.IsReturned then
                    outputFromThisKernel <- ReturnedUntrackedBuffer(buffer)

            // A regular parameter 
            | FunctionParameterType.NormalParameter ->
                let ob = input.[inputIndex]
                if par.DataType.IsArray then    
                    let elementType = par.DataType.GetElementType()  
                    let arr, lengths, isBuffer = 
                        match ob with
                        | ReturnedUntrackedBuffer(b) ->
                            None, b.Count, true
                        | ReturnedTrackedBuffer(b, v) ->
                            Some(v), b.Count, true
                        | ReturnedValue(v) ->
                            if v.GetType().IsArray then
                                Some(v :?> Array), ArrayUtil.GetArrayLengths(v), false
                            else
                                // Impossible
                                failwith "Error"

                    // Create buffer if needed (no local address space)
                    let addressSpace = par.Meta.Get<AddressSpaceAttribute>()
                    if addressSpace.AddressSpace = AddressSpace.Local then
                        rk.CompiledKernelData.Kernel.SetLocalArgument(argIndex, (lengths |> Array.reduce(*)) * (System.Runtime.InteropServices.Marshal.SizeOf(elementType) |> int64)) 
                        // Store dim sizes
                        let sizeParameters = par.SizeParameters
                        //bufferSizes.Add(par.Name, new Dictionary<string, int>())
                        for i = 0 to sizeParameters.Count - 1 do
                            //bufferSizes.[par.Name]
                            arguments.Add(sizeParameters.[i].Name, lengths.[i])
                    else
                        // Get buffer or try reuse the input one
                        let buffer = 
                            if isBuffer |> not then
                                pool.RequireBufferForParameter(par, Some(arr.Value), lengths, rk.DeviceData.Context, rk.DeviceData.Queue, isRoot, sharePriority) 
                            else
                                pool.RequireBufferForParameter(par, None, lengths, rk.DeviceData.Context, rk.DeviceData.Queue, isRoot, sharePriority, input.[inputIndex]) 
                            
                        // Set kernel arg
                        rk.CompiledKernelData.Kernel.SetMemoryArgument(argIndex, buffer)                      
                        // Store dim sizes
                        let sizeParameters = par.SizeParameters
                        for i = 0 to sizeParameters.Count - 1 do
                            arguments.Add(sizeParameters.[i].Name, lengths.[i])
                        // Store argument and buffer
                        arguments.Add(par.Name, arr.Value)
                        buffers.Add(par.Name, buffer)
                        // Check if this is returned
                        if par.IsReturned then
                            if isBuffer then
                                outputFromThisKernel <- ReturnedUntrackedBuffer(buffer)
                            else
                                outputFromThisKernel <- ReturnedTrackedBuffer(buffer, arr.Value)
                else
                    // An normal input that is not of type array is not
                    // associated to any buffer su must be wrapped in a ReturnedValue
                    match ob with
                    | ReturnedValue(v) ->
                        rk.CompiledKernelData.Kernel.SetValueArgumentAsObject(argIndex, v)
                    | _ ->
                        raise (new KernelSetupException("The parameter " + par.Name + " is scalar but it is wrapped in struct different from ReturnedValue(v)"))
                inputIndex <- inputIndex + 1

            // A reference to a collection variable
            // or to an env value
            | FunctionParameterType.EnvVarParameter(_)
            | FunctionParameterType.OutValParameter(_) ->
                let ob =
                    match par.ParameterType with
                    | FunctionParameterType.EnvVarParameter(v) ->
                        // Determine the value associated to the collection var
                        collectionVars.[v]
                    | FunctionParameterType.OutValParameter(e) ->
                        // Evaluate the expr
                        LeafExpressionConverter.EvaluateQuotation(e)
                    | _ ->
                        // Impossible
                        null
                if par.DataType.IsArray then    
                    let elementType = par.DataType.GetElementType()  
                    let arr, lengths = 
                        ob :?> Array,
                        ArrayUtil.GetArrayLengths(ob :?> Array)

                    // Get buffer or try reuse the input one
                    let buffer = 
                        pool.RequireBufferForParameter(par, Some(arr), lengths, rk.DeviceData.Context, rk.DeviceData.Queue, isRoot, sharePriority) 
                            
                    // Set kernel arg
                    rk.CompiledKernelData.Kernel.SetMemoryArgument(argIndex, buffer)                      
                    // Store dim sizes
                    let sizeParameters = par.SizeParameters
                    for i = 0 to sizeParameters.Count - 1 do
                        arguments.Add(sizeParameters.[i].Name, lengths.[i])
                    // Store argument and buffer
                    arguments.Add(par.Name, arr)
                    buffers.Add(par.Name, buffer)
                    // Check if this is returned
                    if par.IsReturned then
                        outputFromThisKernel <- ReturnedTrackedBuffer(buffer, arr)
                else
                    // Ref to a scalar
                    rk.CompiledKernelData.Kernel.SetValueArgumentAsObject(argIndex, ob)                    
                outsiderIndex <- outsiderIndex + 1
    
            // A size parameter
            | FunctionParameterType.SizeParameter ->
                // We can access succesfully "arguments" cause 
                // length args come always later than the relative array
                let v = arguments.[par.Name]             
                rk.CompiledKernelData.Kernel.SetValueArgument<int>(argIndex, v :?> int64 |> int)
                
            // An implicit parameter
            | FunctionParameterType.ImplicitParameter ->
                raise (new KernelSetupException("Cannot handle implicit parameters in default kernel execution"))

            // Update arg index
            argIndex <- argIndex + 1
        arguments, buffers, outputFromThisKernel
