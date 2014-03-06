﻿namespace FSCL.Runtime.CacheInspection

open System
open System.Reflection
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open FSCL.Compiler
open FSCL.Runtime
open Microsoft.FSharp.Linq.RuntimeHelpers
open Cloo

[<Step("FSCL_CACHE_INSPECTION_STEP", 
       Dependencies = [| "FSCL_MODULE_PARSING_STEP" |], 
       Before = [| "FSCL_MODULE_PREPROCESSING_STEP" |])>] 
type CacheInspectionStep(tm: TypeManager,
                          processors: ICompilerStepProcessor list) = 
    inherit CompilerStep<KernelModule, KernelModule>(tm, processors)
    
    member private this.LiftArgumentsAndKernelCalls(e: Expr,
                                                    args: Dictionary<string, obj>,
                                                    localSize: int64 array,
                                                    globalSize: int64 array) =
        match e with
        // Return allocation expression can contain a call to global_size, local_size, num_groups or work_dim
        | Patterns.Call(o, m, arguments) ->
            if m.DeclaringType.Name = "KernelLanguage" && (m.Name = "get_global_size") then
                Expr.Value(globalSize.[LeafExpressionConverter.EvaluateQuotation(arguments.[0]) :?> int])
            else if m.DeclaringType.Name = "KernelLanguage" && (m.Name = "get_local_size") then
                Expr.Value(localSize.[LeafExpressionConverter.EvaluateQuotation(arguments.[0]) :?> int])
            else if m.DeclaringType.Name = "KernelLanguage" && (m.Name = "get_num_groups") then
                let gs = globalSize.[LeafExpressionConverter.EvaluateQuotation(arguments.[0]) :?> int]
                let ls = localSize.[LeafExpressionConverter.EvaluateQuotation(arguments.[0]) :?> int]
                Expr.Value(int (Math.Ceiling(float gs / float ls)))
            else if m.DeclaringType.Name = "KernelLanguage" && (m.Name = "get_work_dim") then
                Expr.Value(globalSize.Rank)
            else
                if o.IsSome then
                    let evaluatedIstance = this.LiftArgumentsAndKernelCalls(o.Value, args, localSize, globalSize);
                    let liftedArgs = List.map(fun (e: Expr) -> this.LiftArgumentsAndKernelCalls(e, args, localSize, globalSize)) arguments;
                    Expr.Call(
                        evaluatedIstance,
                        m, 
                        liftedArgs)
                else
                    Expr.Call(
                        m, List.map(fun (e: Expr) -> this.LiftArgumentsAndKernelCalls(e, args, localSize, globalSize)) arguments)
        // Return allocation expression can contain references to arguments
        | Patterns.Var(v) ->
            if (args.ContainsKey(v.Name)) then
                let t = args.[v.Name].GetType()
                Expr.Value(args.[v.Name], t)
            else
                e                
        | ExprShape.ShapeVar(v) ->
            e
        | ExprShape.ShapeLambda(l, b) ->
            failwith "Error in substituting parameters"
        | ExprShape.ShapeCombination(c, argsList) ->
            ExprShape.RebuildShapeCombination(c, List.map(fun (e: Expr) -> this.LiftArgumentsAndKernelCalls(e, args, localSize, globalSize)) argsList)

    member private this.EvaluateReturnedBufferAllocationSize(t: Type,
                                                             sizes: Expr list,
                                                             args: Dictionary<string, obj>, 
                                                             localSize: int64 array,
                                                             globalSize: int64 array) =   
        let intSizes = new List<int64>()    
        for exp in sizes do
            let lifted = this.LiftArgumentsAndKernelCalls(exp, args, localSize, globalSize)
            let evaluated = LeafExpressionConverter.EvaluateQuotation(lifted)
            intSizes.Add((evaluated :?> int32) |> int64)
        ExplicitAllocationSize(intSizes |> Seq.toArray)    
               
    override this.Run(kmodule, opts) =
        if kmodule.CustomInfo.ContainsKey("RUNTIME_CACHE") then
            // Get cache
            let cache = kmodule.CustomInfo.["RUNTIME_CACHE"] :?> RuntimeCache
            // Skip kernels already compiled
            for k in kmodule.GetKernels() do
                // If a mathing kernel has been cached and it contains the opencl source code
                if cache.Kernels.ContainsKey(k.Info.ID) && cache.Kernels.[k.Info.ID].OpenCLCode.IsSome then
                    let cachedKernel = cache.Kernels.[k.Info.ID]
                    k.Info.Skip <- true
                    k.Info.Body <- cachedKernel.Info.Body
                    k.Info.Code <- cachedKernel.Info.Code
                    k.Info.Name <- cachedKernel.Info.Name
                    k.Info.ReturnType <- cachedKernel.Info.ReturnType
                    for item in cachedKernel.Info.CustomInfo do
                        if not (k.Info.CustomInfo.ContainsKey(item.Key)) then
                            k.Info.CustomInfo.Add(item.Key, item.Value)  
                    for item in cachedKernel.Info.Parameters do
                        k.Info.Parameters.Add(item)

                    // We are not going to execute further compiler steps
                    // in particular the function preprocessing steps
                    // Function preprocessing determines some inputs for the flow graph node,
                    // for example when the kernel has a dynamic alloc array
                    // We therefore need to inspect parameters and act like function preprocessing
                    // creating the missing flow graph inputs
                    let dynArray = Seq.tryFind(fun (p:KernelParameterInfo) -> p.IsDynamicArrayParameter) k.Info.Parameters
                    // Get flow graph nodes matching the current kernel    
                    let nodes = FlowGraphManager.GetKernelNodes(k.Info.ID, kmodule.FlowGraph)
                    if dynArray.IsSome then                        
                        // Set flow graph argument
                        for item in nodes do
                            FlowGraphManager.SetNodeInput(item, 
                                                            dynArray.Value.Name, 
                                                            BufferAllocationSize(
                                                            fun(args, localSize, globalSize) ->
                                                                this.EvaluateReturnedBufferAllocationSize(
                                                                    dynArray.Value.Type.GetElementType(), 
                                                                    dynArray.Value.DynamicAllocationArguments, 
                                                                    args, localSize, globalSize))) 

                    // Set implicit node input for each array length arg
                    for p in k.Info.Parameters do
                        if p.IsSizeParameter then
                            for item in nodes do
                                FlowGraphManager.SetNodeInput(item,
                                                                p.Name,
                                                                ImplicitValue)
        kmodule