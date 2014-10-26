﻿namespace FSCL.Runtime.RuntimeSteps

open System
open FSCL.Compiler
open FSCL.Compiler.Util
open FSCL.Runtime
open FSCL.Runtime.Managers
open FSCL.Language
open System.Collections.Generic
open System.Reflection
open FSCL.Language
open OpenCL
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Linq.RuntimeHelpers
open FSCL.Compiler.AcceleratedCollections

[<StepProcessor("FSCL_FLOW_GRAPH_ACCELERATED_PROCESSOR", 
                "FSCL_FLOW_GRAPH_BUILDING_STEP",
                Before = [| "FSCL_FLOW_GRAPH_DEFAULT_PROCESSOR" |])>]
type AcceleratedFlowGraphBuildingProcessor() =      
    inherit CompilerStepProcessor<ComputationCreationResult, FlowGraphNode option>()
        
    member private this.LiftArgumentsAndKernelCalls(e: Expr,
                                                    args: Dictionary<string, obj>,
                                                    localSize: int64 array,
                                                    globalSize: int64 array) =
        match e with
        // Return allocation expression can contain a call to global_size, local_size, num_groups or work_dim
        | Patterns.Call(o, m, arguments) ->
            if m.DeclaringType.Name = "Language" && (m.Name = "get_global_size") then
                Expr.Value(globalSize.[LeafExpressionConverter.EvaluateQuotation(arguments.[0]) :?> int])
            else if m.DeclaringType.Name = "Language" && (m.Name = "get_local_size") then
                Expr.Value(localSize.[LeafExpressionConverter.EvaluateQuotation(arguments.[0]) :?> int])
            else if m.DeclaringType.Name = "Language" && (m.Name = "get_num_groups") then
                let gs = globalSize.[LeafExpressionConverter.EvaluateQuotation(arguments.[0]) :?> int]
                let ls = localSize.[LeafExpressionConverter.EvaluateQuotation(arguments.[0]) :?> int]
                Expr.Value(int (Math.Ceiling(float gs / float ls)))
            else if m.DeclaringType.Name = "Language" && (m.Name = "get_work_dim") then
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
                    (*
                if m.DeclaringType <> null && m.DeclaringType.Name = "Array" && m.Name = "GetLength" then
                    match arguments.[0] with
                    | Patterns.Value(v) ->
                        let t = o.Value.GetType()
                        let size = t.GetMethod("GetLength").Invoke(o.Value, [| v |])
                        Expr.Value(size)
                    | _ ->
                        failwith "Error in substituting parameters"
                else
                    failwith "Error in substituting parameters"*)
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

    member private this.EvaluateBufferAllocationSize(t: Type,
                                                     sizes: Expr array,
                                                     args: Dictionary<string, obj>, 
                                                     localSize: int64 array,
                                                     globalSize: int64 array) =   
        let intSizes = new List<int64>()    
        for exp in sizes do
            let lifted = this.LiftArgumentsAndKernelCalls(exp, args, localSize, globalSize)
            let evaluated = LeafExpressionConverter.EvaluateQuotation(lifted)
            intSizes.Add(evaluated :?> System.Int64)
        intSizes |> Seq.toArray

    override this.Run(data, s, opts) =
        let step = s :?> FlowGraphBuildingStep

        match data with 
        | ComputationCreationResult.OpenCLKernel(input) ->
            // Check if this is an accelerated collections kernel
            if input.KernelData.Kernel :? AcceleratedKernelInfo then
                let info = input.KernelData.Kernel :?> AcceleratedKernelInfo

                // Create flow graph node
                let node = new OpenCLKernelFlowGraphNode(input.DeviceData, input.KernelData, input.CompiledKernelData)
            
                // Build node input
                let parameters = input.KernelData.Kernel.Parameters
                match info.CollectionFunctionName with
                | "Array.map"
                | "Array.mapi" ->
                    // Params: input, output, input_size, output_size
                    // Check if output of a kernel (this i possible only if this is a normal parameter, that is visible to the user)
                    let processedParam = 
                        step.Process(input.CallArgs.[1])
                    match processedParam with
                    | Some(precNode) ->
                        FlowGraphUtil.SetNodeInput(node,
                                                   parameters.[0].Name,
                                                   KernelOutput(precNode, 0))
                    | _ ->                    
                        FlowGraphUtil.SetNodeInput(node,
                                                   parameters.[0].Name,
                                                   ActualArgument(input.CallArgs.[1]))
                    FlowGraphUtil.SetNodeInput(node, parameters.[1].Name, BufferAllocationSize(fun(args, wi) ->
                                                                                // Same size of input
                                                                                // This can be an array or a buffer (if the output of another kernel)
                                                                                ArrayUtil.GetArrayOrBufferLengths(args.[parameters.[0].Name])))
                    FlowGraphUtil.SetNodeInput(node, parameters.[2].Name, ArraySizeArgument)
                    FlowGraphUtil.SetNodeInput(node, parameters.[3].Name, ArraySizeArgument)

                    // Create input for next step
                    Some(node :> FlowGraphNode)

                | "Array.map2"
                | "Array.mapi2" ->
                    // Params: input1, input2, output, input1_size, input2_size, output_size
                    // Check if output of a kernel (this i possible only if this is a normal parameter, that is visible to the user)
                    let processedParam1 = 
                        step.Process(input.CallArgs.[1])
                    match processedParam1 with
                    | Some(precNode) ->
                        FlowGraphUtil.SetNodeInput(node,
                                                   parameters.[0].Name,
                                                   KernelOutput(precNode, 0))
                    | _ ->                    
                        FlowGraphUtil.SetNodeInput(node,
                                                   parameters.[0].Name,
                                                   ActualArgument(input.CallArgs.[1]))                                               
                    let processedParam2 = 
                        step.Process(input.CallArgs.[2])
                    match processedParam2 with
                    | Some(precNode) ->
                        FlowGraphUtil.SetNodeInput(node,
                                                   parameters.[1].Name,
                                                   KernelOutput(precNode, 0))
                    | _ ->                    
                        FlowGraphUtil.SetNodeInput(node,
                                                   parameters.[1].Name,
                                                   ActualArgument(input.CallArgs.[2]))
                    FlowGraphUtil.SetNodeInput(node, parameters.[2].Name, BufferAllocationSize(fun(args, wi) ->
                                                                                // Same size of input
                                                                                // This can be an array or a buffer (if the output of another kernel)
                                                                                ArrayUtil.GetArrayOrBufferLengths(args.[parameters.[0].Name])))
                    FlowGraphUtil.SetNodeInput(node, parameters.[3].Name, ArraySizeArgument)
                    FlowGraphUtil.SetNodeInput(node, parameters.[4].Name, ArraySizeArgument)
                    FlowGraphUtil.SetNodeInput(node, parameters.[5].Name, ArraySizeArgument)
                
                    // Create input for next step
                    Some(node :> FlowGraphNode)

                | "Array.reduce"            
                | "Array.sum"  ->
                    // Params: input, local, output, input_size, local_size, output_size
                    // Check if output of a kernel (this i possible only if this is a normal parameter, that is visible to the user)
                    if info.CollectionFunctionName = "Array.sum" then
                        let processedParam = step.Process(input.CallArgs.[0])
                        match processedParam with
                        | Some(precNode) ->
                            FlowGraphUtil.SetNodeInput(node,
                                                       parameters.[0].Name,
                                                       KernelOutput(precNode, 0))
                        | _ ->                    
                            FlowGraphUtil.SetNodeInput(node,
                                                       parameters.[0].Name,
                                                       ActualArgument(input.CallArgs.[0]))  
                    else
                        let processedParam = step.Process(input.CallArgs.[1])
                        match processedParam with
                        | Some(precNode) ->
                            FlowGraphUtil.SetNodeInput(node,
                                                       parameters.[0].Name,
                                                       KernelOutput(precNode, 0))
                        | _ ->                    
                            FlowGraphUtil.SetNodeInput(node,
                                                       parameters.[0].Name,
                                                       ActualArgument(input.CallArgs.[1]))  
                    // If cpu then this is block_size otherwise it's a local array   
                    let devType = input.KernelData.Kernel.Meta.KernelMeta.Get<DeviceTypeAttribute>()  
                    if devType.Type = DeviceType.Cpu then                     
                        FlowGraphUtil.SetNodeInput(node, parameters.[1].Name, IntrinsicArgument)
                        FlowGraphUtil.SetNodeInput(node, parameters.[2].Name, IntrinsicArgument)
                        FlowGraphUtil.SetNodeInput(node, parameters.[3].Name, ArraySizeArgument)
                        FlowGraphUtil.SetNodeInput(node, parameters.[4].Name, ArraySizeArgument)
                    else                                
                        FlowGraphUtil.SetNodeInput(node, parameters.[1].Name, BufferAllocationSize(fun(args, wi) -> 
                                                                                                        [| wi.Value.LocalSize(0) |> int64 |]))
                        FlowGraphUtil.SetNodeInput(node, parameters.[2].Name, BufferAllocationSize(fun(args, wi) ->
                                                                                                        // Size is number of groups
                                                                                                        [| wi.Value.NumGroups(0) |> int64 |]))
                        FlowGraphUtil.SetNodeInput(node, parameters.[3].Name, ArraySizeArgument)
                        FlowGraphUtil.SetNodeInput(node, parameters.[4].Name, ArraySizeArgument)
                        FlowGraphUtil.SetNodeInput(node, parameters.[5].Name, ArraySizeArgument)

                    // Create input for next step
                    Some(node :> FlowGraphNode)
            
                | "Array.rev" ->
                    // Params: input, output, input_size, output_size
                    // Check if output of a kernel (this i possible only if this is a normal parameter, that is visible to the user)
                    let processedParam = 
                        step.Process(input.CallArgs.[0])
                    match processedParam with
                    | Some(precNode) ->
                        FlowGraphUtil.SetNodeInput(node,
                                                   parameters.[0].Name,
                                                   KernelOutput(precNode, 0))
                    | _ ->                    
                        FlowGraphUtil.SetNodeInput(node,
                                                   parameters.[0].Name,
                                                   ActualArgument(input.CallArgs.[0]))
                    FlowGraphUtil.SetNodeInput(node, parameters.[1].Name, BufferAllocationSize(fun(args, wi) ->
                                                                                // Same size of input
                                                                                // This can be an array or a buffer (if the output of another kernel)
                                                                                ArrayUtil.GetArrayOrBufferLengths(args.[parameters.[0].Name])))
                    FlowGraphUtil.SetNodeInput(node, parameters.[2].Name, ArraySizeArgument)
                    FlowGraphUtil.SetNodeInput(node, parameters.[3].Name, ArraySizeArgument)

                    // Create input for next step
                    Some(node :> FlowGraphNode)
                | _ ->
                    None
            else
                None

        // Multithread kernel        
        | ComputationCreationResult.MultithreadKernel(input) ->
            if input.KernelData :? AcceleratedKernelInfo then
                let node = new MultithreadKernelFlowGraphNode(None, input.KernelData, input.CallArgs)
                // Build node input
                let parameters = input.KernelData.ParsedSignature.GetParameters()
                for i = 1 to parameters.Length - 1 do
                    let processedParam = step.Process(input.CallArgs.[i])
                    match processedParam with
                    | Some(precNode) ->
                        FlowGraphUtil.SetNodeInput(node,
                                                    parameters.[i].Name,
                                                    KernelOutput(precNode, 0))
                    | _ ->                    
                        FlowGraphUtil.SetNodeInput(node,
                                                    parameters.[i].Name,
                                                    ActualArgument(input.CallArgs.[i])) 
                Some(node :> FlowGraphNode)
            else
                None

        // Never happens regular function
        | _ ->
            None

                  