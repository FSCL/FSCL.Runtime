﻿namespace FSCL.Runtime.RuntimeSteps

open System
open FSCL.Compiler
open FSCL.Runtime
open FSCL.Runtime.Managers
open FSCL.Runtime.Language
open System.Collections.Generic
open System.Reflection
open Cloo
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Linq.RuntimeHelpers

[<assembly:DefaultComponentAssembly>]
do()

[<StepProcessor("FSCL_FLOW_GRAPH_DEFAULT_PROCESSOR", "FSCL_FLOW_GRAPH_BUILDING_STEP")>]
type DefaultFlowGraphBuildingProcessor() =      
    inherit CompilerStepProcessor<KernelCreationResult, FlowGraphNode option>()
        
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
            if evaluated :? int32 then
                intSizes.Add(evaluated :?> int32 |> int64)
            else
                intSizes.Add(evaluated :?> int64)
        intSizes |> Seq.toArray

    override this.Run(input, s, opts) =
        let step = s :?> FlowGraphBuildingStep

        // Create flow graph node
        let node = new FlowGraphNode(input.DeviceData, input.KernelData, input.CompiledKernelData)

        // Set node input
        let parameters = input.KernelData.Kernel.Parameters
        for i = 0 to parameters.Count - 1 do
            let p = parameters.[i]
            if p.DataType.IsArray then
                // Check if output of a kernel (this i possible only if this is a normal parameter, that is visible to the user)
                let processedParam = 
                    match p.ParameterType with
                    | NormalParameter ->
                        step.Process(input.CallArgs.[i])
                    | _ ->
                        None

                match processedParam with
                | Some(precNode) ->
                    FlowGraphUtil.SetNodeInput(node,
                                                p.Name,
                                                KernelOutput(precNode, 0))
                | None ->
                    match p.ParameterType with
                        | NormalParameter ->
                            FlowGraphUtil.SetNodeInput(node, 
                                                        p.Name, 
                                                        ActualArgument(input.CallArgs.[i]))
                        | DynamicParameter(allocArgs) ->
                            FlowGraphUtil.SetNodeInput(node, 
                                                        p.Name, 
                                                        BufferAllocationSize(fun(args, localSize, globalSize) ->
                                                            this.EvaluateBufferAllocationSize(p.DataType, allocArgs, args, localSize, globalSize)))
                        (*
                        | GeneratedParameterWithKnownSize(name, eval) ->
                            FlowGraphUtil.SetNodeInput(node, 
                                                        p.Name, 
                                                        BufferAllocationSize(fun(args, localSize, globalSize) ->
                                                            eval(args.[name] :?> Array)))           
                                                            *)
                        | _ ->
                            raise (new KernelFlowGraphException("Cannot build flow graph input for parameter " + p.Name))                     
            else
                match p.ParameterType with
                | NormalParameter ->
                    FlowGraphUtil.SetNodeInput(node, p.Name, ActualArgument(input.CallArgs.[i]))
                | SizeParameter ->
                    FlowGraphUtil.SetNodeInput(node, p.Name, SizeArgument)
                | _ ->
                    ()
            

        // Create input for next step
        Some(node)

                  