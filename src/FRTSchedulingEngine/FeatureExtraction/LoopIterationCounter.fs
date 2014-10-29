namespace FSCL.Runtime.Scheduling.FRTSchedulingEngine.FeatureExtraction

open Microsoft.FSharp.Quotations
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Linq.RuntimeHelpers
open System.Collections.Generic
//open QuotEval.QuotationEvaluation

open Microsoft.FSharp.Linq

open FSCL.Runtime.Scheduling
open System
open FSCL.Runtime.Scheduling.QuotationUtil
open FSCL.Language
open FSCL.Compiler
open FSCL.Compiler.Util
open Microsoft.FSharp.Reflection
open FSCL.Runtime.Scheduling.VarStack
open System.Linq

type LoopIterationCountError(msg: string) =
    inherit System.Exception(msg)

type LoopIterationCounter() = 
    // Build an expression that contains the number of interesting items
    static member private Estimate(kmod: IKernelModule, 
                                   functionBody: Expr, 
                                   parameters: (ParameterInfo * Var)[],
                                   stack: VarStack) =
        let isParameterReference v = 
            (Array.tryFind (fun (p:ParameterInfo, pv:Var) -> pv = v) parameters).IsSome || v.Type = typeof<WorkItemInfo>
                      
        let rec EstimateInternal(expr: Expr, stack: VarStack) =
            // Check if this is an interesting item
            match expr with            
            | Patterns.Call(o, mi, arguments) ->
                // Check if this is a call to a reflected function
                match mi with
                | DerivedPatterns.MethodWithReflectedDefinition(b) ->
                    // It's a reflected method
                    // Get functioninfo
                    let calledFun =  kmod.Functions.Values |> Seq.find(fun v -> v.ParsedSignature = mi)
                    match QuotationAnalysis.FunctionsManipulation.GetCurriedOrTupledArgs(b) with
                    | thisVar, Some(paramVars) ->       
                        // Count inside arguments
                        let l, newStack = EstimateList(arguments, stack)
                        // We must be sure that variables used for arguments can be unfolded to expressions of parameters, constants an work size functions
                        let unfoldedArguments = arguments |> List.map(fun (it:Expr) -> KernelUtil.UnfoldExpression(it, newStack, isParameterReference))
                        // Build an evaluator for this function as if it was a kernel
                        let evaluatorExpr = LoopIterationCounter.Count(kmod, calledFun)
                       
                        let mutable call = evaluatorExpr
                        for a in unfoldedArguments do
                            call <- Expr.Application(call, a)
                        if thisVar.IsSome then
                            call <- Expr.Application(call, o.Value)

                        <@ %l + %%call:float32 @>, stack
                    | _ ->      
                        EstimateList(arguments, stack)
                | _ ->      
                    EstimateList(arguments, stack)

            | Patterns.Let(va, value, body) ->
                let maybeOpRange: (Expr<float32> * VarStack) option = EstimateLoopWithOpRange(expr, stack)
                if maybeOpRange.IsSome then
                    maybeOpRange.Value
                else
                    let _, newStack = EstimateInternal(value, stack)
                    let bodyCount, newStack = EstimateInternal(body, push stack (va, value))
                    bodyCount, pop newStack

            | Patterns.ForIntegerRangeLoop(v, starte, ende, body) ->
                let newStack = push stack (v, starte)

                let unfoldStart = KernelUtil.UnfoldExpression(starte, stack, isParameterReference)
                let unfoldEnd = KernelUtil.UnfoldExpression(ende, newStack, isParameterReference)
                let iterCount = 
                    <@
                        if ((%%unfoldStart : int) > (%%unfoldEnd : int)) then
                            (((float32)(%%unfoldStart : int) - (float32)(%%unfoldEnd : int) + 1.0f))
                        else
                            (((float32)(%%unfoldEnd : int) - (float32)(%%unfoldStart : int) + 1.0f))
                    @>      
                let bodyCount, bodyStack = EstimateInternal(body, newStack)

                <@ (%iterCount) * Math.Max(%bodyCount, 1.0f) @>, pop bodyStack
                  
            | Patterns.WhileLoop(guard, body) ->
                let totalIterCount = EstimateSimpleWhileLoop(guard, body, stack)
                totalIterCount, stack

            | ExprShape.ShapeVar(_) ->
                <@ 0.0f @>, stack

            | ExprShape.ShapeLambda(v, l) ->
                let bodyCount, newStack = EstimateInternal(l, stack)
                bodyCount, newStack
            
            | ExprShape.ShapeCombination(o, l) ->
                EstimateList(l, stack)               
              
        and EstimateList(l: Expr list, stack: VarStack) =
            let mutable totalIterCount = <@ 0.0f @>
            let mutable newStack = stack
            for item in l do
                let itemCount, nStack = EstimateInternal(item, newStack)
                totalIterCount <- <@ %totalIterCount + %itemCount @>
                newStack <- nStack
            totalIterCount, newStack   

        and EstimateLoopWithOpRange(expr: Expr, stack: VarStack) =
            match expr with
            | Patterns.Let (inputSequence, value, body) ->
                match value with 
                | Patterns.Call(ob, mi, a) ->
                    if mi.Name = "op_RangeStep" then
                        // The args is the beginning, the step and the end of the iteration
                        let starte, stepe, ende = a.[0], a.[1], a.[2]
                        match body with
                        | Patterns.Let(enumerator, value, body) ->
                            let newStack = push stack (enumerator, value)
                            match body with
                            | Patterns.TryFinally (trye, fine) ->
                                match trye with
                                    | Patterns.WhileLoop(cond, body) ->
                                        match body with
                                        | Patterns.Let(v, value, body) ->
                                            match value with
                                            | Patterns.PropertyGet(e, pi, a) ->
                                                // Ok, that's an input sequence!
                                                let newStack = push newStack (v, starte)
                                                let unfoldStart = KernelUtil.UnfoldExpression(starte, stack, isParameterReference)
                                                let unfoldStep = KernelUtil.UnfoldExpression(stepe, stack, isParameterReference)
                                                let unfoldEnd = KernelUtil.UnfoldExpression(ende, stack, isParameterReference)
                                                let totalIterCount = <@
                                                                        ((float32)(Math.Ceiling((float)(((float32)(%%unfoldEnd:int) - (float32)(%%unfoldStart:int) + 1.0f) / (float32)(%%unfoldStep:int)))))
                                                                     @>
                                                let bodyCount, subStack = EstimateInternal(body, newStack)
                                                Some(<@ (%totalIterCount) * Math.Max(%bodyCount, 1.0f) @>, (subStack |> pop |> pop))
                                            | _ -> None
                                        | _ -> None
                                    | _ -> None
                                | _ -> None
                            | _ -> None
                        | _ -> None                                           
                    else
                        None
                | _ -> 
                    None  
            | _ -> 
                None

        and CheckModification(vs: Var list, expr: Expr, varModification: Dictionary<Var, List<Expr>>, validModificationContext: bool) =
            match expr with
            | Patterns.VarSet(v, value) ->
                if ((List.tryFind (fun vl -> vl = v) vs).IsSome) then 
                    // One of the while loop guard variables is modified here
                    if not validModificationContext then
                        false
                    else
                        // Add this modification to the set of modifications for the variable
                        if not (varModification.ContainsKey(v)) then
                            varModification.Add(v, new List<Expr>())
                        varModification.[v].Add(value)
                        CheckModification(vs, value, varModification, true)
                else
                    CheckModification(vs, value, varModification, true)
            | Patterns.WhileLoop(guard, body) ->
                // Not a valid modification context
                let first = CheckModification(vs, guard, varModification, false)
                first && CheckModification(vs, body, varModification, false)
            | Patterns.IfThenElse(guard, ifb, elseb) ->
                // Not a valid modification context
                let first = CheckModification(vs, guard, varModification, false)
                let snd = first && CheckModification(vs, ifb, varModification, false)
                snd && CheckModification(vs, elseb, varModification, false)
            | Patterns.ForIntegerRangeLoop(v, st, en, b) ->
                // Not a valid modification context
                let first = CheckModification(vs, st, varModification, false)
                let snd = first && CheckModification(vs, en, varModification, false)
                snd && CheckModification(vs, b, varModification, false)
            | ExprShape.ShapeVar(v) ->
                true
            | ExprShape.ShapeLambda(v, b) ->                
                CheckModification(vs, b, varModification, true)
            | ExprShape.ShapeCombination(o, bl) ->               
                if bl.Length > 0 then
                    bl |> List.map (fun it -> CheckModification(vs, it, varModification, true)) |> List.reduce(fun a b -> a && b)
                else
                    true

        and EstimateSimpleWhileLoop(guard:Expr, body:Expr, stack: VarStack) =
            let rec findVarUpdate(v: Var, e: Expr) =
                match e with
                | Patterns.VarSet(ov, assExpr) ->
                    if ov = v then
                        let otherUpdate:(Expr * Expr) option = findVarUpdate(v, assExpr)
                        if otherUpdate.IsNone then
                            // Check the form v <- v OP expr
                            match assExpr with
                            | DerivedPatterns.SpecificCall <@ (+) @> (o, t, arguments) 
                            | DerivedPatterns.SpecificCall <@ (-) @> (o, t, arguments) 
                            | DerivedPatterns.SpecificCall <@ (*) @> (o, t, arguments) 
                            | DerivedPatterns.SpecificCall <@ (/) @> (o, t, arguments) 
                            | DerivedPatterns.SpecificCall <@ (>>>) @> (o, t, arguments) 
                            | DerivedPatterns.SpecificCall <@ (<<<) @> (o, t, arguments) ->
                                match arguments.[0] with
                                | Patterns.Var(ov) ->
                                    if ov = v then
                                        // Ok, now make sure expr can be unfold to parameters and constants
                                        let unfoldUpdate = KernelUtil.UnfoldExpression(arguments.[1], stack, isParameterReference)
                                        Some(assExpr, unfoldUpdate)
                                    else
                                        raise (new LoopIterationCountError("Cannot estimate the trip count of a while body where the guard variable is updated using an expression that differs from VAR <- VAR OP EXPR"))
                                | _ ->
                                    raise (new LoopIterationCountError("Cannot estimate the trip count of a while body where the guard variable is updated using an expression that differs from VAR <- VAR OP EXPR"))
                            | _ ->
                                raise (new LoopIterationCountError("Cannot estimate the trip count of a while body where the guard variable is updated using an expression that differs from VAR <- VAR OP EXPR"))                                
                        else
                            raise (new LoopIterationCountError("Cannot estimate the trip count of a while body where the same variable is updated more then once"))
                    else
                        findVarUpdate(v, assExpr)       
                | Patterns.ForIntegerRangeLoop(_, starte, ende, body) ->
                    let update = findVarUpdate(v, body)
                    if update.IsSome then
                        raise (new LoopIterationCountError("Cannot estimate the trip count of a while body where the guard variable is updated in an if-then-else or in a nested loop"))
                    else
                        None  
                | Patterns.WhileLoop(_, body) ->
                    let update = findVarUpdate(v, body)
                    if update.IsSome then
                        raise (new LoopIterationCountError("Cannot estimate the trip count of a while body where the guard variable is updated in an if-then-else or in a nested loop"))
                    else
                        None  
                | Patterns.IfThenElse(_, ifb, elseb) ->
                    let update1 = findVarUpdate(v, ifb)
                    let update2 = findVarUpdate(v, elseb)
                    if update1.IsSome || update2.IsSome then
                        raise (new LoopIterationCountError("Cannot estimate the trip count of a while body where the guard variable is updated in an if-then-else or in a nested loop"))
                    else
                        None                        
                | ExprShape.ShapeVar(ov) ->
                    None
                | ExprShape.ShapeLambda(ov, e) ->
                    findVarUpdate(v, e)
                | ExprShape.ShapeCombination(o, a) ->
                    if a.Length > 0 then
                        a |> List.map (fun i -> findVarUpdate(v, i)) |> List.reduce (fun a b -> match a, b with
                                                                                                            | None, None ->
                                                                                                                None
                                                                                                            | a, None ->
                                                                                                                a
                                                                                                            | None, a ->
                                                                                                                a
                                                                                                            | Some(a), Some(b) -> 
                                                                                                                raise (new LoopIterationCountError("Cannot estimate the trip count of a while body where the same variable is updated more then once")))
                    else
                        None
                                        
            // Check if guard is Expr.Var OP Expr            
            match guard with            
            | DerivedPatterns.SpecificCall <@ (>) @> (e, t, a)  
            | DerivedPatterns.SpecificCall <@ (<) @> (e, t, a)  
            | DerivedPatterns.SpecificCall <@ (>=) @> (e, t, a) 
            | DerivedPatterns.SpecificCall <@ (<=) @> (e, t, a) ->
                match a.[0] with
                | Patterns.Var(guardVar) ->
                    // Verify that guardVar has not mutated from the binding
                    let bindingValue, _ = findAndTail guardVar true stack
                    if bindingValue.IsSome then
                        // Unfold the guard expr
                        let unfoldCond = KernelUtil.UnfoldExpression(a.[1], stack, isParameterReference)
                        // Search in the body the ONLY update in the form v <- v +-*/>>><<< expr
                        let update = findVarUpdate(guardVar, body)
                        match update with
                        | Some(updateOp, updateExpr) ->
                            // Count interesting things in body
                            // Determine the rounding
                            let increment = 
                                match guard with            
                                | DerivedPatterns.SpecificCall <@ (>=) @> (e, t, a) 
                                | DerivedPatterns.SpecificCall <@ (<=) @> (e, t, a) ->
                                    1.0
                                | _ ->
                                    0.0
                            // Build estimator
                            let totalIterCount =
                                match updateOp with
                                | DerivedPatterns.SpecificCall <@ (+) @> (o, t, arguments) ->
                                    <@ ((((%%unfoldCond |> float32) - (%%bindingValue.Value |> float32)) / (%%updateExpr |> float32)) + (increment |> float32)) @>
                                | DerivedPatterns.SpecificCall <@ (-) @> (o, t, arguments) ->
                                    <@ ((((%%bindingValue.Value |> float32) - (%%unfoldCond |> float32)) / (%%updateExpr |> float32)) + (increment |> float32)) @>
                                | DerivedPatterns.SpecificCall <@ (*) @> (o, t, arguments) ->
                                    <@ (Math.Floor(Math.Log((%%unfoldCond |> float), (%%updateExpr |> float)) - Math.Log((%%bindingValue.Value |> float), (%%updateExpr |> float)) + increment) |> float32) @> 
                                | DerivedPatterns.SpecificCall <@ (/) @> (o, t, arguments)-> 
                                    <@ (Math.Floor(Math.Log((%%bindingValue.Value |> float), (%%updateExpr |> float)) - Math.Log((%%unfoldCond |> float), (%%updateExpr |> float)) + increment)  |> float32) @> 
                                | DerivedPatterns.SpecificCall <@ (>>>) @> (o, t, arguments) ->
                                    <@ (Math.Floor(Math.Log((%%bindingValue.Value |> float), (Math.Pow(2.0, %%updateExpr|> float))) - Math.Log((%%unfoldCond |> float), (Math.Pow(2.0, %%updateExpr|> float))) + increment) |> float32) @>  
                                | DerivedPatterns.SpecificCall <@ (<<<) @> (o, t, arguments) ->
                                    <@ (Math.Floor(Math.Log((%%unfoldCond |> float), (Math.Pow(2.0, %%updateExpr|> float))) - Math.Log((%%bindingValue.Value |> float), (Math.Pow(2.0, %%updateExpr|> float))) + increment) |> float32) @> 
                                | _ ->
                                    raise (new LoopIterationCountError("Cannot estimate the trip count of a while body where the guard variable is updated using an expression that differs from VAR <- VAR OP EXPR"))                                

                            let bodyCount, subStack = EstimateInternal(body, stack)
                            <@ (%totalIterCount) * Math.Max(%bodyCount, 1.0f) @>
                        | _ ->                            
                            raise (new LoopIterationCountError("Cannot find the variable update of a while loop"))                                
                    else
                        raise (new LoopIterationCountError("Cannot determine the original value of a while loop iteration variable"))                                                                
                | _ ->
                    raise (new LoopIterationCountError("Cannot estimate the trip count of a while body where the guard is not in the form VAR COMP_OP EXPR"))                                
            | _ ->
                raise (new LoopIterationCountError("Cannot estimate the trip count of a while body where the guard is not in the form VAR COMP_OP EXPR"))                                
                   
        EstimateInternal(functionBody, stack)            
            
    static member Count(kmod: IKernelModule,
                        kfun: IFunctionInfo) = 
                        
        let parameters = kmod.Kernel.OriginalParameters |> 
                         Seq.map(fun (p: IOriginalFunctionParameter) -> 
                                    (p.OriginalParamterInfo, p.OriginalPlaceholder)) |>
                         Array.ofSeq   

        // Create a lambda to evaluate instruction count
        let prepBody = 
            QuotationUtil.ToCurriedFunction(kfun.OriginalBody)

        // Create a lambda to evaluate instruction count
        //let workItemIdContainerPlaceholder = Quotations.Var("workItemIdContainer", typeof<WorkItemIdContainer>)
        let precExpr, newStack = LoopIterationCounter.Estimate(kmod, prepBody, parameters, EmptyStack)
        let cleanCountExpr = KernelUtil.EvaluateClosedSubtrees(precExpr)
        (KernelUtil.CloseExpression(prepBody, cleanCountExpr).Value)
            



        

      