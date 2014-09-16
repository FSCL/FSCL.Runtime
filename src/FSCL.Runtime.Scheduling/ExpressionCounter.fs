namespace FSCL.Runtime.Scheduling

open Microsoft.FSharp.Quotations
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Linq.RuntimeHelpers
open System.Collections.Generic
//open QuotEval.QuotationEvaluation

open Microsoft.FSharp.Linq

open System
open QuotationUtil
open FSCL.Language
open FSCL.Compiler
open FSCL.Compiler.Util
open Microsoft.FSharp.Reflection
open VarStack
open System.Linq

(* EXPRESSION COUNTER *)
type CountAction =
| Value of Expr<float32>
| Continue

type CountError(msg: string) =
    inherit System.Exception(msg)

type ExpressionCounter() = 
    (*
    static member private ReplaceWorkSizeFunctions(expr: Expr, workItemIdContainerPlaceholder: Quotations.Var) =
        let rec ReplaceInternal(expr: Expr) =
            match expr with 
            | Patterns.Call(o, mi, l) ->
                match expr with           
                | DerivedPatterns.SpecificCall <@ get_work_dim @> (o, tl, l) ->  
                    Expr.Call(Expr.Var(workItemIdContainerPlaceholder), typeof<WorkItemIdContainer>.GetMethod("WorkDim"), [])
                | DerivedPatterns.SpecificCall <@ get_global_size @> (o, tl, l) ->
                    let replIndex = ReplaceInternal(l.[0])
                    Expr.Call(Expr.Var(workItemIdContainerPlaceholder), typeof<WorkItemIdContainer>.GetMethod("GlobalSize"), [ replIndex ])
                | DerivedPatterns.SpecificCall <@ get_local_size @> (o, tl, l) ->
                    let replIndex = ReplaceInternal(l.[0])
                    Expr.Call(Expr.Var(workItemIdContainerPlaceholder), typeof<WorkItemIdContainer>.GetMethod("LocalSize"), [ replIndex ])
                | DerivedPatterns.SpecificCall <@ get_num_groups @> (o, tl, l) ->
                    let replIndex = ReplaceInternal(l.[0])
                    Expr.Call(Expr.Var(workItemIdContainerPlaceholder), typeof<WorkItemIdContainer>.GetMethod("NumGroups"), [ replIndex ])
                | DerivedPatterns.SpecificCall <@ get_global_id @> (o, tl, l) ->
                    let replIndex = ReplaceInternal(l.[0])
                    Expr.Call(Expr.Var(workItemIdContainerPlaceholder), typeof<WorkItemIdContainer>.GetMethod("GlobalID"), [ replIndex ])
                | DerivedPatterns.SpecificCall <@ get_local_id @> (o, tl, l) ->
                    let replIndex = ReplaceInternal(l.[0])
                    Expr.Call(Expr.Var(workItemIdContainerPlaceholder), typeof<WorkItemInfo>.GetMethod("LocalID"), [ replIndex ])
                | _ ->
                    let replList = l |> List.map(fun e -> ReplaceInternal(e))
                    if o.IsSome then
                        Expr.Call(o.Value, mi, replList)
                    else
                        Expr.Call(mi, replList)                        
            | ExprShape.ShapeVar(v) ->
                expr
            | ExprShape.ShapeLambda(v, b) ->
                let replBody = ReplaceInternal(b)
                let newLambda = Expr.Lambda(v, replBody)
                let i = 0
                newLambda
            | ExprShape.ShapeCombination(o, l) ->
                let replList = l |> List.map(fun e -> ReplaceInternal(e))
                ExprShape.RebuildShapeCombination(o, replList)

        ReplaceInternal(expr)
        *)
    static member private ReplaceDynamicConstantDefines(expr: Expr, dynamicDefinesPlaceholders: List<Var>) =
        let rec ReplaceInternal(expr: Expr) =
            match expr with 
            | Patterns.PropertyGet(o, pi, value) ->
                // A property get can be handled only if the property has a reflected definition attribute
                let isStatic =
                    let attr = List.ofSeq (pi.GetCustomAttributes<DynamicConstantDefineAttribute>())
                    attr.Length = 0
                if not isStatic then
                    // Check if var already created
                    let prevVar = dynamicDefinesPlaceholders |> Seq.tryFind(fun p -> p.Name = pi.Name)
                    if prevVar.IsNone then
                        // Create a new var for this
                        let v = Quotations.Var(pi.Name, pi.PropertyType)
                        dynamicDefinesPlaceholders.Add(v)
                        Expr.Var(v)
                    else
                        Expr.Var(prevVar.Value)
                else
                    if o.IsSome then
                        let replList = value |> List.map(fun e -> ReplaceInternal(e))
                        Expr.PropertyGet(o.Value, pi, replList)
                    else
                        let replList = value |> List.map(fun e -> ReplaceInternal(e))
                        Expr.PropertyGet(pi, replList)
            | ExprShape.ShapeVar(v) ->
                expr
            | ExprShape.ShapeLambda(v, b) ->
                Expr.Lambda(v, ReplaceInternal(b))
            | ExprShape.ShapeCombination(o, l) ->
                let replList = l |> List.map(fun e -> ReplaceInternal(e))
                ExprShape.RebuildShapeCombination(o, replList)

        ReplaceInternal(expr)

    static member private PrepareBody(body: Expr,
                                     // workItemIdContainerPlaceholder: Quotations.Var,
                                      dynamicDefinesPlaceholders: List<Var>) =
        let replacedBody = ExpressionCounter.ReplaceDynamicConstantDefines(
                                //ExpressionCounter.ReplaceWorkSizeFunctions(
                                    QuotationUtil.ToCurriedFunction(body), 
                                    //workItemIdContainerPlaceholder,
                                dynamicDefinesPlaceholders)
               
        let prep = AddParametersToCurriedFunction(replacedBody, (dynamicDefinesPlaceholders |> List.ofSeq))
        prep

    // Build an expression that contains the number of interesting items
    static member private Estimate(functionBody: Expr, 
                                   parameters: (ParameterInfo * Var)[],
                                   action: Expr * (ParameterInfo * Var)[] * (Expr -> Expr<float32>) -> CountAction,
                                   stack: VarStack,
                                   //workItemIdContainerPlaceholder: Quotations.Var,
                                   dynamicDefinesPlaceholders: Var list,
                                   considerLoopIncr: bool) =            
        let rec EstimateInternal(expr: Expr, stack: VarStack) =
            // Check if this is an interesting item
            match action(expr, parameters, ExpressionCounter.ContinueCount parameters action stack dynamicDefinesPlaceholders considerLoopIncr) with
            | Value(c) ->
                c, stack
            | _ ->
                match expr with        
                | Patterns.Let (v, value, body) ->
                    let maybeOpRange: (Expr<float32> * VarStack) option = EstimateLoopWithOpRange(expr, stack)
                    if maybeOpRange.IsSome then
                        maybeOpRange.Value
                    else
                        let first, newStack = EstimateInternal(value, stack)
                        let second, newStack = EstimateInternal(body, push newStack (v, value))
                        <@ %first + %second @>, pop newStack

                | Patterns.VarSet (v, e) ->
                    let first, newStack = EstimateInternal(e, stack)
                    first, set v e newStack

                | Patterns.IfThenElse (c, ib, eb) ->
                    let cond, condStack = EstimateInternal(c, stack)
                    let ifb, newStack = EstimateInternal(ib, condStack)
                    let elseb, newStack = EstimateInternal(eb, condStack)
                    // Fixe: before we had half the instrs of if branch and half the instrs of else branch. But on GPU often executed in lockstep
                    let result = <@ %cond + 1.0f * %ifb + 1.0f * %elseb @>, newStack
                    result

                | Patterns.ForIntegerRangeLoop(v, starte, ende, body) ->
                    // Check that startv is an expression of constants and fers to parameters
                    let es, _ = EstimateInternal(starte, stack)
                    let ee, _ = EstimateInternal(ende, stack)
                    let newStack = push stack (v, starte)
                    let subexpr, subStack = EstimateInternal(body, newStack)

                    let unfoldStart = UnfoldExpr(starte, stack)
                    let unfoldEnd = UnfoldExpr(ende, newStack)
                    let incCount = if considerLoopIncr then 1.0f else 0.0f
                    <@
                        if ((%%unfoldStart : int) > (%%unfoldEnd : int)) then
                            %es + ((((float32)(%%unfoldStart : int) - (float32)(%%unfoldEnd : int) + 1.0f)) * (float32)(%subexpr + %ee + incCount))
                        else
                            %es + ((((float32)(%%unfoldEnd : int) - (float32)(%%unfoldStart : int) + 1.0f)) * (float32)(%subexpr + %ee + incCount))
                    @>, pop subStack

                | Patterns.Sequential(e1, e2) ->
                    let ev1, newStack = EstimateInternal(e1, stack)
                    let ev2, newStack = EstimateInternal(e2, newStack)
                    <@ %ev1 + %ev2 @>, newStack

                | Patterns.WhileLoop(guard, body) ->
                    let estimator = EstimateSimpleWhileLoop(guard, body, stack)
                    estimator, stack

                | Patterns.Call(o, mi, arguments) ->
                    // Check if this is a call to a reflected function
                    match mi with
                    | DerivedPatterns.MethodWithReflectedDefinition(b) ->
                        // It's a reflected method
                        match QuotationAnalysis.GetCurriedOrTupledArgs(b) with
                        | Some(paramVars) ->       
                            let parameters = List.zip (mi.GetParameters() |> List.ofArray) paramVars
                            // Count inside arguments
                            let l, newStack = EstimateList(arguments, stack)
                            // We must be sure that variables used for arguments can be unfolded to expressions of parameters, constants an work size functions
                            let unfoldedArguments = arguments |> List.map(fun (it:Expr) -> UnfoldExpr(it, newStack))
                            // Build an evaluator for this function as if it was a kernel
                            let evaluatorExpr, placeholders = ExpressionCounter.Count(b, parameters |> Array.ofList, action, considerLoopIncr)
                            // Get the method info to invoke the evaluator
                            //let c = <@ evaluator(0.0f, 0.0f, new FSCL.Language.WorkItemIdContainer([||], [||],[||], [||], [||])) @>
                            //let evaluationMethod = evaluator.GetType().GetMethod("Invoke")
                            //Expr.Lambda
                            //let res = evaluationMethod.Invoke(evaluator, [| (0.0f, 0.0f, new FSCL.Language.WorkItemIdContainer([||], [||],[||], [||], [||])) |])
                            // We build a tupledArg out of the args array
                            //let newTupleExpr = Expr.NewTuple(arguments @ [ Expr.Var(workItemIdContainerPlaceholder)]) 
                            //let t = newTupleExpr.Type

                            let mutable call = evaluatorExpr
                            for a in unfoldedArguments do
                                call <- Expr.Application(call, a);
                            //call <- Expr.Application(call, Expr.Var(workItemIdContainerPlaceholder))

                            // We create an expression that sums the estimation for arguments to the invocation of the evaluator
                            // The evaluation requires the values for work size functions (global_size_num, groups, etc)
                            // We pass the placeholders to the corresponding functions
                            let fv = call.GetFreeVars() |> Seq.toArray
                            // Now we add this expr to the estimation of the arguments
                            <@ %l + %%call:float32 @>, stack
                        | _ ->      
                            EstimateList(arguments, stack)
                    | _ ->      
                        EstimateList(arguments, stack)
                | ExprShape.ShapeVar(var) ->
                    <@ 0.0f @>, stack
                | ExprShape.ShapeLambda(var, lambda) ->
                    EstimateInternal (lambda, stack)
                | ExprShape.ShapeCombination(o, e) ->        
                    EstimateList(e, stack)
                | _ -> 
                    raise (CountError("Cannot build instruction evaluation lamba because of unrecognized patterns"))

        and EstimateList(l: Expr list, stack: VarStack) =
            if l.IsEmpty then
                <@ 0.0f @>, stack
            else
                let result = ref (EstimateInternal(l.[0], stack))
                for i = 1 to l.Length - 1 do
                    let v, newStack = EstimateInternal(l.[i], snd !result)
                    result := <@ %(fst !result) + %v @>, newStack
                !result
                    
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
                                                let es, esStack = EstimateInternal(starte, stack)
                                                let se, seStack = EstimateInternal(stepe, stack)
                                                let ee, eeStack = EstimateInternal(ende, stack)
                                                let subexpr, subStack = EstimateInternal(body, newStack)

                                                let unfoldStart = UnfoldExpr(starte, stack)
                                                let unfoldStep = UnfoldExpr(stepe, stack)
                                                let unfoldEnd = UnfoldExpr(ende, stack)
                                                let incCount = if considerLoopIncr then 1.0f else 0.0f
                                                let result = <@
                                                                 %es + %se + ((float32)(Math.Ceiling((float)(((float32)(%%unfoldEnd:int) - (float32)(%%unfoldStart:int) + 1.0f) / (float32)(%%unfoldStep:int)))) * (float32)(%subexpr + %ee + incCount))
                                                             @>
                                                Some(result, subStack |> pop |> pop)
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

        and UnfoldExpr(expr:Expr, stack: VarStack) =
            match expr with
            // If getting a static var evaluate it
            | Patterns.PropertyGet(e, pi, args) ->
                match pi with
                | DerivedPatterns.PropertyGetterWithReflectedDefinition(e) ->
                    let freeVars = List.ofSeq(e.GetFreeVars())
                    if freeVars.IsEmpty then
                        let value = LeafExpressionConverter.EvaluateQuotation(e)
                        Expr.Value (value, e.Type)
                    else
                        raise (CountError("Error during variable unfolding: cannot get the value of var [" + pi.Name + "]"))
                | _ ->
                    raise (CountError("Error during variable unfolding: cannot get the value of var [" + pi.Name + "]"))
            // If referring to a var try to replace it with an expression with only references to parameters, work size functions and dynamic defines
            | ExprShape.ShapeVar(v) ->
                let isParameterReference = (Array.tryFind (fun (p:ParameterInfo, pv:Var) -> pv = v) parameters).IsSome || v.Type = typeof<WorkItemInfo>
                //let isWorkSizeFunctionReference = (v = workItemIdContainerPlaceholder)
                let isDynamicDefineReference = (List.tryFind (fun (pv:Var) -> pv = v) dynamicDefinesPlaceholders).IsSome
                if isParameterReference || isDynamicDefineReference then
                   expr
                else
                    let varValue, stackTail = findAndTail v true stack
                    if varValue.IsSome then
                        UnfoldExpr(varValue.Value, stackTail)
                    else
                        raise (CountError("Cannot find variable " + v.Name + " to count expression"))
            | ExprShape.ShapeLambda(v, b) ->
                Expr.Lambda(v, UnfoldExpr(b, stack))
            | ExprShape.ShapeCombination(o, l) ->
                let replList = l |> List.map(fun e -> UnfoldExpr(e, stack))
                ExprShape.RebuildShapeCombination(o, replList)

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
                                        let unfoldUpdate = UnfoldExpr(arguments.[1], stack)
                                        Some(assExpr, unfoldUpdate)
                                    else
                                        raise (new ExpressionCounterError("Cannot estimate the trip count of a while body where the guard variable is updated using an expression that differs from VAR <- VAR OP EXPR"))
                                | _ ->
                                    raise (new ExpressionCounterError("Cannot estimate the trip count of a while body where the guard variable is updated using an expression that differs from VAR <- VAR OP EXPR"))
                            | _ ->
                                raise (new ExpressionCounterError("Cannot estimate the trip count of a while body where the guard variable is updated using an expression that differs from VAR <- VAR OP EXPR"))                                
                        else
                            raise (new ExpressionCounterError("Cannot estimate the trip count of a while body where the same variable is updated more then once"))
                    else
                        findVarUpdate(v, assExpr)       
                | Patterns.ForIntegerRangeLoop(_, starte, ende, body) ->
                    let update = findVarUpdate(v, body)
                    if update.IsSome then
                        raise (new ExpressionCounterError("Cannot estimate the trip count of a while body where the guard variable is updated in an if-then-else or in a nested loop"))
                    else
                        None  
                | Patterns.WhileLoop(_, body) ->
                    let update = findVarUpdate(v, body)
                    if update.IsSome then
                        raise (new ExpressionCounterError("Cannot estimate the trip count of a while body where the guard variable is updated in an if-then-else or in a nested loop"))
                    else
                        None  
                | Patterns.IfThenElse(_, ifb, elseb) ->
                    let update1 = findVarUpdate(v, ifb)
                    let update2 = findVarUpdate(v, elseb)
                    if update1.IsSome || update2.IsSome then
                        raise (new ExpressionCounterError("Cannot estimate the trip count of a while body where the guard variable is updated in an if-then-else or in a nested loop"))
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
                                                                                                                raise (new ExpressionCounterError("Cannot estimate the trip count of a while body where the same variable is updated more then once")))
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
                        let unfoldCond = UnfoldExpr(a.[1], stack)
                        // Search in the body the ONLY update in the form v <- v +-*/>>><<< expr
                        let update = findVarUpdate(guardVar, body)
                        match update with
                        | Some(updateOp, updateExpr) ->
                            // Count interesting things in body
                            let bodyCount, _ = EstimateInternal(body, stack)
                            // Determine the rounding
                            let increment = 
                                match guard with            
                                | DerivedPatterns.SpecificCall <@ (>=) @> (e, t, a) 
                                | DerivedPatterns.SpecificCall <@ (<=) @> (e, t, a) ->
                                    1.0
                                | _ ->
                                    0.0
                            // Build estimator
                            let estimator =
                                match updateOp with
                                | DerivedPatterns.SpecificCall <@ (+) @> (o, t, arguments) ->
                                    <@ ((((%%unfoldCond |> float32) - (%%bindingValue.Value |> float32)) / (%%updateExpr |> float32)) + (increment |> float32)) * %bodyCount @>
                                | DerivedPatterns.SpecificCall <@ (-) @> (o, t, arguments) ->
                                    <@ ((((%%bindingValue.Value |> float32) - (%%unfoldCond |> float32)) / (%%updateExpr |> float32)) + (increment |> float32)) * %bodyCount @>
                                | DerivedPatterns.SpecificCall <@ (*) @> (o, t, arguments) ->
                                    <@ (Math.Floor(Math.Log((%%unfoldCond |> float), (%%updateExpr |> float)) - Math.Log((%%bindingValue.Value |> float), (%%updateExpr |> float)) + increment) |> float32) * %bodyCount @> 
                                | DerivedPatterns.SpecificCall <@ (/) @> (o, t, arguments)-> 
                                    <@ (Math.Floor(Math.Log((%%bindingValue.Value |> float), (%%updateExpr |> float)) - Math.Log((%%unfoldCond |> float), (%%updateExpr |> float)) + increment)  |> float32) * %bodyCount @> 
                                | DerivedPatterns.SpecificCall <@ (>>>) @> (o, t, arguments) ->
                                    <@ (Math.Floor(Math.Log((%%bindingValue.Value |> float), (Math.Pow(2.0, %%updateExpr|> float))) - Math.Log((%%unfoldCond |> float), (Math.Pow(2.0, %%updateExpr|> float))) + increment) |> float32) * %bodyCount @>  
                                | DerivedPatterns.SpecificCall <@ (<<<) @> (o, t, arguments) ->
                                    <@ (Math.Floor(Math.Log((%%unfoldCond |> float), (Math.Pow(2.0, %%updateExpr|> float))) - Math.Log((%%bindingValue.Value |> float), (Math.Pow(2.0, %%updateExpr|> float))) + increment) |> float32) * %bodyCount @> 
                                | _ ->
                                    raise (new ExpressionCounterError("Cannot estimate the trip count of a while body where the guard variable is updated using an expression that differs from VAR <- VAR OP EXPR"))                                
                            estimator
                        | _ ->                            
                            raise (new ExpressionCounterError("Cannot find the variable update of a while loop"))                                
                    else
                        raise (new ExpressionCounterError("Cannot determine the original value of a while loop iteration variable"))                                                                
                | _ ->
                    raise (new ExpressionCounterError("Cannot estimate the trip count of a while body where the guard is not in the form VAR COMP_OP EXPR"))                                
            | _ ->
                raise (new ExpressionCounterError("Cannot estimate the trip count of a while body where the guard is not in the form VAR COMP_OP EXPR"))                                
                    

            (*            
            // Determine the free variables in guard
            let freeVars = ReplaceFunctionBody(functionBody, guard).Value.GetFreeVars() |> List.ofSeq
            // For each free variable check that they can be unfold to paramters/constants, etc
            let unfold = freeVars |> List.map(fun v -> UnfoldExpr(Expr.Var(v), stack))
            // Now extract value assignments to the free vars in the while body
            let varModification = new Dictionary<Var, List<Expr>>()
            let isValidModification = CheckModification(freeVars, body, varModification, true)
            // By now we support one only variables
            if isValidModification && freeVars.Length = 1 then
                // I got all the modifications of free vars, now I build a while loop to dynamically count the iterations
                let tripCount = Quotations.Var("tripCount", typeof<float32>, true)
                let sumMethodInfo = QuotationAnalysis.ExtractMethodFromExpr(<@ 1.0f + 1.0f @>).Value
                let mulMethodInfo = QuotationAnalysis.ExtractMethodFromExpr(<@ 1.0f * 1.0f @>).Value
                // I estimate the body of the loop (should not depend on the loop variable...)
                let bodyEstimation, _ = EstimateInternal(body, stack) 
                let mutable updateExpr = Expr.Sequential(
                                            Expr.VarSet(freeVars.[0], varModification.[freeVars.[0]].Last()),
                                            Expr.VarSet(tripCount, Expr.Call(sumMethodInfo, [ Expr.Var(tripCount); Expr.Value(1.0f) ])))
                for i = varModification.[freeVars.[0]].Count - 2 downto 0 do
                    updateExpr <- Expr.Sequential(
                                    Expr.VarSet(freeVars.[0], varModification.[freeVars.[0]].[i]),
                                    updateExpr)

                let e = Expr.Let(tripCount, Expr.Value(0.0f), 
                             Expr.Let(freeVars.[0], unfold.[0],
                                     Expr.WhileLoop(guard, Expr.Value(()))))
                e
            else
                Expr.Value(0.0f)   
                *) 

        EstimateInternal(functionBody, stack)            
            
    // Removes (0+0+0+0+0+) useless counts in the expression
    static member private CleanInstructionCount (expr: Expr<float32>) =
        let rec cleanInstructionInternal(expr: Expr) =
            let fv = expr.GetFreeVars() |> Array.ofSeq
            if (Seq.isEmpty (expr.GetFreeVars())) && expr.Type = typeof<float32> then
                let value = LeafExpressionConverter.EvaluateQuotation(expr) :?> float32 //expr.EvalUntyped() :?> float32
                <@ value @> :> Expr
            else
                match expr with
                | ExprShape.ShapeVar (v) ->
                    expr
                | ExprShape.ShapeLambda(v, e) ->
                    Expr.Lambda(v, cleanInstructionInternal(e))
                | ExprShape.ShapeCombination(o, l) ->
                    let cleanedList = List.map (fun el -> cleanInstructionInternal(el)) l
                    ExprShape.RebuildShapeCombination(o, cleanedList)
        let result = cleanInstructionInternal(expr)
        <@ (%%result:float32) @>
    
    static member Count(body: Expr,
                        parameters: (ParameterInfo * Var)[],
                        action: Expr * (ParameterInfo * Var)[] * (Expr -> Expr<float32>) -> CountAction,
                        considerLoopIncr: bool) = 
        // Create a lambda to evaluate instruction count
        //let workItemIdContainerPlaceholder = Quotations.Var("workItemIdContainer", typeof<WorkItemIdContainer>)
        let dynamicDefinePlaceholders = new List<Var>()

        let prepBody = ExpressionCounter.PrepareBody(body, dynamicDefinePlaceholders);
        let precExpr, newStack = ExpressionCounter.Estimate(prepBody.Value, parameters, action, EmptyStack, dynamicDefinePlaceholders |> List.ofSeq, considerLoopIncr)
        let cleanCountExpr = ExpressionCounter.CleanInstructionCount(precExpr)
        (ExpressionCounter.CloseCountExpression(prepBody.Value, cleanCountExpr), dynamicDefinePlaceholders)
            
    static member ContinueCount (parameters: (ParameterInfo * Var)[])
                                (action: Expr * (ParameterInfo * Var)[] * (Expr -> Expr<float32>) -> CountAction) 
                                (stack: VarStack) 
                               // (workItemIdContainerPlaceholder: Quotations.Var)
                                (dynamicDefinesPlaceholders: Var list)
                                (considerLoopIncr: bool)
                                (e: Expr) =
        let precExpr, newStack = ExpressionCounter.Estimate(e, parameters, action, stack, dynamicDefinesPlaceholders, considerLoopIncr)
        ExpressionCounter.CleanInstructionCount(precExpr)

    static member private CloseCountExpression(body: Expr, precExpr: Expr) = 
        // We build an expression with args preparation preamble where
        // each parameter is bound to the proper arg
        let preparedExpr = ReplaceFunctionBody(body, precExpr).Value
        // Return
        preparedExpr



        

      