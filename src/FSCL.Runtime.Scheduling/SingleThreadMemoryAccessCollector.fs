namespace FSCL.Runtime.Scheduling

open Microsoft.FSharp.Quotations
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Linq.RuntimeHelpers
open System.Collections.Generic
open Microsoft.FSharp.Linq
open FSCL.Runtime
open System
open QuotationUtil
open FSCL.Language
open FSCL.Compiler
open FSCL.Compiler.Util
open Microsoft.FSharp.Reflection
open VarStack
open System.Linq

type MemoryAccessNode =
| LoopNode of Expr * Var * Expr * Expr * Expr * MemoryAccessNode list
| MemAccess of Var * Expr

type SingleThreadMemoryAccessCollector() = 
    static member private ZeroThreadID(e: Expr) =
        QuotationUtil.ReplaceExpr(e, fun it -> 
                                        match it with
                                        | Patterns.Call(o, mi, a) ->
                                            if (o.IsSome && typeof<WorkItemInfo>.IsAssignableFrom(o.Value.Type) &&
                                                (mi.Name = "GlobalID" || mi.Name = "LocalID" || mi.Name = "GroupID")) then
                                                Some(Expr.Value(0))
                                            else
                                                None
                                        | _ ->
                                            None)

    static member private CheckModification(vs: Var list, expr: Expr, varModification: Dictionary<Var, List<Expr>>, validModificationContext: bool) =
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
                    SingleThreadMemoryAccessCollector.CheckModification(vs, value, varModification, true)
            else
                SingleThreadMemoryAccessCollector.CheckModification(vs, value, varModification, true)
        | Patterns.WhileLoop(guard, body) ->
            // Not a valid modification context
            let first = SingleThreadMemoryAccessCollector.CheckModification(vs, guard, varModification, false)
            first && SingleThreadMemoryAccessCollector.CheckModification(vs, body, varModification, false)
        | Patterns.IfThenElse(guard, ifb, elseb) ->
            // Not a valid modification context
            let first = SingleThreadMemoryAccessCollector.CheckModification(vs, guard, varModification, false)
            let snd = first && SingleThreadMemoryAccessCollector.CheckModification(vs, ifb, varModification, false)
            snd && SingleThreadMemoryAccessCollector.CheckModification(vs, elseb, varModification, false)
        | Patterns.ForIntegerRangeLoop(v, st, en, b) ->
            // Not a valid modification context
            let first = SingleThreadMemoryAccessCollector.CheckModification(vs, st, varModification, false)
            let snd = first && SingleThreadMemoryAccessCollector.CheckModification(vs, en, varModification, false)
            snd && SingleThreadMemoryAccessCollector.CheckModification(vs, b, varModification, false)
        | ExprShape.ShapeVar(v) ->
            true
        | ExprShape.ShapeLambda(v, b) ->                
            SingleThreadMemoryAccessCollector.CheckModification(vs, b, varModification, true)
        | ExprShape.ShapeCombination(o, bl) ->               
            if bl.Length > 0 then
                bl |> List.map (fun it -> SingleThreadMemoryAccessCollector.CheckModification(vs, it, varModification, true)) |> List.reduce(fun a b -> a && b)
            else
                true
                
    // Build an expression that contains the number of interesting items
    static member private Estimate(functionBody: Expr, 
                                   parameters: (ParameterInfo * Var)[],
                                   stack: VarStack,
                                   dynamicDefinesPlaceholders: Var list) = 
        let isParameterReference v = 
            (Array.tryFind (fun (p:ParameterInfo, pv:Var) -> pv = v) parameters).IsSome || v.Type = typeof<WorkItemInfo>
        let isDynamicDefineReference v = 
            (List.tryFind (fun (pv:Var) -> pv = v) dynamicDefinesPlaceholders).IsSome
        let isLoopVarReference loopVars (v:Var) =
            (loopVars |> List.tryFind(fun v1 -> v1 = v)).IsSome

        // Instantiate the AST as if it was code specific for thread 0
        let zeroThreadBody = SingleThreadMemoryAccessCollector.ZeroThreadID(functionBody)
                                    
        let rec EstimateExpr(expr: Expr, stack: VarStack, loopVars: Var list) =
            match expr with        
            // Check if access to array            
            | Patterns.Call(e, i, l) ->   
                // Estimate list
                let accessed, newStack = EstimateList(l, stack, loopVars)

                // Classic access                                        
                if i.DeclaringType.Name = "IntrinsicFunctions" && (i.Name = "GetArray" || i.Name = "GetArray2D" || i.Name = "GetArray3D") then
                    let arrayVar = 
                        match l.[0] with 
                        | Patterns.Var(v) -> 
                            v
                        | _ -> 
                            raise (new KernelSchedulingException("Cannot analyse inter-thread stride when array accessed is not a var ref"))
                    let accessExprs = List.tail l

                    // Normalise multi-dim access to 1D access  
                    let mutable normalisedExpr = QuotationUtil.NormalizeArrayAccess(l.[0], accessExprs) :> Expr
                    let a = isLoopVarReference loopVars
                    // Unfold expr
                    normalisedExpr <- KernelUtil.UnfoldExpressionPreservingLoopVars(normalisedExpr, newStack, isParameterReference, isDynamicDefineReference, a)

                    accessed @ [ MemAccess(arrayVar, normalisedExpr) ], newStack
                else
                    accessed, newStack

            // Loop: we check inside it
            | Patterns.ForIntegerRangeLoop(v, starte, ende, body) ->
                // Determine loop trip count
                let unfoldStart = KernelUtil.UnfoldExpressionPreservingLoopVars(starte, stack, isParameterReference, isDynamicDefineReference, isLoopVarReference loopVars)
                let unfoldEnd = KernelUtil.UnfoldExpressionPreservingLoopVars(ende, stack, isParameterReference, isDynamicDefineReference, isLoopVarReference loopVars)
                // Check accesses in starte and ende
                let startEAccess, newStack = EstimateExpr(starte, stack, loopVars)
                let endEAccess, newStack = EstimateExpr(ende, stack, loopVars)
                // Create trip count expression
                let tripCountExpr = <@ Math.Abs((%%unfoldStart:int) - (%%unfoldEnd : int)) + 1 @>  
                // Now check inside loop             
                let bodyAccess, newStack = EstimateExpr(body, push newStack (v, starte), loopVars @ [ v ])                  
                [ LoopNode(tripCountExpr, 
                            v, 
                            unfoldStart, 
                            <@ (%%unfoldStart:int) + 1 @>,                            
                            <@ (%%unfoldEnd : int) @>, 
                            startEAccess @ endEAccess @ bodyAccess) ], pop newStack  

            | Patterns.Let (v, value, body) ->
                let maybeOpRange: (MemoryAccessNode list * VarStack) option = EstimateLoopWithOpRange(expr, stack, loopVars)
                if maybeOpRange.IsSome then
                    maybeOpRange.Value
                else
                    let valueAccess, newStack = EstimateExpr(value, stack, loopVars)
                    let bodyAccess, newStack = EstimateExpr(body, push newStack (v, value), loopVars)
                    valueAccess @ bodyAccess, pop newStack

            | Patterns.VarSet (v, e) ->
                let valueAccess, newStack = EstimateExpr(e, stack, loopVars)
                valueAccess, set v e newStack

            | Patterns.IfThenElse (c, ib, eb) ->
                let condAccess, condStack = EstimateExpr(c, stack, loopVars)
                let ifAccess, newStack = EstimateExpr(ib, condStack, loopVars)
                let elseAccess, newStack = EstimateExpr(eb, condStack, loopVars)
                condAccess @ ifAccess @ elseAccess, newStack
                
            | Patterns.WhileLoop(guard, body) ->
                let newStack = EstimateSimpleWhileLoop(guard, body, stack, loopVars)
                newStack
            | ExprShape.ShapeVar(var) ->
                [], stack
            | ExprShape.ShapeLambda(var, lambda) ->
                EstimateExpr (lambda, stack, loopVars)
            | ExprShape.ShapeCombination(o, e) ->        
                EstimateList(e, stack, loopVars)
            | _ -> 
                raise (CountError("Cannot build instruction evaluation lamba because of unrecognized patterns"))

        and EstimateList(l: Expr list, stack: VarStack, loopVars: Var list) =
            if l.IsEmpty then
                [], stack
            else
                let mutable result = EstimateExpr(l.[0], stack, loopVars)
                for i = 1 to l.Length - 1 do
                    let itRes, itStack = EstimateExpr(l.[i], result |> snd, loopVars)
                    result <- (result |> fst) @ itRes, itStack
                result
                    
        and EstimateLoopWithOpRange(expr: Expr, stack: VarStack, loopVars: Var list) =
            match expr with
            | Patterns.Let (inputSequence, value, body) ->
                match value with 
                | Patterns.Call(ob, mi, a) ->
                    if mi.Name = "op_RangeStep" then
                        // The args is the beginning, the step and the end of the iteration
                        let starte, stepe, ende = a.[0], a.[1], a.[2]
                        match body with
                        | Patterns.Let(enumerator, value, body) ->
                            match body with
                            | Patterns.TryFinally (trye, fine) ->
                                match trye with
                                    | Patterns.WhileLoop(cond, body) ->
                                        match body with
                                        | Patterns.Let(v, value, body) ->
                                            match value with
                                            | Patterns.PropertyGet(e, pi, a) ->
                                                // Ok, that's an input sequence!
                                                let startEAccess, esStack = EstimateExpr(starte, stack, loopVars)
                                                let stepEAccess, seStack = EstimateExpr(stepe, stack, loopVars)
                                                let endEAccess, eeStack = EstimateExpr(ende, stack, loopVars)

                                                let unfoldStart = KernelUtil.UnfoldExpressionPreservingLoopVars(starte, stack, isParameterReference, isDynamicDefineReference, isLoopVarReference loopVars)
                                                let unfoldStep = KernelUtil.UnfoldExpressionPreservingLoopVars(stepe, stack, isParameterReference, isDynamicDefineReference, isLoopVarReference loopVars)
                                                let unfoldEnd = KernelUtil.UnfoldExpressionPreservingLoopVars(ende, stack, isParameterReference, isDynamicDefineReference, isLoopVarReference loopVars)
                                                let tripCountExpr = <@
                                                                        ((int)(Math.Ceiling((float)((%%unfoldEnd:int) - (%%unfoldStart:int) + 1) / (float)(%%unfoldStep:int)))) |> int
                                                                    @>
                                                // Now check inside loop             
                                                let bodyAccess, bodyStack = EstimateExpr(body, push stack (v, unfoldStart), loopVars @ [ v ])
                                                Some([ LoopNode(
                                                        tripCountExpr, 
                                                        v, 
                                                        unfoldStart,
                                                        <@ (%%unfoldStart:int) + (%%unfoldStep:int) @>,
                                                        <@ Math.Min((%%unfoldEnd : int), 
                                                                     (Math.Abs((%%unfoldEnd : int) - (%%unfoldStart:int)) / (%%unfoldStep:int)) * (%%unfoldStep:int)) @>, 
                                                        startEAccess @ stepEAccess @ endEAccess @ bodyAccess) ], pop bodyStack)
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
                
        and EstimateSimpleWhileLoop(guard:Expr, 
                                    body:Expr, 
                                    stack: VarStack, 
                                    loopVars: Var list) =
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
                                        let unfoldUpdate = KernelUtil.UnfoldExpressionPreservingLoopVars(arguments.[1], stack, isParameterReference, isDynamicDefineReference, isLoopVarReference loopVars)
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
                                        
            // Check accesses in guard
            let guardAccess, newStack = EstimateExpr(guard, stack, loopVars)

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
                        let unfoldCond = KernelUtil.UnfoldExpressionPreservingLoopVars(a.[1], stack, isParameterReference, isDynamicDefineReference, isLoopVarReference loopVars)
                        // Search in the body the ONLY update in the form v <- v +-*/>>><<< expr
                        let update = findVarUpdate(guardVar, body)
                        match update with
                        | Some(updateOp, updateExpr) ->
                            // Determine the rounding
                            let increment = 
                                match guard with            
                                | DerivedPatterns.SpecificCall <@ (>=) @> (e, t, a) 
                                | DerivedPatterns.SpecificCall <@ (<=) @> (e, t, a) ->
                                    1.0
                                | _ ->
                                    0.0
                            // Compute while trip count
                            let tripCountExpr =
                                match updateOp with
                                | DerivedPatterns.SpecificCall <@ (+) @> (o, t, arguments) ->
                                    <@ ((((%%unfoldCond |> float32) - (%%bindingValue.Value |> float32)) / (%%updateExpr |> float32)) + (increment |> float32)) |> int @>
                                | DerivedPatterns.SpecificCall <@ (-) @> (o, t, arguments) ->
                                    <@ ((((%%bindingValue.Value |> float32) - (%%unfoldCond |> float32)) / (%%updateExpr |> float32)) + (increment |> float32)) |> int @>
                                | DerivedPatterns.SpecificCall <@ (*) @> (o, t, arguments) ->
                                    <@ (Math.Floor(Math.Log((%%unfoldCond |> float), (%%updateExpr |> float)) - Math.Log((%%bindingValue.Value |> float), (%%updateExpr |> float)) + increment)) |> int @> 
                                | DerivedPatterns.SpecificCall <@ (/) @> (o, t, arguments)-> 
                                    <@ (Math.Floor(Math.Log((%%bindingValue.Value |> float), (%%updateExpr |> float)) - Math.Log((%%unfoldCond |> float), (%%updateExpr |> float)) + increment)) |> int @> 
                                | DerivedPatterns.SpecificCall <@ (>>>) @> (o, t, arguments) ->
                                    <@ (Math.Floor(Math.Log((%%bindingValue.Value |> float), (Math.Pow(2.0, %%updateExpr|> float))) - Math.Log((%%unfoldCond |> float), (Math.Pow(2.0, %%updateExpr|> float))) + increment)) |> int @>  
                                | DerivedPatterns.SpecificCall <@ (<<<) @> (o, t, arguments) ->
                                    <@ (Math.Floor(Math.Log((%%unfoldCond |> float), (Math.Pow(2.0, %%updateExpr|> float))) - Math.Log((%%bindingValue.Value |> float), (Math.Pow(2.0, %%updateExpr|> float))) + increment)) |> int @> 
                                | _ ->
                                    raise (new ExpressionCounterError("Cannot estimate the trip count of a while body where the guard variable is updated using an expression that differs from VAR <- VAR OP EXPR"))                                
                            // Now check inside loop             
                            let bodyAccess, newStack = EstimateExpr(body, newStack, loopVars @ [ guardVar ])
                                                        
                            [ LoopNode(tripCountExpr, 
                                       guardVar, 
                                       bindingValue.Value,
                                       updateOp,
                                       unfoldCond,
                                       guardAccess @ bodyAccess) ], newStack
                        | _ ->                            
                            raise (new ExpressionCounterError("Cannot find the variable update of a while loop"))                                
                    else
                        raise (new ExpressionCounterError("Cannot determine the original value of a while loop iteration variable"))                                                                
                | _ ->
                    raise (new ExpressionCounterError("Cannot estimate the trip count of a while body where the guard is not in the form VAR COMP_OP EXPR"))                                
            | _ ->
                raise (new ExpressionCounterError("Cannot estimate the trip count of a while body where the guard is not in the form VAR COMP_OP EXPR"))                                
                
        let data, s = EstimateExpr(zeroThreadBody, stack, [])
        data
           
    static member EstimateMemoryAccessStride(body: Expr,
                                             parameters: (ParameterInfo * Var)[]) = 
        let dynamicDefinePlaceholders = new List<Var>()

        let prepBody = KernelUtil.PrepareBodyForAnalysis(body, dynamicDefinePlaceholders);
        let memAccesses = SingleThreadMemoryAccessCollector.Estimate(prepBody.Value, parameters, EmptyStack, dynamicDefinePlaceholders |> List.ofSeq)
        
        let rec closeAll(l: MemoryAccessNode list) =
            l |> List.map(fun n ->
                            match n with 
                            | MemAccess(v, addr) ->
                                MemAccess(v, KernelUtil.CloseExpression(prepBody.Value, addr).Value)
                            | LoopNode(tripCount, itVar, startExpr, stepExpr, endExpr, child) ->
                                LoopNode(
                                    KernelUtil.CloseExpression(prepBody.Value, tripCount).Value,
                                    itVar,
                                    KernelUtil.CloseExpression(prepBody.Value, startExpr).Value,
                                    KernelUtil.CloseExpression(prepBody.Value, stepExpr).Value,
                                    KernelUtil.CloseExpression(prepBody.Value, endExpr).Value,
                                    closeAll(child)))
        let closed = closeAll(memAccesses)
        // Wrap up in a fake 1-iter loop
        LoopNode(KernelUtil.CloseExpression(prepBody.Value, <@ 1 @>).Value,
                 Quotations.Var("unused", typeof<int>),
                 KernelUtil.CloseExpression(prepBody.Value, <@ 0.0f @>).Value,
                 KernelUtil.CloseExpression(prepBody.Value, <@ 1.0f @>).Value,
                 KernelUtil.CloseExpression(prepBody.Value, <@ 0.0f @>).Value,
                 closed), dynamicDefinePlaceholders