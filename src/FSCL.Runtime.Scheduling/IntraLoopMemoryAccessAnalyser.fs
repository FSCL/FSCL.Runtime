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
open System.Runtime.InteropServices
open System.Linq

type LoopTree =
| Loop of int * LoopTree list

type IntraLoopMemoryAccessAnalyser() = 
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
                                      dynamicDefinesPlaceholders: List<Var>) =
        let replacedBody = IntraLoopMemoryAccessAnalyser.ReplaceDynamicConstantDefines(
                                    QuotationUtil.ToCurriedFunction(body), 
                                dynamicDefinesPlaceholders)
               
        let prep = AddParametersToCurriedFunction(replacedBody, (dynamicDefinesPlaceholders |> List.ofSeq))
        prep

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
                    IntraLoopMemoryAccessAnalyser.CheckModification(vs, value, varModification, true)
            else
                IntraLoopMemoryAccessAnalyser.CheckModification(vs, value, varModification, true)
        | Patterns.WhileLoop(guard, body) ->
            // Not a valid modification context
            let first = IntraLoopMemoryAccessAnalyser.CheckModification(vs, guard, varModification, false)
            first && IntraLoopMemoryAccessAnalyser.CheckModification(vs, body, varModification, false)
        | Patterns.IfThenElse(guard, ifb, elseb) ->
            // Not a valid modification context
            let first = IntraLoopMemoryAccessAnalyser.CheckModification(vs, guard, varModification, false)
            let snd = first && IntraLoopMemoryAccessAnalyser.CheckModification(vs, ifb, varModification, false)
            snd && IntraLoopMemoryAccessAnalyser.CheckModification(vs, elseb, varModification, false)
        | Patterns.ForIntegerRangeLoop(v, st, en, b) ->
            // Not a valid modification context
            let first = IntraLoopMemoryAccessAnalyser.CheckModification(vs, st, varModification, false)
            let snd = first && IntraLoopMemoryAccessAnalyser.CheckModification(vs, en, varModification, false)
            snd && IntraLoopMemoryAccessAnalyser.CheckModification(vs, b, varModification, false)
        | ExprShape.ShapeVar(v) ->
            true
        | ExprShape.ShapeLambda(v, b) ->                
            IntraLoopMemoryAccessAnalyser.CheckModification(vs, b, varModification, true)
        | ExprShape.ShapeCombination(o, bl) ->               
            if bl.Length > 0 then
                bl |> List.map (fun it -> IntraLoopMemoryAccessAnalyser.CheckModification(vs, it, varModification, true)) |> List.reduce(fun a b -> a && b)
            else
                true

    // Build an expression that contains the number of interesting items
    static member private Estimate(functionBody: Expr, 
                                   parameters: (ParameterInfo * Var)[],
                                   stack: VarStack,
                                   dynamicDefinesPlaceholders: Var list) = 
        // Access dataset contains for each array accessed the list of offsets inter-thread (one for each loop found)   
        let globalAccessDataset = new Dictionary<Var, List<Expr<int> * Expr>>()  
                        
        let rec UnfoldExpr(expr:Expr, stack: VarStack) =
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
 
        let rec EstimateExpr(expr: Expr, stack: VarStack, tripCount: Expr<int>, loopVars: (Var * Expr * Expr * Expr<int>) list) =
            match expr with        
            // Check if access to array            
            | Patterns.Call(e, i, l) ->   
                // Estimate list
                EstimateList(l, stack, tripCount, loopVars) |> ignore

                // Classic access                                        
                if i.DeclaringType.Name = "IntrinsicFunctions" && (i.Name = "GetArray" || i.Name = "GetArray2D" || i.Name = "GetArray3D") then
                    let arrayVar = 
                        match l.[0] with 
                        | Patterns.Var(v) -> 
                            v
                        | _ -> 
                            raise (new KernelSchedulingException("Cannot analyse inter-thread stride when array accessed is not a var ref"))
                    let accessExprs = List.tail l

                    match loopVars.Last() with
                    | loopVar, bindingValue, followingValue, tCount ->
                        // Normalise multi-dim access to 1D access  
                        let normalisedExpr = QuotationUtil.NormalizeArrayAccess(l.[0], accessExprs) :> Expr

                        // Compute delta between the access with last loop var at binding value and last loop var at binding value + step
                        // Compute start value for iteration var
                        let bindingExpr = normalisedExpr.Substitute(fun v -> 
                                                                    if v = loopVar then
                                                                        Some(bindingValue)
                                                                    else
                                                                        None)
                        // Compute the successive value for iteration var
                        let stepExpr = normalisedExpr.Substitute(fun v -> 
                                                                    if v = loopVar then
                                                                        Some(followingValue)
                                                                    else
                                                                        None)
                        let mutable deltaExpr = <@@ Math.Abs((%%bindingExpr:int) - (%%stepExpr:int)) * Marshal.SizeOf(arrayVar.Type.GetElementType()) @@>  

                        // Unfold expr
                        deltaExpr <- UnfoldExpr(deltaExpr, stack) 

                        // Add this expression to the access dataset
                        if not (globalAccessDataset.ContainsKey(arrayVar)) then
                            globalAccessDataset.Add(arrayVar, new List<Expr<int> * Expr>())
                        globalAccessDataset.[arrayVar].Add((tripCount, <@@ %%deltaExpr |> float32 @@>))
                stack

            // Loop: we check inside it
            | Patterns.ForIntegerRangeLoop(v, starte, ende, body) ->
                // Determine loop trip count
                let unfoldStart = UnfoldExpr(starte, stack)
                let unfoldEnd = UnfoldExpr(ende, stack)
                // Check accesses in starte and ende
                let newStack = EstimateExpr(starte, stack, tripCount, loopVars)
                let newStack = EstimateExpr(ende, stack, tripCount, loopVars)
                // Create trip count expression
                let tripCountExpr = 
                    <@
                        if ((%%starte : int) > (%%ende : int)) then
                            ((((%%unfoldStart:int) - (%%unfoldEnd : int) + 1)))
                        else
                            ((((%%unfoldEnd:int) - (%%unfoldStart : int) + 1)))
                    @>
                // Compute expr calculating the second value for loop var
                let followingValue =
                    <@@
                        if ((%%starte : int) > (%%ende : int)) then
                            ((((%%unfoldStart:int) + 1)))
                        else
                            ((((%%unfoldEnd:int) - 1)))
                    @@>
                // Compute total trip count
                let mutable totalTripCount = tripCountExpr
                for loopVar, loopInit, loopStep, loopTripCount in loopVars do
                    totalTripCount <- <@ (%totalTripCount) * %loopTripCount @>   
                // Now check inside loop             
                EstimateExpr(body, newStack, totalTripCount, loopVars @ [ (v, unfoldStart, followingValue, tripCountExpr) ])

            | Patterns.Let (v, value, body) ->
                let maybeOpRange: VarStack option = EstimateLoopWithOpRange(expr, stack, tripCount, loopVars)
                if maybeOpRange.IsSome then
                    maybeOpRange.Value
                else
                    let newStack = EstimateExpr(value, stack, tripCount, loopVars)
                    let newStack = EstimateExpr(body, push newStack (v, value, v.IsMutable), tripCount, loopVars)
                    pop newStack

            | Patterns.VarSet (v, e) ->
                let newStack = EstimateExpr(e, stack, tripCount, loopVars)
                set v e newStack

            | Patterns.IfThenElse (c, ib, eb) ->
                let condStack = EstimateExpr(c, stack, tripCount, loopVars)
                let newStack = EstimateExpr(ib, condStack, tripCount, loopVars)
                let newStack = EstimateExpr(eb, condStack, tripCount, loopVars)
                newStack
                
            | Patterns.Sequential(e1, e2) ->
                let newStack = EstimateExpr(e1, stack, tripCount, loopVars)
                let newStack = EstimateExpr(e2, newStack, tripCount, loopVars)
                newStack

            | Patterns.WhileLoop(guard, body) ->
                let newStack = EstimateSimpleWhileLoop(guard, body, stack, tripCount, loopVars)
                newStack
            | ExprShape.ShapeVar(var) ->
                stack
            | ExprShape.ShapeLambda(var, lambda) ->
                EstimateExpr (lambda, stack, tripCount, loopVars)
            | ExprShape.ShapeCombination(o, e) ->        
                EstimateList(e, stack, tripCount, loopVars)
            | _ -> 
                raise (CountError("Cannot build instruction evaluation lamba because of unrecognized patterns"))

        and EstimateList(l: Expr list, stack: VarStack, tripCount: Expr<int>, loopVars: (Var * Expr * Expr * Expr<int>) list) =
            if l.IsEmpty then
                stack
            else
                let result = ref (EstimateExpr(l.[0], stack, tripCount, loopVars))
                for i = 1 to l.Length - 1 do
                    result := EstimateExpr(l.[i], !result, tripCount, loopVars)
                !result
                    
        and EstimateLoopWithOpRange(expr: Expr, stack: VarStack, tripCount: Expr<int>, loopVars: (Var * Expr * Expr * Expr<int>) list) =
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
                                                let esStack = EstimateExpr(starte, stack, tripCount, loopVars)
                                                let seStack = EstimateExpr(stepe, stack, tripCount, loopVars)
                                                let eeStack = EstimateExpr(ende, stack, tripCount, loopVars)

                                                let unfoldStart = UnfoldExpr(starte, stack)
                                                let unfoldStep = UnfoldExpr(stepe, stack)
                                                let unfoldEnd = UnfoldExpr(ende, stack)
                                                let tripCountExpr = <@
                                                                        ((int)(Math.Ceiling((float)((%%unfoldEnd:int) - (%%unfoldStart:int) + 1) / (float)(%%unfoldStep:int)))) |> int
                                                                    @>
                                                                    
                                                // Compute expr calculating the second value for loop var
                                                let followingValue =
                                                    <@@
                                                        (%%unfoldStart:int) + (%%stepe:int)
                                                    @@>
                                                // Compute total trip count
                                                let mutable totalTripCount = tripCountExpr
                                                for loopVar, loopInit, secondValue, loopTripCount in loopVars do
                                                    totalTripCount <- <@ (%totalTripCount) * %loopTripCount @>   
                                                // Now check inside loop             
                                                Some(EstimateExpr(body, eeStack, totalTripCount, loopVars @ [ (v, unfoldStart, followingValue, tripCountExpr) ]))
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
                                    tripCount: Expr<int>, 
                                    loopVars: (Var * Expr * Expr * Expr<int>) list) =
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
                                        
            // Check accesses in guard
            let newStack = EstimateExpr(guard, stack, tripCount, loopVars)

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
                            // Compute total trip count
                            let mutable totalTripCount = tripCountExpr
                            for loopVar, loopInit, secondValue, loopTripCount in loopVars do
                                totalTripCount <- <@ (%totalTripCount) * %loopTripCount @>   
                            // Now check inside loop             
                            EstimateExpr(body, newStack, totalTripCount, loopVars @ [ (guardVar, bindingValue.Value,  tripCountExpr) ])
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
                let bodyEstimation, _ = EstimateExpr(body, stack) 
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
        let s = EstimateExpr(functionBody, stack, <@ 1 @>, [])
        globalAccessDataset
            
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
    
    static member EstimateMemoryAccessStride(body: Expr,
                                             parameters: (ParameterInfo * Var)[]) = 
        let dynamicDefinePlaceholders = new List<Var>()

        let prepBody = IntraLoopMemoryAccessAnalyser.PrepareBody(body, dynamicDefinePlaceholders);
        let memAccesses = IntraLoopMemoryAccessAnalyser.Estimate(prepBody.Value, parameters, EmptyStack, dynamicDefinePlaceholders |> List.ofSeq)
        let memAccessesEvaluators = new Dictionary<Var, (Expr * Expr) list>()
        for item in memAccesses do
            let evals = item.Value |> Seq.map(fun (a, b) -> (IntraLoopMemoryAccessAnalyser.CloseExpression(prepBody.Value, a), IntraLoopMemoryAccessAnalyser.CloseExpression(prepBody.Value, b)))
            memAccessesEvaluators.Add(item.Key,
                                      evals |> Seq.toList)
        memAccessesEvaluators, dynamicDefinePlaceholders
            
    static member private CloseExpression(body: Expr, precExpr: Expr) = 
        // We build an expression with args preparation preamble where
        // each parameter is bound to the proper arg
        let preparedExpr = ReplaceFunctionBody(body, precExpr).Value
        // Return
        preparedExpr



        

      