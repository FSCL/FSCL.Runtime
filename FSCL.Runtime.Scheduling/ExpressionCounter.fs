namespace FSCL.Runtime.Scheduling

open Microsoft.FSharp.Quotations
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Linq.RuntimeHelpers
open System.Collections.Generic
//open QuotEval.QuotationEvaluation
open System
open QuotationUtil
open FSCL.Language
open FSCL.Compiler
open FSCL.Compiler.Util
open Microsoft.FSharp.Reflection

(* EXPRESSION COUNTER *)
type CountAction =
| Value of Expr<float32>
| Continue

type VarStack = Stack<Dictionary<Var, Expr>>

type CountError(msg: string) =
    inherit System.Exception(msg)

type ExpressionCounter() = 
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
                    Expr.Call(Expr.Var(workItemIdContainerPlaceholder), typeof<WorkItemIdContainer>.GetMethod("LocalID"), [ replIndex ])
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
                                      workItemIdContainerPlaceholder: Quotations.Var,
                                      dynamicDefinesPlaceholders: List<Var>) =
        let replacedBody = ExpressionCounter.ReplaceDynamicConstantDefines(
                                ExpressionCounter.ReplaceWorkSizeFunctions(
                                    QuotationUtil.ToCurriedFunction(body), 
                                    workItemIdContainerPlaceholder),
                                dynamicDefinesPlaceholders)
               
        let prep = AddParametersToCurriedFunction(replacedBody, [workItemIdContainerPlaceholder] @ (dynamicDefinesPlaceholders |> List.ofSeq))
        prep

    // Stack function helpers
    static member private GetStackVar(var:Var, stack: VarStack) =
        let expr = ref None
        let mutable enumerator = stack.GetEnumerator()
        while expr.Value.IsNone && enumerator.MoveNext() do
            if enumerator.Current.ContainsKey(var) then
                expr := Some(enumerator.Current.[var])
        expr.Value.Value

    static member private GetStackVarAndTail(var:Var, stack: VarStack) =
        let found = ref false
        let expr = ref None
        let currentStack = new VarStack(stack)
        while found.Value = false && currentStack.Count > 0 do
            if currentStack.Peek().ContainsKey(var) then
                found := true
                expr := Some(currentStack.Peek().[var])
            else
                currentStack.Pop() |> ignore
        expr.Value.Value, currentStack

    static member private SetStackVar(var:Var, value:Expr, stack: VarStack) =
        let found = ref false
        let mutable enumerator = stack.GetEnumerator()
        while found.Value = false && enumerator.MoveNext() do
            if enumerator.Current.ContainsKey(var) then
                enumerator.Current.[var] <- value
                found := true

    static member private AddStackVar(var:Var, value:Expr, stack: VarStack) =
        stack.Peek().Add(var, value)

    static member private PushStack(stack: VarStack) =
        stack.Push(new Dictionary<Var, Expr>())
            
    static member private PopStack(stack: VarStack) =
        stack.Pop() |> ignore

    // Build an expression that contains the number of interesting items
    static member private Estimate(functionBody: Expr, 
                                   parameters: (ParameterInfo * Var)[],
                                   action: Expr * (ParameterInfo * Var)[] * (Expr -> Expr<float32>) -> CountAction,
                                   stack: VarStack,
                                   workItemIdContainerPlaceholder: Quotations.Var,
                                   dynamicDefinesPlaceholders: Var list,
                                   considerLoopIncr: bool) =            
        let rec EstimateInternal(expr: Expr) =
            // Check if this is an interesting item
            match action(expr, parameters, ExpressionCounter.ContinueCount parameters action stack workItemIdContainerPlaceholder dynamicDefinesPlaceholders considerLoopIncr) with
            | Value(c) ->
                c
            | _ ->
                match expr with        
                | Patterns.Let (v, value, body) ->
                    let maybeOpRange: Expr<float32> option = CheckOpRange(expr)
                    if maybeOpRange.IsSome then
                        maybeOpRange.Value
                    else
                        let first = EstimateInternal(value)
                        ExpressionCounter.PushStack(stack)
                        ExpressionCounter.AddStackVar(v, value, stack)
                        let second = EstimateInternal(body)
                        ExpressionCounter.PopStack(stack)
                        <@ %first + %second @>

                | Patterns.VarSet (v, e) ->
                    let first = EstimateInternal(e)
                    ExpressionCounter.SetStackVar(v, e, stack)
                    first

                | Patterns.IfThenElse (c, ib, eb) ->
                    let cond = EstimateInternal(c)
                    let ifb = EstimateInternal(ib)
                    let elseb = EstimateInternal(eb)
                    // Fixe: before we had half the instrs of if branch and half the instrs of else branch. But on GPU often executed in lockstep
                    let result = <@ %cond + 0.5f * %ifb + 0.5f * %elseb @>
                    result

                | Patterns.ForIntegerRangeLoop(v, starte, ende, body) ->
                    // Check that startv is an expression of constants and fers to parameters
                    let es = EstimateInternal(starte)
                    let ee = EstimateInternal(ende)
                    let subexpr = EstimateInternal(body)

                    let unfoldStart = UnfoldExpr(starte, stack)
                    let unfoldEnd = UnfoldExpr(ende, stack)
                    let incCount = if considerLoopIncr then 1.0f else 0.0f
                    <@
                        if ((%%starte : int) > (%%ende : int)) then
                            %es + ((((float32)(%%unfoldStart : int) - (float32)(%%unfoldEnd : int) + 1.0f)) * (float32)(%subexpr + %ee + incCount))
                        else
                            %es + ((((float32)(%%unfoldEnd : int) - (float32)(%%unfoldStart : int) + 1.0f)) * (float32)(%subexpr + %ee + incCount))
                    @>

                | Patterns.Sequential(e1, e2) ->
                    let ev1 = EstimateInternal(e1)
                    let ev2 = EstimateInternal(e2)
                    <@ %ev1 + %ev2 @>

                | Patterns.WhileLoop(guard, body) ->
                    let estimator = BuildWhileLoopEstimator(guard, body, stack)
                    let ev1 = EstimateInternal(guard)
                    let ev2 = EstimateInternal(body)
                    <@ %ev1 + %ev2 @>

                | Patterns.Call(o, mi, arguments) ->
                    // Check if this is a call to a reflected function
                    match mi with
                    | DerivedPatterns.MethodWithReflectedDefinition(b) ->
                        // It's a reflected method
                        match QuotationAnalysis.GetCurriedOrTupledArgs(b) with
                        | Some(paramVars) ->       
                            let parameters = List.zip (mi.GetParameters() |> List.ofArray) paramVars
                            // Count inside arguments
                            let l = EstimateList(arguments)
                            // We must be sure that variables used for arguments can be unfolded to expressions of parameters, constants an work size functions
                            let unfoldedArguments = arguments |> List.map(fun (it:Expr) -> UnfoldExpr(it, stack))
                            // Build an evaluator for this function as if it was a kernel
                            let evaluatorExpr, placeholders = ExpressionCounter.Count(b, parameters |> Array.ofList, action, considerLoopIncr)
                            let evaluator = LeafExpressionConverter.EvaluateQuotation(evaluatorExpr) :?> float32 -> float32 -> WorkItemIdContainer -> float32
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
                            call <- Expr.Application(call, Expr.Var(workItemIdContainerPlaceholder))

                            // We create an expression that sums the estimation for arguments to the invocation of the evaluator
                            // The evaluation requires the values for work size functions (global_size_num, groups, etc)
                            // We pass the placeholders to the corresponding functions
                            let fv = call.GetFreeVars() |> Seq.toArray
                            // Now we add this expr to the estimation of the arguments
                            <@ %l + %%call:float32 @>
                        | _ ->      
                            EstimateList(arguments)
                    | _ ->      
                        EstimateList(arguments)
                | ExprShape.ShapeVar(var) ->
                    <@ 0.0f @>
                | ExprShape.ShapeLambda(var, lambda) ->
                    EstimateInternal (lambda)
                | ExprShape.ShapeCombination(o, e) ->        
                    EstimateList(e)
                | _ -> 
                    raise (CountError("Cannot build instruction evaluation lamba because of unrecognized patterns"))

        and EstimateList(l: Expr list) =
            if l.IsEmpty then
                <@ 0.0f @>
            else
                let result = ref (EstimateInternal(l.[0]))
                for i = 1 to l.Length - 1 do
                    let v = EstimateInternal(l.[i])
                    result := <@ %(!result) + %v @>
                !result
                    
        and CheckOpRange(expr: Expr) =
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
                                                let es = EstimateInternal(starte)
                                                let ee = EstimateInternal(ende)
                                                let se = EstimateInternal(stepe)
                                                let subexpr = EstimateInternal(body)

                                                let unfoldStart = UnfoldExpr(starte, stack)
                                                let unfoldEnd = UnfoldExpr(ende, stack)
                                                let unfoldStep = UnfoldExpr(stepe, stack)
                                                let subexpr = EstimateInternal(body)
                                                let incCount = if considerLoopIncr then 1.0f else 0.0f
                                                let result = <@
                                                                 %es + %se + ((float32)(Math.Ceiling((float)(((float32)(%%unfoldEnd:int) - (float32)(%%unfoldStart:int) + 1.0f) / (float32)(%%unfoldStep:int)))) * (float32)(%subexpr + %ee + incCount))
                                                             @>
                                                Some(result)
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
                let isParameterReference = (Array.tryFind (fun (p:ParameterInfo, pv:Var) -> pv = v) parameters).IsSome
                let isWorkSizeFunctionReference = (v = workItemIdContainerPlaceholder)
                let isDynamicDefineReference = (List.tryFind (fun (pv:Var) -> pv = v) dynamicDefinesPlaceholders).IsSome
                if isParameterReference || isWorkSizeFunctionReference || isDynamicDefineReference then
                   expr
                else
                    let varValue, stackTail = ExpressionCounter.GetStackVarAndTail(v, stack)
                    UnfoldExpr(varValue, stackTail)
            | ExprShape.ShapeLambda(v, b) ->
                Expr.Lambda(v, UnfoldExpr(b, stack))
            | ExprShape.ShapeCombination(o, l) ->
                let replList = l |> List.map(fun e -> UnfoldExpr(e, stack))
                ExprShape.RebuildShapeCombination(o, replList)

        and CheckModification(vs: Var list, expr: Expr, varModification: Dictionary<Var, Expr list>, validModificationContext: bool) =
            match expr with
            | Patterns.VarSet(v, value) ->
                if ((List.tryFind (fun vl -> vl = v) vs).IsSome) then 
                    // One of the while loop guard variables is modified here
                    if not validModificationContext then
                        false
                    else
                        // Add this modification to the set of modifications for the variable
                        if not (varModification.ContainsKey(v)) then
                            varModification.Add(v, [])
                        varModification.[v] <- varModification.[v] @ [value]
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

        and BuildWhileLoopEstimator(guard:Expr, body:Expr, stack: VarStack) =
            // Determine the free variables in guard
            let freeVars = ReplaceFunctionBody(functionBody, guard).Value.GetFreeVars() |> List.ofSeq
            let varModification = new Dictionary<Var, Expr list>()
            let isValidModification = CheckModification(freeVars, body, varModification, true)
            ()

        EstimateInternal(functionBody)            
            
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
        let stack = new VarStack()
        let workItemIdContainerPlaceholder = Quotations.Var("workItemIdContainer", typeof<WorkItemIdContainer>)
        let dynamicDefinePlaceholders = new List<Var>()

        let prepBody = ExpressionCounter.PrepareBody(body, workItemIdContainerPlaceholder, dynamicDefinePlaceholders);
        let precExpr = ExpressionCounter.Estimate(prepBody.Value, parameters, action, stack, workItemIdContainerPlaceholder, dynamicDefinePlaceholders |> List.ofSeq, considerLoopIncr)
        let cleanCountExpr = ExpressionCounter.CleanInstructionCount(precExpr)
        (ExpressionCounter.CloseCountExpression(prepBody.Value, cleanCountExpr), dynamicDefinePlaceholders)
            
    static member ContinueCount (parameters: (ParameterInfo * Var)[])
                                (action: Expr * (ParameterInfo * Var)[] * (Expr -> Expr<float32>) -> CountAction) 
                                (stack: VarStack) 
                                (workItemIdContainerPlaceholder: Quotations.Var)
                                (dynamicDefinesPlaceholders: Var list)
                                (considerLoopIncr: bool)
                                (e: Expr) =
        let precExpr = ExpressionCounter.Estimate(e, parameters, action, stack, workItemIdContainerPlaceholder, dynamicDefinesPlaceholders, considerLoopIncr)
        ExpressionCounter.CleanInstructionCount(precExpr)

    static member private CloseCountExpression(body: Expr, precExpr: Expr) = 
        // We build an expression with args preparation preamble where
        // each parameter is bound to the proper arg
        let preparedExpr = ReplaceFunctionBody(body, precExpr).Value
        // Return
        preparedExpr



        

      