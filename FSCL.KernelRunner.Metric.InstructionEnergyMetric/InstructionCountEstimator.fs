namespace InstructionEnergy.Tools

open Microsoft.FSharp.Quotations
open System.Reflection
open Microsoft.FSharp.Reflection
open FSCL.KernelRunner.MetricTools
open Microsoft.FSharp.Linq.QuotationEvaluation
open System.Collections.Generic

module internal InstructionCountEstimator =   
    // Checks if vars in an expression referes exclusively the parameters contained in a list
    let plusMethodInfo = ExpressionAnalyzer.GetOperatorMethodInfo (<@ (+) @>, typeof<double>)
    let multMethodInfo = ExpressionAnalyzer.GetOperatorMethodInfo (<@ (*) @>, typeof<double>)
    let subMethodInfo = ExpressionAnalyzer.GetOperatorMethodInfo (<@ (-) @>, typeof<double>)
    let greatMethodInfo = ExpressionAnalyzer.GetOperatorMethodInfo (<@ (>) @>, typeof<double>)
    let mutable stack = EmptyStack
    (*
        A loop condition may recursively depend on calls such as get_global_id, get_local_size and so on.
        Since these are stub functions (no meaningful implementation) we need to replace the call with a (new) variable.
        When instantiated, the metric should at first assign the appropriate value to each variable. Therefore we
        store them into a list, to be used during instantiation.
        Each function call is replaced with an array access. So, get_global_id(0) is replaced with get_global_id_placeholder.[0]
        At instantiation time, we must define the array value.
    *)
    let (fsclFunctionCallPlaceholders:Var list) = [ 
        Quotations.Var("get_global_id_placeholder", typeof<int[]>);
        Quotations.Var("get_local_id_placeholder", typeof<int[]>);
        Quotations.Var("get_global_size_placeholder", typeof<int[]>);
        Quotations.Var("get_local_size_placeholder", typeof<int[]>);
        Quotations.Var("get_num_groups_placeholder", typeof<int[]>);
        Quotations.Var("get_group_id_placeholder", typeof<int[]>) ]

    let GetFSCLPlaceholder(mi:MethodInfo) =
        List.find (fun (v:Var) -> v.Name.StartsWith(mi.Name)) fsclFunctionCallPlaceholders
            
    // Stack function helpers
    let GetStackVar(var:Var) =
        let rec GetStackVarInner(stackinner:Dictionary<Var, Expr> stack) =
            let head = Stack.hd stackinner
            if head.ContainsKey(var) then
                head.[var]
            else
                GetStackVarInner(Stack.tl stackinner)
        GetStackVarInner (stack)
        
    let SetStackVar(var:Var, value:Expr) =
        let rec SetStackVarInner(stackinner:Dictionary<Var, Expr> stack) =
            if Stack.empty <> stackinner then
                let head = Stack.hd stackinner
                if head.ContainsKey(var) then
                    head.[var] <- value
                else
                   SetStackVarInner(Stack.tl stackinner)
        SetStackVarInner (stack)

    let PushStack() =
        stack <- Stack.push stack (new Dictionary<Var, Expr>())
            
    let PopStack() =
        stack <- Stack.pop stack
        
    let AddStackVar(var:Var, value:Expr) =
        let head = Stack.hd stack
        head.Add(var, value)

    // Estimator functions
    let rec internal unfoldLoopExpr(expr:Expr, pars:ParameterInfo[]) =
        match expr with
        | Patterns.Var(v) ->
            let isParameterReference = (Array.filter (fun (p:ParameterInfo) -> p.Name = v.Name && v.Type = p.ParameterType) pars).Length <> 0
            if isParameterReference then
                Expr.Var(v)
            else
                let stackValue = GetStackVar(v)
                unfoldLoopExpr(stackValue, pars)
        | Patterns.Value(v, t) ->
            Expr.Value(v, t)
        | Patterns.Call(ob, mi, args) ->
            // Check if it is a call to an fscl function
            if mi.DeclaringType.Name = "KernelFunctions" then
                let placeholder = Expr.Var(GetFSCLPlaceholder(mi))
                let unfoldIndex = unfoldLoopExpr(args.[0], pars)
                <@@ (%%placeholder : int[]).[(%%unfoldIndex) : int] @@>
            else
                match expr with
                | DerivedPatterns.SpecificCall <@ (+) @> (e, t, a) 
                | DerivedPatterns.SpecificCall <@ (-) @> (e, t, a) 
                | DerivedPatterns.SpecificCall <@ (*) @> (e, t, a) 
                | DerivedPatterns.SpecificCall <@ (/) @> (e, t, a) 
                | DerivedPatterns.SpecificCall <@ (%) @> (e, t, a) 
                | DerivedPatterns.SpecificCall <@ (&&) @> (e, t, a) 
                | DerivedPatterns.SpecificCall <@ (||) @> (e, t, a) 
                | DerivedPatterns.SpecificCall <@ (&&&) @> (e, t, a) 
                | DerivedPatterns.SpecificCall <@ (|||) @> (e, t, a) 
                | DerivedPatterns.SpecificCall <@ (<<<) @> (e, t, a) 
                | DerivedPatterns.SpecificCall <@ (>>>) @> (e, t, a) 
                | DerivedPatterns.SpecificCall <@ (^^^) @> (e, t, a) 
                | DerivedPatterns.SpecificCall <@ (~-) @> (e, t, a) 
                | DerivedPatterns.SpecificCall <@ (~+) @> (e, t, a) ->
                    if ob.IsSome then
                        Expr.Call(e.Value, mi, List.map(fun arge -> unfoldLoopExpr(arge, pars)) a)
                    else
                        Expr.Call(mi, List.map(fun arge -> unfoldLoopExpr(arge, pars)) a)
                | _ ->
                    expr
        | Patterns.PropertyGet(e, pi, args) ->
            match pi with
            | DerivedPatterns.PropertyGetterWithReflectedDefinition(e) ->
                let freeVars = List.ofSeq(e.GetFreeVars())
                if freeVars.IsEmpty then
                    let value = e.EvalUntyped()
                    Expr.Value (value, e.Type)
                else
                    raise (MetricEvaluationError("Error during variable unfolding: cannot get the value of var [" + pi.Name + "]"))
            | _ ->
                raise (MetricEvaluationError("Error during variable unfolding: cannot get the value of var [" + pi.Name + "]"))
        | _ ->
            raise (MetricEvaluationError("Error during variable unfolding: cannot recognize construct [" + expr.ToString() + "]"))
           
    // Check if a let .. in is the beginning of the AST of an input Sequence, that is a for x in y..k..w do
    let rec CheckOpRange(expr: Expr, args: ParameterInfo[]) =
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
                                            let unfoldStart = unfoldLoopExpr(starte, args)
                                            let unfoldEnd = unfoldLoopExpr(ende, args)
                                            let unfoldStep = unfoldLoopExpr(stepe, args)
                                            let subexpr = Estimate(body, args)
                                            let result = <@@
                                                            ((double)(((%%unfoldEnd:int) - (%%unfoldStart:int) + 1) / (%%unfoldStep:int)) * ((%%subexpr:double) + 1.0))
                                                         @@>
                                            Some(result)
                                        | _ -> None
                                    | _ -> None
                                | _ -> None
                            | _ -> None
                        | _ -> None
                    | _ -> None                                           
                else
                    None
            | _ -> None  
        | _ -> None
                
    and internal Estimate (expr: Expr, args: ParameterInfo[]) =
        match expr with        
        | Patterns.Let (v, value, body) ->
            let maybeOpRange = CheckOpRange(expr, args)
            if maybeOpRange.IsSome then
                maybeOpRange.Value
            else
                let first = Estimate(value, args)
                PushStack()
                AddStackVar(v, value)
                let second = Estimate(body, args)
                PopStack()
                second

        | Patterns.VarSet (v, e) ->
            let first = Estimate(e, args)
            SetStackVar(v, e)
            first

        | Patterns.Call(e, i, a) ->
            EstimateCall(expr, args)

        | Patterns.IfThenElse (c, ib, eb) ->
            let cond = Estimate(c, args)
            let ifb = Estimate(ib, args)
            let elseb = Estimate(eb, args)
            // Fixe: before we had half the instrs of if branch and half the instrs of else branch. But on GPU often executed in lockstep
            let result = <@@ (double)((%%cond:double) + (%%ifb:double) + (%%elseb:double)) @@>
            result

        | Patterns.ForIntegerRangeLoop(v, starte, ende, body) ->
            // Check that startv is an expression of constants and fers to parameters
            let unfoldStart = unfoldLoopExpr(starte, args)
            let unfoldEnd = unfoldLoopExpr(ende, args)
            let subexpr = Estimate(body, args)
            let e = <@@
                        if ((%%starte : int) > (%%ende : int)) then
                            ((double)((%%unfoldStart : int) - (%%unfoldEnd : int) + 1)) * (%%subexpr + 1.0)
                        else
                            ((double)((%%unfoldEnd : int) - (%%unfoldStart : int) + 1)) * (%%subexpr + 1.0)
                    @@>
            e        
        | ExprShape.ShapeVar(var) ->
            Expr.Value (0.0)
        | ExprShape.ShapeLambda(var, lambda) ->
            Estimate (lambda, args)
        | ExprShape.ShapeCombination(o, e) ->        
            EstimateList (e, args)
        | _ -> 
            raise (MetricEvaluationError("Cannot build instruction evaluation lamba because of unrecognized patterns"))

    and internal EstimateList (l: Expr list, args: ParameterInfo[]) =
        if l.IsEmpty then
            Expr.Value (0.0)
        else
            let result = ref (Estimate(l.[0], args))
            for i = 1 to l.Length - 1 do
                result := Expr.Call(plusMethodInfo.Value, [ !result; Estimate(l.[i], args) ])
            !result
                             
    and internal EstimateCall (expr:Expr, args: ParameterInfo[]) =   
        match expr with        
        | DerivedPatterns.SpecificCall <@ (+) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (-) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (*) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (/) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (%) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (&&) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (||) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (&&&) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (|||) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (<<<) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (>>>) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (^^^) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (~-) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (~+) @> (e, t, a) ->
            if e.IsSome then
                let e = Expr.Call(
                            plusMethodInfo.Value,
                            [ EstimateList(a @ [ e.Value ], args); Expr.Value(1.0) ])
                // Do this to add parenthesis for evaluation priority
                e
            else 
                let e = Expr.Call(
                            plusMethodInfo.Value,
                            [ EstimateList(a, args); Expr.Value(1.0) ])
                // Do this to add parenthesis for evaluation priority
                e
        | Patterns.Call(e, i, a) ->
            if e.IsSome then
                EstimateList(a @ [ e.Value ], args)
            else
                EstimateList(a, args)
        | _ ->
            raise (MetricEvaluationError("Cannot recognize some operations to count instructions"))
    
    // Removes (0+0+0+0+0+) useless counts in the expression
    let rec internal CleanInstructionCount (expr: Expr) =
        if (Seq.isEmpty (expr.GetFreeVars())) then
            let value = expr.EvalUntyped() :?> double
            <@ value @> :> Expr
        else
            match expr with
            | Patterns.Lambda (v, e) ->
                Expr.Lambda(v, CleanInstructionCount(e))
            | Patterns.Let (v, e, b) ->
                Expr.Let(v, CleanInstructionCount(e), CleanInstructionCount(b))
            | Patterns.Call(e, i, [f; s]) ->  
                if (e.IsSome) then 
                    Expr.Call(CleanInstructionCount(e.Value), i, List.map (fun el -> CleanInstructionCount(el)) [f; s])
                else
                    Expr.Call(i, List.map (fun el -> CleanInstructionCount(el)) [f; s])
            | _ ->
                expr
    
    let EstimateInstructionCount (meth: MethodBase) =
        let args = meth.GetParameters()
        match meth with
        | DerivedPatterns.MethodWithReflectedDefinition (b) ->
            // Create a lambda to evaluate instruction count
            let lambdaBody = Estimate(b, args)
            let lc = CleanInstructionCount(lambdaBody)
            Some(lc)
        | _ ->
            None

        

