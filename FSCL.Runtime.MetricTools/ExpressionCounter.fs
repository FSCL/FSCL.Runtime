namespace FSCL.Runtime.MetricTools

open Microsoft.FSharp.Quotations
open System.Reflection
open Microsoft.FSharp.Reflection
open FSCL.Compiler.Core.Util
open FSCL.Compiler.Core.Util.QuotationEvaluation
open System.Collections.Generic
open Stack

type CountAction =
| Value of Expr
| Continue

type CountError(msg: string) =
    inherit System.Exception(msg)

type Counter(mi: MethodInfo) =   
    let detbody = 
        match mi with
        | DerivedPatterns.MethodWithReflectedDefinition (b) ->
            b
        | _ ->
            raise (CountError("Only methods tagged with Reflected Definition attributes can be analyzed!"))

    member val private stack = EmptyStack with get, set
    member val private parameters = mi.GetParameters() with get
    member val private callback = (fun (e, p) -> Continue) with get, set
    member val private body = detbody with get
    member val private considerLoopInc = false with get, set
         
    member val private fsclFunctionCallPlaceholders:Var list = [ 
        Quotations.Var("get_global_id_placeholder", typeof<int[]>);
        Quotations.Var("get_local_id_placeholder", typeof<int[]>);
        Quotations.Var("get_global_size_placeholder", typeof<int[]>);
        Quotations.Var("get_local_size_placeholder", typeof<int[]>);
        Quotations.Var("get_num_groups_placeholder", typeof<int[]>);
        Quotations.Var("get_group_id_placeholder", typeof<int[]>) ] with get

    member private this.GetFSCLPlaceholder(mi:MethodInfo) =
        List.find (fun (v:Var) -> v.Name.StartsWith(mi.Name)) this.fsclFunctionCallPlaceholders
           
    // Stack function helpers
    member private this.GetStackVar(var:Var) =
        let rec GetStackVarInner(stackinner:Dictionary<Var, Expr> stack) =
            let head = Stack.hd stackinner
            if head.ContainsKey(var) then
                head.[var]
            else
                GetStackVarInner(Stack.tl stackinner)
        GetStackVarInner (this.stack)
        
    member private this.SetStackVar(var:Var, value:Expr) =
        let rec SetStackVarInner(stackinner:Dictionary<Var, Expr> stack) =
            if Stack.empty <> stackinner then
                let head = Stack.hd stackinner
                if head.ContainsKey(var) then
                    head.[var] <- value
                else
                   SetStackVarInner(Stack.tl stackinner)
        SetStackVarInner (this.stack)

    member private this.PushStack() =
        this.stack <- Stack.push this.stack (new Dictionary<Var, Expr>())
            
    member private this.PopStack() =
        this.stack <- Stack.pop this.stack
        
    member private this.AddStackVar(var:Var, value:Expr) =
        let head = Stack.hd this.stack
        head.Add(var, value)

    // Estimator functions
    member private this.UnfoldLoopExpr(expr:Expr) =
        match expr with
        | Patterns.Var(v) ->
            let isParameterReference = (Array.filter (fun (p:ParameterInfo) -> p.Name = v.Name && v.Type = p.ParameterType) this.parameters).Length <> 0
            if isParameterReference then
                Expr.Var(v)
            else
                let stackValue = this.GetStackVar(v)
                this.UnfoldLoopExpr(stackValue)
        | Patterns.Value(v, t) ->
            Expr.Value(v, t)
        | Patterns.Call(ob, mi, args) ->
            // Check if it is a call to an fscl function
            if mi.DeclaringType.Name = "KernelFunctions" then
                let placeholder = Expr.Var(this.GetFSCLPlaceholder(mi))
                let unfoldIndex = this.UnfoldLoopExpr(args.[0])
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
                        Expr.Call(e.Value, mi, List.map(fun arge -> this.UnfoldLoopExpr(arge)) a)
                    else
                        Expr.Call(mi, List.map(fun arge -> this.UnfoldLoopExpr(arge)) a)
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
                    raise (CountError("Error during variable unfolding: cannot get the value of var [" + pi.Name + "]"))
            | _ ->
                raise (CountError("Error during variable unfolding: cannot get the value of var [" + pi.Name + "]"))
        | _ ->
            raise (CountError("Error during variable unfolding: cannot recognize construct [" + expr.ToString() + "]"))
           
    // Check if a let .. in is the beginning of the AST of an input Sequence, that is a for x in y..k..w do
    member private this.CheckOpRange(expr: Expr) =
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
                                            let es = this.Estimate(starte)
                                            let ee = this.Estimate(ende)
                                            let se = this.Estimate(stepe)
                                            let subexpr = this.Estimate(body)

                                            let unfoldStart = this.UnfoldLoopExpr(starte)
                                            let unfoldEnd = this.UnfoldLoopExpr(ende)
                                            let unfoldStep = this.UnfoldLoopExpr(stepe)
                                            let subexpr = this.Estimate(body)
                                            let incCount = if this.considerLoopInc then 1.0 else 0.0
                                            let result = <@@
                                                            (((%%es : double)) + ((%%es : double)) + ((%%se : double)) + (((double)(((%%unfoldEnd:int) - (%%unfoldStart:int) + 1) / (%%unfoldStep:int)) * ((%%subexpr:double) + incCount))))
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
                
    member private this.Estimate (expr: Expr) =
        match this.callback(expr, this.parameters) with
        | Value(c) ->
            c
        | _ ->
            match expr with        
            | Patterns.Let (v, value, body) ->
                let maybeOpRange = this.CheckOpRange(expr)
                if maybeOpRange.IsSome then
                    maybeOpRange.Value
                else
                    let first = this.Estimate(value)
                    this.PushStack()
                    this.AddStackVar(v, value)
                    let second = this.Estimate(body)
                    this.PopStack()
                    second

            | Patterns.VarSet (v, e) ->
                let first = this.Estimate(e)
                this.SetStackVar(v, e)
                first

            | Patterns.IfThenElse (c, ib, eb) ->
                let cond = this.Estimate(c)
                let ifb = this.Estimate(ib)
                let elseb = this.Estimate(eb)
                // Fixe: before we had half the instrs of if branch and half the instrs of else branch. But on GPU often executed in lockstep
                let result = <@@ (double)((%%cond:double) + (%%ifb:double) + (%%elseb:double)) @@>
                result

            | Patterns.ForIntegerRangeLoop(v, starte, ende, body) ->
                // Check that startv is an expression of constants and fers to parameters
                let es = this.Estimate(starte)
                let ee = this.Estimate(ende)
                let subexpr = this.Estimate(body)

                let unfoldStart = this.UnfoldLoopExpr(starte)
                let unfoldEnd = this.UnfoldLoopExpr(ende)
                let incCount = if this.considerLoopInc then 1.0 else 0.0
                <@@
                    if ((%%starte : int) > (%%ende : int)) then
                        ((%%es : double)) + ((%%ee : double)) + (((double)((%%unfoldStart : int) - (%%unfoldEnd : int) + 1)) * (%%subexpr + incCount))
                    else
                        ((%%es : double)) + ((%%ee : double)) + (((double)((%%unfoldEnd : int) - (%%unfoldStart : int) + 1)) * (%%subexpr + incCount))
                @@>
            | ExprShape.ShapeVar(var) ->
                Expr.Value (0.0)
            | ExprShape.ShapeLambda(var, lambda) ->
                this.Estimate (lambda)
            | ExprShape.ShapeCombination(o, e) ->        
                this.EstimateList (e)
            | _ -> 
                raise (CountError("Cannot build instruction evaluation lamba because of unrecognized patterns"))

    member private this.EstimateList (l: Expr list) =
        if l.IsEmpty then
            Expr.Value (0.0)
        else
            let result = ref (this.Estimate(l.[0]))
            for i = 1 to l.Length - 1 do
                let v = this.Estimate(l.[i])
                result := <@@ ((%%(!result) : double)) + (%%v : double) @@>
            !result
    
    // Removes (0+0+0+0+0+) useless counts in the expression
    member private this.CleanInstructionCount (expr: Expr) =
        if (Seq.isEmpty (expr.GetFreeVars())) then
            let value = expr.EvalUntyped() :?> double
            <@ value @> :> Expr
        else
            match expr with
            | Patterns.Lambda (v, e) ->
                Expr.Lambda(v, this.CleanInstructionCount(e))
            | Patterns.Let (v, e, b) ->
                Expr.Let(v, this.CleanInstructionCount(e), this.CleanInstructionCount(b))
            | Patterns.Call(e, i, [f; s]) ->  
                if (e.IsSome) then 
                    Expr.Call(this.CleanInstructionCount(e.Value), i, List.map (fun el -> this.CleanInstructionCount(el)) [f; s])
                else
                    Expr.Call(i, List.map (fun el -> this.CleanInstructionCount(el)) [f; s])
            | _ ->
                expr
    
    member this.Count(call: Expr * ParameterInfo[] -> CountAction, considerLoopIncr) =    
        this.callback <- call    
        this.considerLoopInc <- considerLoopIncr
        // Create a lambda to evaluate instruction count
        let lambdaBody = this.Estimate(this.body)
        this.CleanInstructionCount(lambdaBody)
            
    member this.ContinueCount (e: Expr) =
        let lambdaBody = this.Estimate(e)
        this.CleanInstructionCount(lambdaBody)



        

