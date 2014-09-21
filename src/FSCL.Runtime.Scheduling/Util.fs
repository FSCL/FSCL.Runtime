namespace FSCL.Runtime.Scheduling

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open System.Collections.Generic
open System
open FSCL.Language
open System.Reflection
open Microsoft.FSharp.Linq.RuntimeHelpers
open VarStack 

module ReflectionUtil =
    let ToTuple(args: obj[]) =
        let tupleType = FSharpType.MakeTupleType(Array.mapi(fun idx (i:obj) -> 
            if i :? WorkSize && idx = args.Length - 1 then
                typeof<WorkItemInfo>
            else
                i.GetType()) args)
        let tuple = FSharpValue.MakeTuple(args, tupleType)
        tuple
            
    let rec GetMethodInfoFromQuotation(e: Expr) = 
        match e with
        | Patterns.Call(o, mi, a) ->
            mi
        | ExprShape.ShapeVar(v) ->
            failwith "Cannot find a call to a method inside the given expression"
        | ExprShape.ShapeLambda(v, b) ->
            GetMethodInfoFromQuotation(b)
        | ExprShape.ShapeCombination(o, l) ->
            failwith "Cannot find a call to a method inside the given expression"


// Quotations-related functions
module QuotationUtil =
    let rec ReplaceExpr(e: Expr, f: Expr -> Expr option) =
        match f e with
        | Some(newExpression) ->
            newExpression
        | None ->
            match e with
            | ExprShape.ShapeVar(v) ->
                e
            | ExprShape.ShapeLambda(v, b) ->
                let nb = ReplaceExpr(b, f)
                Expr.Lambda(v, nb)
            | ExprShape.ShapeCombination(o, args) ->
                let na = args |> List.map(fun e -> ReplaceExpr(e, f))
                ExprShape.RebuildShapeCombination(o, na)
                                
    let rec ReplaceVars(e: Expr, vars: (Var * Expr) list) =
        match e with
        | Patterns.Var(v) ->
            let va = vars |> List.tryFind (fun (v1, va1) -> v1 = v)
            if va.IsSome then
                va.Value |> snd
            else
                e
        | _ ->
            match e with
            | ExprShape.ShapeVar(v) ->
                e
            | ExprShape.ShapeLambda(v, b) ->
                let nb = ReplaceVars(b, vars)
                Expr.Lambda(v, nb)
            | ExprShape.ShapeCombination(o, args) ->
                let na = args |> List.map(fun e -> ReplaceVars(e, vars))
                ExprShape.RebuildShapeCombination(o, na)
                
    // Normalizes a multi-dim array access expressions, creating an expression for 1D access
    let NormalizeArrayAccess(arrayExpr: Expr, accessExprs: Expr list) =        
        let mutable normalisedExpr = <@ (%%accessExprs.[0]) @>
        let lengthMethodCall = arrayExpr.Type.GetMethod("GetLength");
        for idx = 1 to accessExprs.Length - 1 do         
            let length = Expr.Call(arrayExpr, lengthMethodCall, [ <@@ idx - 1 @@> ])               
            normalisedExpr <- <@ ((%normalisedExpr * (%%length:int)) + (%%accessExprs.[idx]:int)) @>
        normalisedExpr

    // Replaces the body of a tupled function (preserving arguments binding)
    let ReplaceTupledFunctionBody(root: Expr, haystack: Expr) =
        let rec InsertRecursive(expr) =
            match expr with
            | Patterns.Let(v, value, body) ->
                match value with
                | Patterns.TupleGet(te, i) ->
                    Expr.Let(v, Expr.TupleGet(te, i), InsertRecursive(body)) 
                | _ ->
                    haystack
            | _ ->
                haystack
                
        match root with
        | Patterns.Lambda(v, e) ->
            if v.Name = "tupledArg" then
                Some(Expr.Lambda(v, InsertRecursive(e)))
            else
                None
        | _ ->
            None

    let ReplaceCurriedFunctionBody(root: Expr, haystack: Expr) =
        let rec InsertRecursive(expr) =
            match expr with
            | Patterns.Lambda(v, body) ->
                Expr.Lambda(v, InsertRecursive(body))
            | _ ->
                haystack
                
        match root with
        | Patterns.Lambda(v, e) ->
            Some(Expr.Lambda(v, InsertRecursive(e)))
        | _ ->
            None

    let ReplaceFunctionBody(root: Expr, haystack: Expr) =
        match ReplaceTupledFunctionBody(root, haystack) with
        | Some(b) ->
            Some(b)
        | None ->
            match ReplaceCurriedFunctionBody(root, haystack) with
            | Some(b) ->
                Some(b)
            | _ ->
                None
                                
    let AddParametersToTupledFunction(f: Expr, pars: Var list) =
        let rec GetFunctionBody(expr) =
            match expr with
            | Patterns.Let(v, value, body) ->
                match value with
                | Patterns.TupleGet(te, i) ->
                    GetFunctionBody(body) 
                | _ ->
                    expr
            | _ ->
                expr
                
        match f with
        | Patterns.Lambda(v, e) ->
            if v.Name = "tupledArg" then
                let originalTypes = FSharpType.GetTupleElements(v.Type) |> List.ofArray
                let additionalTypes = pars |> List.map (fun i -> i.Type)
                let newTupleType = FSharpType.MakeTupleType((originalTypes @ additionalTypes) |> Array.ofList)
                let newTupleVar = Quotations.Var(v.Name, newTupleType)
                // Now replace this var in the param vars preparation
                let replExpr = e.Substitute(fun (nv:Var) -> if nv = v then Some(Expr.Var(newTupleVar)) else None)
                // Now add preparation for additional args
                let mutable fBody = GetFunctionBody(e)
                for i = additionalTypes.Length - 1 downto 0 do
                    fBody <- Expr.Let(pars.[i], Expr.TupleGet(Expr.Var(newTupleVar), i + originalTypes.Length), fBody)
                // Now insert the new body into the previous pars preparation
                let newLambda = ReplaceTupledFunctionBody(Expr.Lambda(newTupleVar, replExpr), fBody) 
                // Return
                newLambda
            else
                None
        | _ ->
            None
                          
    let AddParametersToCurriedFunction(f: Expr, pars: Var list) =
        let rec AppendParametersToCurriedFunction(expr, vs: Var list) =
            match expr with
            | Patterns.Lambda(v, body) ->                
                Expr.Lambda(v, AppendParametersToCurriedFunction(body, vs))
            | _ ->
                match vs with
                | a::tail ->
                    Expr.Lambda(a, AppendParametersToCurriedFunction(expr, tail))
                | [] ->
                    expr                
        match f with
        | Patterns.Lambda(v, e) ->
            if v.Name <> "tupledArg" then
                Some(AppendParametersToCurriedFunction(f, pars))
            else
                None
        | _ ->
            None

    let ToTupledFunction(f: Expr) = 
        let rec convertToTupledInternal(tupledVar: Var, tupledIndex: int, e: Expr) =
            match e with
            | Patterns.Lambda(v, body) ->
                Expr.Let(v, 
                         Expr.TupleGet(Expr.Var(tupledVar), tupledIndex), 
                         convertToTupledInternal(tupledVar, tupledIndex + 1, body)) 
            | _ ->
                e
        let rec extractParamTypesInternal(currentList: Type list, e: Expr) =
            match e with
            | Patterns.Lambda(v, body) ->
                extractParamTypesInternal(currentList @ [ v.Type ], body)
            | _ ->
                currentList
                   
        match f with
        | Patterns.Lambda(v, e) ->
            if v.Name = "tupledArg" then
                // Already tupled
                f
            else
                let types = extractParamTypesInternal([], f)
                let tupledVarType = FSharpType.MakeTupleType(types |> List.toArray)
                let tupledVar = Quotations.Var("tupledArg", tupledVarType)
                Expr.Lambda(tupledVar, convertToTupledInternal(tupledVar, 0, e))
        | _ ->
            failwith "Cannot convert to tupled an expression that doesn't contain a function"

    let ToCurriedFunction(f: Expr) = 
        let rec replaceTupleGetWithLambda(tupledArgVar: Var, e: Expr) =
            match e with
            | Patterns.Let(v, value, body) ->
                match value with
                | Patterns.TupleGet(tve, idx) ->
                    match tve with
                    | Patterns.Var(tv) ->
                        if tv = tupledArgVar then
                            Expr.Lambda(v, replaceTupleGetWithLambda(tupledArgVar, body))
                        else
                            e
                    | _ ->
                        e
                | _ ->
                    e
            | _ ->
                e
                   
        match f with
        | Patterns.Lambda(v, e) ->
            if v.Name <> "tupledArg" then
                // Already curried
                f
            else
                let repl = replaceTupleGetWithLambda(v, e) 
                repl
        | _ ->
            failwith "Cannot convert to tupled an expression that doesn't contain a function"


module KernelUtil =
    let ReplaceDynamicConstantDefines(expr: Expr, dynamicDefinesPlaceholders: List<Var>) =
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

    let PrepareBodyForAnalysis(body: Expr,
                               dynamicDefinesPlaceholders: List<Var>) =
        let replacedBody = ReplaceDynamicConstantDefines(
                                    QuotationUtil.ToCurriedFunction(body), 
                                dynamicDefinesPlaceholders)               
        let prep = QuotationUtil.AddParametersToCurriedFunction(replacedBody, (dynamicDefinesPlaceholders |> List.ofSeq))
        prep
        
    let EvaluateClosedSubtrees(expr: Expr<float32>) =
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
                
    let inline CloseExpression(body: Expr, precExpr: Expr) = 
        QuotationUtil.ReplaceFunctionBody(body, precExpr)
                    
    let rec UnfoldExpression(expr:Expr, stack: VarStack, isParameterRef: Var -> bool, isDynamicDefineRef: Var -> bool) =
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
                    failwith ("Error during variable unfolding: cannot get the value of var [" + pi.Name + "]")
            | _ ->
                failwith ("Error during variable unfolding: cannot get the value of var [" + pi.Name + "]")
        // If referring to a var try to replace it with an expression with only references to parameters, work size functions and dynamic defines
        | ExprShape.ShapeVar(v) ->
            if isParameterRef v || isDynamicDefineRef v then
                expr
            else
                let varValue, stackTail = findAndTail v true stack
                if varValue.IsSome then
                    UnfoldExpression(varValue.Value, stackTail, isParameterRef, isDynamicDefineRef)
                else
                    failwith ("Cannot find variable " + v.Name + " to count expression")
        | ExprShape.ShapeLambda(v, b) ->
            Expr.Lambda(v, UnfoldExpression(b, stack, isParameterRef, isDynamicDefineRef))
        | ExprShape.ShapeCombination(o, l) ->
            let replList = l |> List.map(fun e -> UnfoldExpression(e, stack, isParameterRef, isDynamicDefineRef))
            ExprShape.RebuildShapeCombination(o, replList)
            
    let rec UnfoldExpressionPreservingLoopVars(expr:Expr, stack: VarStack, isParameterRef: Var -> bool, isDynamicDefineRef: Var -> bool, isLoopVar: Var -> bool) =
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
                    failwith ("Error during variable unfolding: cannot get the value of var [" + pi.Name + "]")
            | _ ->
                failwith ("Error during variable unfolding: cannot get the value of var [" + pi.Name + "]")
        // If referring to a var try to replace it with an expression with only references to parameters, work size functions and dynamic defines
        | ExprShape.ShapeVar(v) ->
            if isLoopVar v || isParameterRef v || isDynamicDefineRef v then
                expr
            else
                let varValue, stackTail = findAndTail v true stack
                if varValue.IsSome then
                    UnfoldExpressionPreservingLoopVars(varValue.Value, stackTail, isParameterRef, isDynamicDefineRef, isLoopVar)
                else
                    failwith ("Cannot find variable " + v.Name + " to count expression")
        | ExprShape.ShapeLambda(v, b) ->
            Expr.Lambda(v, UnfoldExpressionPreservingLoopVars(b, stack, isParameterRef, isDynamicDefineRef, isLoopVar))
        | ExprShape.ShapeCombination(o, l) ->
            let replList = l |> List.map(fun e -> UnfoldExpressionPreservingLoopVars(e, stack, isParameterRef, isDynamicDefineRef, isLoopVar))
            ExprShape.RebuildShapeCombination(o, replList)
            
    let EvaluateClosedExpression(fe: Expr, args: obj list, dynDefArgs: obj list) =
        let f = LeafExpressionConverter.EvaluateQuotation(fe)
        let mutable result = f
        for a in args do
            result <- result.GetType().GetMethod("Invoke").Invoke(result, [| a |])
        for d in dynDefArgs do
            result <- result.GetType().GetMethod("Invoke").Invoke(result, [| d |])
        result
        
    let EvaluateLambdaObject(f: obj, args: obj list, dynDefArgs: obj list) =
        let mutable result = f
        for a in args do
            result <- result.GetType().GetMethod("Invoke").Invoke(result, [| a |])
        for d in dynDefArgs do
            result <- result.GetType().GetMethod("Invoke").Invoke(result, [| d |])
        result
     

