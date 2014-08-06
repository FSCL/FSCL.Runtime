namespace FSCL.Runtime.Scheduling

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open System.Collections.Generic
open System
open FSCL.Language

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

module QuotationUtil =
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


     

