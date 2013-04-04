namespace FSCL.KernelRunner.Metric.InstructionEnergyMetric

open Microsoft.FSharp.Quotations

module internal ExpressionAnalyzer =
    let GetOperatorMethodInfo (expr, ty: System.Type) =
        let rec getOpMethodInfo expr =
            match expr with
            | Patterns.Lambda (v, e) ->
                getOpMethodInfo e
            | DerivedPatterns.SpecificCall <@ (>) @> (e, t, a) 
            | DerivedPatterns.SpecificCall <@ (<) @> (e, t, a) 
            | DerivedPatterns.SpecificCall <@ (>=) @> (e, t, a) 
            | DerivedPatterns.SpecificCall <@ (<=) @> (e, t, a) 
            | DerivedPatterns.SpecificCall <@ (<>) @> (e, t, a) 
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
                match expr with 
                | Patterns.Call (e, i, l) ->
                    Some(
                        i.GetGenericMethodDefinition().MakeGenericMethod(Array.create (i.GetGenericMethodDefinition().GetGenericArguments().Length) ty))
                | _ ->
                    None
            | _ ->
                None

        getOpMethodInfo(expr)
    

