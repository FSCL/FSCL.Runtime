namespace FSCL.Runtime.Scheduling.FeatureExtraction

open FSCL
open FSCL.Compiler
open FSCL.Runtime.Scheduling
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open System.Reflection
open Microsoft.FSharp.Linq.RuntimeHelpers
open FSCL.Compiler.Util
open FSCL.Language

type ArithmeticOperationCounter() = 
    inherit IDefaultFeatureExtractor()
    override this.FeatureNameList 
        with get() =
            [ "Arithmetic operations (per thread)";
              "Logic operations (per thread)";
              "Bitwise operations (per thread)";
              "Comparison operations (per thread)";
              "Pown operations (per thread)"
              "Sqrt operations (per thread)"
              "Hypot operations (per thread)" ]

    override this.Precompute(m: IKernelModule) =
        let parameters = m.Kernel.OriginalParameters |> 
                         Seq.map(fun (p: IOriginalFunctionParameter) -> 
                                    (p.OriginalParamterInfo, p.OriginalPlaceholder)) |>
                         Array.ofSeq   
        // Count ops
        let acount, ph = ExpressionCounter.Count(m.Kernel.OriginalBody,
                                                 parameters,
                                                 (fun (e, parameters, continuation) ->
                                                    match e with
                                                    | DerivedPatterns.SpecificCall <@ (+) @> (e, t, a) 
                                                    | DerivedPatterns.SpecificCall <@ (-) @> (e, t, a) 
                                                    | DerivedPatterns.SpecificCall <@ (*) @> (e, t, a) 
                                                    | DerivedPatterns.SpecificCall <@ (/) @> (e, t, a) 
                                                    | DerivedPatterns.SpecificCall <@ (%) @> (e, t, a) 
                                                    | DerivedPatterns.SpecificCall <@ (~-) @> (e, t, a) 
                                                    | DerivedPatterns.SpecificCall <@ (~+) @> (e, t, a)  ->
                                                        let fa = continuation(a.[0])
                                                        let sa = continuation(a.[1])
                                                        Value(<@ 1.0f + %fa + %sa @>)
                                                    |  Patterns.Call(o, mi, a) ->
                                                        if mi.Name.EndsWith("pasum") then
                                                            let fa = continuation(a.[0])
                                                            let sa = continuation(a.[1])
                                                            Value(<@ 1.0f + %fa + %sa @>)
                                                        else                                            
                                                            Continue
                                                    | _ ->
                                                        Continue),
                                                 false)

        let lcount, ph = ExpressionCounter.Count(m.Kernel.OriginalBody,
                                                 parameters,
                                                 (fun (e, parameters, continuation) ->
                                                    match e with
                                                    | DerivedPatterns.SpecificCall <@ (&&) @> (e, t, a) 
                                                    | DerivedPatterns.SpecificCall <@ (||) @> (e, t, a) 
                                                    | DerivedPatterns.SpecificCall <@ not @> (e, t, a) ->
                                                        let fa = continuation(a.[0])
                                                        let sa = continuation(a.[1])
                                                        Value(<@ 1.0f + %fa + %sa @>)
                                                    | _ ->
                                                        Continue),
                                                 false)
                                                       
        let bcount, ph = ExpressionCounter.Count(m.Kernel.OriginalBody,
                                                 parameters,
                                                 (fun (e, parameters, continuation) ->
                                                    match e with
                                                    | DerivedPatterns.SpecificCall <@ (&&&) @> (e, t, a) 
                                                    | DerivedPatterns.SpecificCall <@ (|||) @> (e, t, a) 
                                                    | DerivedPatterns.SpecificCall <@ (<<<) @> (e, t, a) 
                                                    | DerivedPatterns.SpecificCall <@ (>>>) @> (e, t, a) 
                                                    | DerivedPatterns.SpecificCall <@ (^^^) @> (e, t, a) ->
                                                        let fa = continuation(a.[0])
                                                        let sa = continuation(a.[1])
                                                        Value(<@ 1.0f + %fa + %sa @>)
                                                    | _ ->
                                                        Continue),
                                                 false)        
        
        let ccount, ph = ExpressionCounter.Count(m.Kernel.OriginalBody,
                                                 parameters,
                                                 (fun (e, parameters, continuation) ->
                                                    match e with
                                                    | DerivedPatterns.SpecificCall <@ (>) @> (e, t, a) 
                                                    | DerivedPatterns.SpecificCall <@ (<) @> (e, t, a) 
                                                    | DerivedPatterns.SpecificCall <@ (>=) @> (e, t, a) 
                                                    | DerivedPatterns.SpecificCall <@ (<=) @> (e, t, a) 
                                                    | DerivedPatterns.SpecificCall <@ (=) @> (e, t, a) 
                                                    | DerivedPatterns.SpecificCall <@ (<>) @> (e, t, a) ->
                                                        let fa = continuation(a.[0])
                                                        let sa = continuation(a.[1])
                                                        Value(<@ 1.0f + %fa + %sa @>)
                                                    | _ ->
                                                        Continue),
                                                 false)        
        
        let pcount, ph = ExpressionCounter.Count(m.Kernel.OriginalBody,
                                                 parameters,
                                                 (fun (e, parameters, continuation) ->
                                                    match e with
                                                    | DerivedPatterns.SpecificCall <@ ( ** ) @> (e, t, a) ->
                                                        let fa = continuation(a.[0])
                                                        let sa = continuation(a.[1])
                                                        Value(<@ 1.0f + %fa + %sa @>)
                                                    | Patterns.Call(o, mi, a) ->
                                                        if mi.DeclaringType.GetCustomAttribute<VectorTypeAttribute>() <> null && mi.Name = "pown" then
                                                            let fa = continuation(a.[0])
                                                            let sa = continuation(a.[1])
                                                            Value(<@ 1.0f + %fa + %sa @>)
                                                        else                                            
                                                            Continue
                                                    | _ ->
                                                        Continue),
                                                 false)  

        let scount, ph = ExpressionCounter.Count(m.Kernel.OriginalBody,
                                                 parameters,
                                                 (fun (e, parameters, continuation) ->
                                                    match e with
                                                    | DerivedPatterns.SpecificCall <@ System.Math.Sqrt @> (e, t, a) ->
                                                        let fa = continuation(a.[0])
                                                        Value(<@ 1.0f + %fa @>)
                                                    | _ ->
                                                        Continue),
                                                 false)    

        let hcount, ph = ExpressionCounter.Count(m.Kernel.OriginalBody,
                                                 parameters,
                                                 (fun (e, parameters, continuation) ->
                                                    match e with
                                                    | Patterns.Call(o, mi, a) ->
                                                        if mi.DeclaringType.GetCustomAttribute<VectorTypeAttribute>() <> null && mi.Name = "hypot" then
                                                            let fa = continuation(a.[0])
                                                            let sa = continuation(a.[1])
                                                            Value(<@ 1.0f + %fa + %sa @>)
                                                        else                                            
                                                            Continue
                                                    | _ ->
                                                        Continue),
                                                 false)       
                                              
        ([ box acount; box lcount; box bcount; box ccount; box pcount; box scount; box hcount ], ph |> List.ofSeq) :> obj