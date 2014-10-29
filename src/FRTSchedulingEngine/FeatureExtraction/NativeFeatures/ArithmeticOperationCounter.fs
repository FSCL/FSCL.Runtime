namespace FSCL.Runtime.Scheduling.FRTSchedulingEngine.FeatureExtraction

open FSCL
open FSCL.Compiler
open FSCL.Runtime.Scheduling
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open System.Reflection
open Microsoft.FSharp.Linq.RuntimeHelpers
open FSCL.Compiler.Util
open FSCL.Language
open FSCL.Runtime.Scheduling.FRTSchedulingEngine

[<FRTFeatureExtractor("OperationCounter")>]
type ArithmeticOperationCounter() = 
    inherit FRTDefaultFeatureExtractor() 
    
    override this.FeatureIDs
        with get() =
            [ "Arithmetic operations (per thread)";
               "Logic operations (per thread)";
               "Bitwise operations (per thread)";
               "Comparison operations (per thread)";
               "Pown operations (per thread)";
               "Sqrt operations (per thread)";
               "Hypot operations (per thread)" ]
              
    override this.BuildFinalizers(m) =
        let parameters = m.Kernel.OriginalParameters |> 
                         Seq.map(fun (p: IOriginalFunctionParameter) -> 
                                    (p.OriginalParamterInfo, p.OriginalPlaceholder)) |>
                         Array.ofSeq   
        // Count ops
        let acount = ExpressionCounter.Count(m,
                                             m.Kernel,
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

        let lcount = ExpressionCounter.Count(m,
                                             m.Kernel,
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
                                                       
        let bcount = ExpressionCounter.Count(m,
                                             m.Kernel,
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
        
        let ccount = ExpressionCounter.Count(m,
                                             m.Kernel,
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
        
        let pcount = ExpressionCounter.Count(m,
                                             m.Kernel,
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

        let scount = ExpressionCounter.Count(m,
                                             m.Kernel,
                                             (fun (e, parameters, continuation) ->
                                                    match e with
                                                    | DerivedPatterns.SpecificCall <@ System.Math.Sqrt @> (e, t, a) ->
                                                        let fa = continuation(a.[0])
                                                        Value(<@ 1.0f + %fa @>)
                                                    | _ ->
                                                        Continue),
                                             false)    

        let hcount = ExpressionCounter.Count(m,
                                             m.Kernel,
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
                                              
        [  LeafExpressionConverter.EvaluateQuotation(acount); 
           LeafExpressionConverter.EvaluateQuotation(lcount);
           LeafExpressionConverter.EvaluateQuotation(bcount); 
           LeafExpressionConverter.EvaluateQuotation(ccount); 
           LeafExpressionConverter.EvaluateQuotation(pcount);
           LeafExpressionConverter.EvaluateQuotation(scount); 
           LeafExpressionConverter.EvaluateQuotation(hcount); ] |> box