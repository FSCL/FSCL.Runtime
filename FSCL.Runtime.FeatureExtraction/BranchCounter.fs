namespace FSCL.Runtime.Scheduling.FeatureExtraction

open FSCL.Compiler
open FSCL.Runtime.Scheduling
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open System.Reflection
open Microsoft.FSharp.Linq.RuntimeHelpers
//open QuotEval.QuotationEvaluation
open System
open FSCL.Compiler.Util
open FSCL.Compiler.AcceleratedCollections

type BranchCounter() = 
    inherit IDefaultFeatureExtractor()
    override this.FeatureNameList 
        with get() =
            [ "Branch count (per thread)"]
    override this.Precompute(m: IKernelModule) =
        // Count branches
        let parameters = m.Kernel.OriginalParameters |> 
                         Seq.map(fun (p: IOriginalFunctionParameter) -> 
                                    (p.OriginalParamterInfo, p.OriginalPlaceholder)) |>
                         Array.ofSeq               
                                            
        let count, ph = ExpressionCounter.Count(m.Kernel.OriginalBody,
                                                parameters,
                                                (fun (e:Expr, parameters:(ParameterInfo * Var) array, continuation) ->
                                                    match e with
                                                    | Patterns.IfThenElse(cond, ifb, elseb) ->
                                                        let ifc = continuation(ifb)
                                                        let elsec = continuation(elseb)
                                                        Value(<@ 1.0f + (0.5f * %ifc) + (0.5f * %elsec) @>)
                                                    | _ ->
                                                        Continue),
                                                false)
        // Build lambda expr 
        ([box count], (ph |> List.ofSeq)) :> obj

                