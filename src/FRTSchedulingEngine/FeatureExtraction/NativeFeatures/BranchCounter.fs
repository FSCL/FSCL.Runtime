namespace FSCL.Runtime.Scheduling.FRTSchedulingEngine.FeatureExtraction

open FSCL.Compiler
open FSCL.Runtime.Scheduling
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open System.Reflection
open Microsoft.FSharp.Linq.RuntimeHelpers
open FSCL.Runtime.Scheduling.FRTSchedulingEngine
open System
open FSCL.Compiler.Util
open FSCL.Compiler.AcceleratedCollections

[<FRTFeatureExtractor("BranchCounter")>]
type BranchCounter() = 
    inherit FRTDefaultFeatureExtractor() 

    override this.FeatureIDs
        with get() =
            [ "Branch count" ]

    override this.BuildFinalizers(m: IKernelModule) =
        // Count branches
        let parameters = m.Kernel.OriginalParameters |> 
                         Seq.map(fun (p: IOriginalFunctionParameter) -> 
                                    (p.OriginalParamterInfo, p.OriginalPlaceholder)) |>
                         Array.ofSeq               
                                            
        let bcount = ExpressionCounter.Count(m.Kernel.OriginalBody,
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
        [ LeafExpressionConverter.EvaluateQuotation(bcount) ] |> box

                