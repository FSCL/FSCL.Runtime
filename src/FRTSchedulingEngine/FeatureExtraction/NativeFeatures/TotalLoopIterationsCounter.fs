namespace FSCL.Runtime.Scheduling.FRTSchedulingEngine.FeatureExtraction

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
open FSCL.Runtime.Scheduling.FRTSchedulingEngine

[<FRTFeatureExtractor("LoopIterationCounter")>]
type TotalLoopIterationsCounter() = 
    inherit FRTDefaultFeatureExtractor() 
    
    override this.FeatureIDs
        with get() =
            [ "Loop iterations" ]

    override this.BuildFinalizers(m: IKernelModule) =
        // Count loop iters
        let parameters = m.Kernel.OriginalParameters |> 
                         Seq.map(fun (p: IOriginalFunctionParameter) -> 
                                    (p.OriginalParamterInfo, p.OriginalPlaceholder)) |>
                         Array.ofSeq               
                                            
        let lcount = LoopIterationCounter.Count(m, m.Kernel)

        // Build lambda expr 
        [LeafExpressionConverter.EvaluateQuotation(lcount)] |> box

                