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

type TotalLoopIterationsCounter() = 
    inherit IDefaultFeatureExtractor()
    override this.FeatureNameList 
        with get() =
            [ "Loop iterations" ]

    override this.Precompute(m: IKernelModule) =
        // Count loop iters
        let parameters = m.Kernel.OriginalParameters |> 
                         Seq.map(fun (p: IOriginalFunctionParameter) -> 
                                    (p.OriginalParamterInfo, p.OriginalPlaceholder)) |>
                         Array.ofSeq               
                                            
        let lcount, ph = LoopIterationCounter.Count(m.Kernel.OriginalBody, parameters)

        // Build lambda expr 
        ([box lcount], (ph |> List.ofSeq)) :> obj

                