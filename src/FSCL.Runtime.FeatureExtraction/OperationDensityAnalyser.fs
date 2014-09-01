namespace FSCL.Runtime.Scheduling.FeatureExtraction

open FSCL.Compiler
open FSCL.Runtime.Scheduling
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open System.Reflection
open Microsoft.FSharp.Linq.RuntimeHelpers
//open QuotEval.QuotationEvaluation
open System
open FSCL.Runtime
open FSCL.Compiler.Util
open FSCL.Compiler.AcceleratedCollections

type OperationDensityItem(t: int, w: float32, c: float32) =
    member val Type = t with get, set
    member val Count = c with get, set
    member val Weight = w with get, set

type OperationDensityAnalyser() = 
    inherit IFeatureExtractor()
    override this.FeatureNameList 
        with get() =
            [ "Average ops between memory accesses" ]

    override this.Precompute(m: IKernelModule) =
        // Count loop iters
        let parameters = m.Kernel.OriginalParameters |> 
                         Seq.map(fun (p: IOriginalFunctionParameter) -> 
                                    (p.OriginalParamterInfo, p.OriginalPlaceholder)) |>
                         Array.ofSeq               
                                            
        let lcount, ph = OperationDensityCounter.Count(m.Kernel.OriginalBody, parameters)

        // Build lambda expr 
        (lcount, (ph |> List.ofSeq)) :> obj
        
    override this.Evaluate(m, prec, args, opts) =
        
        let precFeatures, dynDefPlaceholders = prec :?> (OperationTraceItem list * Var list)
        let featureNames = this.FeatureNameList
        let features = new List<OperationDensityItem>()
        for evaluablePrecomputedFeature in precFeatures do
            match evaluablePrecomputedFeature with
                | MemoryAccess(weight) ->
                    let wevaluator = LeafExpressionConverter.EvaluateQuotation(weight)
                    let constantsDefines = if opts.ContainsKey(RuntimeOptions.ConstantDefines) then
                                                Some(opts.[RuntimeOptions.ConstantDefines] :?> (string * obj) list)
                                            else
                                                None
                    let dynDefArgs = seq {
                                        for v in dynDefPlaceholders do
                                            let cdef = if constantsDefines.IsSome then constantsDefines.Value |> List.tryFind(fun (s, o) -> s = v.Name) else None
                                            if cdef.IsSome then
                                                yield snd(cdef.Value)
                                        } |> List.ofSeq     
                                               
                    // Evaluate weight
                    let mutable fwv = wevaluator
                    for a in args do
                        fwv <- fwv.GetType().GetMethod("Invoke").Invoke(fwv, [| a |])
                    for d in dynDefArgs do
                        fwv <- fwv.GetType().GetMethod("Invoke").Invoke(fwv, [| d |])
                    features.Add(new OperationDensityItem(0, fwv :?> float32, 0.0f))
                | OpSequence(weight, count) ->
                    let wevaluator = LeafExpressionConverter.EvaluateQuotation(weight)
                    let cevaluator = LeafExpressionConverter.EvaluateQuotation(count)

                    let constantsDefines = if opts.ContainsKey(RuntimeOptions.ConstantDefines) then
                                                Some(opts.[RuntimeOptions.ConstantDefines] :?> (string * obj) list)
                                            else
                                                None
                    let dynDefArgs = seq {
                                        for v in dynDefPlaceholders do
                                            let cdef = if constantsDefines.IsSome then constantsDefines.Value |> List.tryFind(fun (s, o) -> s = v.Name) else None
                                            if cdef.IsSome then
                                                yield snd(cdef.Value)
                                        } |> List.ofSeq     
                                               
                    // Evaluate weight
                    let mutable fwv = wevaluator
                    for a in args do
                        fwv <- fwv.GetType().GetMethod("Invoke").Invoke(fwv, [| a |])
                    for d in dynDefArgs do
                        fwv <- fwv.GetType().GetMethod("Invoke").Invoke(fwv, [| d |])
                                            
                    // Evaluate count
                    let mutable fcv = cevaluator
                    for a in args do
                        fcv <- fcv.GetType().GetMethod("Invoke").Invoke(fcv, [| a |])
                    for d in dynDefArgs do
                        fcv <- fcv.GetType().GetMethod("Invoke").Invoke(fcv, [| d |])
                    features.Add(new OperationDensityItem(1, fwv :?> float32, fcv :?> float32))

        // Insert a void op between consecutive mem accesses AND
        // Merge ops with the same weights
        let mutable i = 0
        while i < features.Count - 1 do
            if features.[i].Type = 0 && features.[i + 1].Type = 0 then
                features.Insert(i + 1, new OperationDensityItem(1, features.[i + 1].Weight, 0.0f))    
            else if features.[i].Type = 1 && features.[i + 1].Type = 1 then
                if features.[i].Weight = features.[i + 1].Weight then
                    let newItem = new OperationDensityItem(1, features.[i + 1].Weight, features.[i].Count + features.[i + 1].Count)
                    features.RemoveAt(i)
                    features.RemoveAt(i)
                    features.Insert(i, newItem)
                    i <- i - 1  
            i <- i + 1
        [] |> List.ofSeq
                