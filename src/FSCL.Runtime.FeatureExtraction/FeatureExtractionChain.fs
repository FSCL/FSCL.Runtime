namespace FSCL.Runtime.Scheduling.FeatureExtraction

open System
open FSCL.Compiler
open FSCL.Compiler.Util
open Microsoft.FSharp.Quotations
open System.Collections.Generic 

type FeatureExtractionChain(extractors: IFeatureExtractor[]) = 
    member this.FeatureNameList 
        with get() =
            let mutable l = []
            for e in extractors do
                l <- List.append l (e.FeatureNameList)
            l

    member this.Precompute(m: IKernelModule) =
        let precomputed = seq {
                            for extractor in extractors do
                                yield (extractor.Precompute(m))
                             }
        Array.ofSeq precomputed

    member this.Evaluate(m, precomputed: obj[], args: obj list, globalSize: int64[], localSize: int64[], opts: Dictionary<string ,obj>) =
        let mutable fl = []
        for i = 0 to extractors.Length - 1 do
            fl <- fl @ extractors.[i].Evaluate(m, precomputed.[i], args, globalSize, localSize, opts)
        fl
    