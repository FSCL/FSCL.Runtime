namespace FSCL.Runtime.Scheduling.FeatureExtraction

open FSCL
open FSCL.Compiler
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Linq.RuntimeHelpers
open FSCL.Runtime.Scheduling.ReflectionUtil
open FSCL.Compiler.AcceleratedCollections
open System
open FSCL.Language
open System.Collections.Generic
open FSCL.Runtime

type FeatureList = obj list

[<AbstractClass>]
type IFeatureExtractor() =
    abstract FeatureNameList: string list
    abstract Precompute: IKernelModule -> obj
    abstract Evaluate: IKernelModule * obj * obj list * Dictionary<string, obj> -> FeatureList

[<AbstractClass>]
type IDefaultFeatureExtractor() = 
    inherit IFeatureExtractor()
    override this.Evaluate(m, pfl: obj, args, opts) =
        // Default evaluation of precomputed features consists in
        // evaluating the expression to obtain a function to then apply using the current args
        let precFeatures, dynDefPlaceholders = pfl :?> (obj list * Var list)
        let featureNames = this.FeatureNameList
        let features = List.mapi(fun i (evaluablePrecomputedFeature:obj) ->
                                    // Then we evaluate the expr to get a function
                                    //let prc = (evaluablePrecomputedFeature :?> Expr).GetFreeVars()
                                    let evaluator = LeafExpressionConverter.EvaluateQuotation(evaluablePrecomputedFeature :?> Expr)
                                    // Add work size function values to original arguments
                                    // Add dynamic defines additional arguments
                                   (*
                                    <@ 
                                        let mutable tripCount = 0.0f 
                                        let mutable offset = 2
                                        while offset > 0 do 
                                            offset <- offset >>> 2
                                            tripCount <- tripCount + 1.0f
                                        tripCount
                                    @> *)
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
                                                             
                                    // We build a tupledArg out fot the args array
                                    //let tupledArg = ToTuple((args @ workSizeArgs @ dynDefArgs) |> List.toArray)
                                    // NO MORE, we use curried functions

                                    // Handling accelerated collections
                                                 (*                       
                                    // If staged reduction
                                    let isAccelerateReduce = (m.Kernel :? AcceleratedKernelInfo) && 
                                                                m.Kernel.CustomInfo.ContainsKey("ReduceFunction")
                                    if isAccelerateReduce then
                                        // Staged iterations, check how many
                                        let mutable totalSum = evaluator.GetType().GetMethod("Invoke").Invoke(evaluator, [| tupledArg |])
                                        let mutable outputSize = globalSize.[0] / localSize.[0]
                                        while outputSize > localSize.[0] then
                                            let tupledArg = ToTuple()
                                    // Evaluating 
                                    *)

                                    // Now we can apply the evaluator to obtain the value of the feature using actual args
                                    let mutable fv = evaluator
                                    for a in args do
                                        fv <- fv.GetType().GetMethod("Invoke").Invoke(fv, [| a |])
                                    //fv <- fv.GetType().GetMethod("Invoke").Invoke(fv, [| workSizeArgs.[0] |])
                                    for d in dynDefArgs do
                                        fv <- fv.GetType().GetMethod("Invoke").Invoke(fv, [| d |])
                                    //System.Console.WriteLine(fv.ToString())
                                    fv) precFeatures
        features
