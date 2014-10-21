namespace FSCL.Runtime.Scheduling

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

[<AbstractClass>]
type IFeatureExtractor<'INPUT,'FTYPE>() =
    abstract FeatureIDs: string list
    abstract BuildFinalizers: 'INPUT -> obj list
    abstract EvaluateFinalizers: 'INPUT * obj list * obj list * IReadOnlyDictionary<string, obj> -> 'FTYPE list

[<AbstractClass>]
type IDefaultFeatureExtractor<'INPUT,'FTYPE>() = 
    inherit IFeatureExtractor<'INPUT,'FTYPE>()
    override this.EvaluateFinalizers(kmodule, finalizers, args, opts) =
        // Default evaluation of precomputed features consists in
        // evaluating the expression to obtain a function to then apply using the current args
        //let dynDefPlaceholders = kmodule.DynamicDefineVars
        let featureNames = this.FeatureIDs
        let features = List.mapi(fun i (evaluator:obj) ->
                                    // Now we can apply the evaluator to obtain the value of the feature using actual args
                                    let mutable fv = evaluator
                                    for a in args do
                                        fv <- fv.GetType().GetMethod("Invoke").Invoke(fv, [| a |])
                                    //System.Console.WriteLine(fv.ToString())
                                    fv :?> 'FTYPE) finalizers
        features

type FeatureExtractorSet<'INPUT,'FTYPE>(feat: IFeatureExtractor<'INPUT,'FTYPE> list) =
    member this.FeatureIDs
        with get() =
            [ for f in feat do
                yield! f.FeatureIDs ]
    
    member this.BuildFinalizers(input: 'INPUT) =
        [ for f in feat do
            yield f.BuildFinalizers(input) ]

    member this.EvaluateFinalizers(input: 'INPUT, finalizers, args, opts) =
        [ for f in feat do
            yield! f.EvaluateFinalizers(input, finalizers, args, opts) ]
