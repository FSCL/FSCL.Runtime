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
open System.Xml
open System.Xml.Linq
open System.Reflection

[<AllowNullLiteral>]
[<AttributeUsage(System.AttributeTargets.Property)>]
type ConfigurationItemAttribute() =
    inherit Attribute()

type IFeatureExtractor =
    abstract FeatureIDs: string list with get
    abstract BuildFinalizers: obj -> obj 
    abstract EvaluateFinalizers: obj * obj * obj list -> obj[]
    
[<AbstractClass>]
type FeatureExtractor<'INPUT,'FTYPE>() =
    interface IFeatureExtractor with
        member this.EvaluateFinalizers(input, finalizers, args) =
            this.EvaluateFinalizers(input :?> 'INPUT, finalizers, args) |> Array.map(box)
        member this.FeatureIDs 
            with get() =
                this.FeatureIDs
        member this.BuildFinalizers(i) =
            this.BuildFinalizers(i :?> 'INPUT) 
    abstract FeatureIDs: string list with get
    abstract BuildFinalizers: 'INPUT -> obj 
    abstract EvaluateFinalizers: 'INPUT * obj * obj list -> 'FTYPE[]
    
    member this.ConfigureFromXml(el:XElement) =
        let properties = this.GetType().GetProperties() |> Array.filter(fun p -> p.GetCustomAttribute<ConfigurationItemAttribute>() <> null)
        for element in el.Elements() do
            let p = properties |> Array.tryFind(fun propInfo -> propInfo.Name = element.Name.LocalName)
            if p.IsSome then
                p.Value.SetValue(this, Convert.ChangeType(el.Value, p.Value.PropertyType))
                
    member this.ConfigurationToXml() =
        let properties = this.GetType().GetProperties() |> Array.filter(fun p -> p.GetCustomAttribute<ConfigurationItemAttribute>() <> null)
        let el = new XElement(XName.Get("FeatureExtractorConfiguration"))
        for p in properties do
            el.Add(new XElement(XName.Get(p.Name), p.GetValue(this).ToString()))
        el

[<AbstractClass>]
type DefaultFeatureExtractor<'INPUT,'FTYPE>() = 
    inherit FeatureExtractor<'INPUT,'FTYPE>()     

    override this.EvaluateFinalizers(input, finalizers, args) =
        // Default evaluation of precomputed features consists in
        // evaluating the expression to obtain a function to then apply using the current args
        //let dynDefPlaceholders = kmodule.DynamicDefineVars
        let featureNames = this.FeatureIDs
        let features = Array.mapi(fun i (evaluator:obj) ->
                                    // Now we can apply the evaluator to obtain the value of the feature using actual args
                                    let mutable fv = evaluator
                                    for a in args do
                                        fv <- fv.GetType().GetMethod("Invoke").Invoke(fv, [| a |])
                                    //System.Console.WriteLine(fv.ToString())
                                    fv :?> 'FTYPE) (finalizers :?> obj[])
        features


[<AllowNullLiteral>]
type IFeatureExtractorSet =
    abstract member FeatureIDs: string list with get
    abstract member BuildFinalizers: obj -> obj
    abstract member EvaluateFinalizers: obj * obj[] * obj list -> obj
   
[<AllowNullLiteral>]                 
type FeatureExtractorSet<'INPUT,'FTYPE>(feat: FeatureExtractor<'INPUT,'FTYPE>[]) =
    interface IFeatureExtractorSet with
        member this.FeatureIDs 
            with get() =
                [ for f in feat do
                    yield! f.FeatureIDs ]
        member this.BuildFinalizers(i) =
            this.BuildFinalizers(i :?> 'INPUT) :> obj
        member this.EvaluateFinalizers(input, finalizers, args) =
            this.EvaluateFinalizers(input :?> 'INPUT, finalizers, args) :> obj
    
    new() =
         FeatureExtractorSet<'INPUT,'FTYPE>([||])
             
    member this.BuildFinalizers(input: 'INPUT) =
        [| for f in feat do
            yield f.BuildFinalizers(input) |]

    member this.EvaluateFinalizers(input: 'INPUT, finalizers: obj[], args) =
        feat |> Array.mapi(fun i f -> f.EvaluateFinalizers(input, finalizers.[i], args)) |> Array.reduce (Array.append)
