namespace FSCL.Runtime.Scheduling

open FSCL.Compiler
open FSCL.Language
open System.Collections.Generic
open System
open System.IO
open FSCL.Runtime
open System.Diagnostics
open System.Xml
open System.Xml.Linq
open System.Linq
open Microsoft.FSharp.Data
open System.Reflection
open System.Collections.ObjectModel

type TrainingSampleRunningMode =
| OnlyFeatures
| OnlyExecutionTime
| FeaturesAndExecutionTime

type OpenCLDeviceSet = ReadOnlyCollection<int * string * ReadOnlyCollection<int * string * DeviceType>>
    
type IFeatureExtractionTrainingSample =
    abstract member Run: IFeatureExtractorSet * OpenCLDeviceSet * IReadOnlyDictionary<string, obj> -> obj

[<AbstractClass>]
type FeatureExtractionTrainingSample<'FIN,'FOUT,'RUNOUT>() =
    interface IFeatureExtractionTrainingSample with
        member this.Run(features, devices, rm) =
            this.Run(features :?> FeatureExtractorSet<'FIN, 'FOUT>, devices, rm) :> obj
    
    abstract member Run: FeatureExtractorSet<'FIN, 'FOUT> * OpenCLDeviceSet * IReadOnlyDictionary<string, obj> -> 'RUNOUT
    
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
  
[<AllowNullLiteral>]    
type IFeatureExtractionTrainingSampleSet =
    abstract member TrainingSamples: IFeatureExtractionTrainingSample[] with get
    abstract member Run: IFeatureExtractorSet * OpenCLDeviceSet * IReadOnlyDictionary<string, obj> -> obj
   
[<AllowNullLiteral>] 
type FeatureExtractionTrainingSampleSet<'FIN,'FOUT,'RUNOUT>(samples: FeatureExtractionTrainingSample<'FIN,'FOUT,'RUNOUT>[]) =
    interface IFeatureExtractionTrainingSampleSet with
        member this.TrainingSamples
            with get() =
                samples |> Array.map(fun i -> i :> IFeatureExtractionTrainingSample)
        member this.Run(features, devices, opt) =
            this.Run(features :?> FeatureExtractorSet<'FIN,'FOUT>, devices, opt) :> obj

    new () =
        FeatureExtractionTrainingSampleSet<'FIN,'FOUT,'RUNOUT>([||])
        
    member this.Run(features, devices, mode:IReadOnlyDictionary<string, obj>) =
        samples |> Array.map(fun s -> s.Run(features, devices, mode))
