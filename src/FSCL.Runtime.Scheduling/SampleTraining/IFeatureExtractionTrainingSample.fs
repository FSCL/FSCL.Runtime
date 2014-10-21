namespace FSCL.Runtime.Scheduling.FeatureExtraction

open FSCL.Compiler
open FSCL.Runtime.Scheduling
open FSCL.Runtime.Scheduling.FeatureExtraction
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

type TrainingSampleRunningMode =
| OnlyFeatures
| OnlyExecutionTime
| FeaturesAndExecutionTime

[<AbstractClass>]
type IFeatureExtractionTrainingSample() =
    abstract member TrainingSampleID: string with get
    abstract member ResultColumnIDs: string list with get
    abstract member Verify: obj * obj -> bool
    abstract member CreateVerifiedOutput: obj -> obj
    abstract member DefaultConfiguration: unit -> XDocument
    abstract member Configuration: unit -> XDocument
    abstract member RunInternal: FeatureExtractionChain * XDocument * TrainingSampleRunningMode -> List<List<obj>> * List<List<obj>>

    member this.Run(index: int option, fec:FeatureExtractionChain, globalDataPath: string option, runningMode: TrainingSampleRunningMode) =
        let conf = this.Configuration()
        let wr = new StreamWriter(this.TrainingSampleID + "_Features.csv", true)
        let gwr = if globalDataPath.IsSome then
                    new StreamWriter(globalDataPath.Value, true)
                  else
                    null
        // Create data header
        let mutable fnl = []
        if runningMode <> TrainingSampleRunningMode.OnlyExecutionTime then
            fnl <- fnl @ fec.FeatureNameList
        if runningMode <> TrainingSampleRunningMode.OnlyFeatures then
            fnl <- fnl @ this.ResultColumnIDs

        let header = 
            if index.IsSome then
                String.concat ";" ([ "Training sample index" ] @ fnl)
            else
                String.concat ";" fnl
                       
        wr.WriteLine(header)
        if gwr <> null then
            gwr.WriteLine(header) 

        // Run sample
        let resultList, featureList = this.RunInternal(fec, conf, runningMode)

        // Check cols
        for r in featureList do
            if r.Count <> fec.FeatureNameList.Length then
                failwith "Error"

        // Write data
        for r = 0 to featureList.Count - 1 do
            let rowValues =
                match runningMode with            
                | TrainingSampleRunningMode.OnlyFeatures ->
                   featureList.[r] |> Seq.toList
                | TrainingSampleRunningMode.OnlyExecutionTime ->
                   resultList.[r] |> Seq.toList
                | _ ->
                   (featureList.[r] |> Seq.toList) @ (resultList.[r] |> Seq.toList)
            let rowValuesWithIndex =
                if index.IsSome then
                    [ box index.Value ] @ rowValues
                else
                    rowValues
            let toPrint = rowValuesWithIndex |> List.map(fun i -> i.ToString()) |> String.concat ";"
            wr.WriteLine(toPrint)
            if gwr <> null then
                gwr.WriteLine(toPrint)  
        wr.Close()
        if (gwr <> null) then
            gwr.Close()

[<AbstractClass>]
type IDefaultFeatureExtractionTrainingSample() =
    inherit IFeatureExtractionTrainingSample()
            
    abstract member DefaultConfigurationDictionary: unit -> Dictionary<string, obj>

    override this.TrainingSampleID 
        with get() =
            this.GetType().Name   
            
    override this.DefaultConfiguration() =
        let dict = this.DefaultConfigurationDictionary()
        let q = query {
                        for kv in dict do
                            select (new XElement(XName.Get(kv.Key), kv.Value)) }
        let elem = new XElement(XName.Get(this.TrainingSampleID), q |> Array.ofSeq)
        new XDocument(elem)

    override this.Configuration() =
        if File.Exists(this.TrainingSampleID + ".xml") then
            let doc = XDocument.Load(this.TrainingSampleID + ".xml")     
            doc    
        else
            let doc = this.DefaultConfiguration()
            doc  

    static member ConfigurationToDictionary(c:XDocument) =
        let dict = new Dictionary<string, string>()
        for el in c.Elements().Descendants() do
           dict.Add(el.Name.LocalName, el.Value);
        dict
