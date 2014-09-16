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

[<AbstractClass>]
type IFeatureExtractionTrainingSample() =
    abstract member TrainingSampleID: string with get
    abstract member ResultColumnIDs: string list with get
    abstract member Verify: obj * obj -> bool
    abstract member CreateVerifiedOutput: obj -> obj
    abstract member DefaultConfiguration: unit -> XDocument
    abstract member Configuration: unit -> XDocument
    abstract member RunInternal: FeatureExtractionChain * XDocument * bool -> List<List<obj>> * List<List<obj>>

    member this.Run(fec:FeatureExtractionChain, globalDataPath: string option, extractFeaturesOnly: bool) =
        let conf = this.Configuration()
        let wr = new StreamWriter(this.TrainingSampleID + "_Features.csv", false)
        let gwr = if globalDataPath.IsSome then
                    new StreamWriter(globalDataPath.Value, true)
                  else
                    null
        let fnl = 
            if not extractFeaturesOnly then
                String.concat ";" (this.ResultColumnIDs @ fec.FeatureNameList)
            else
                String.concat ";" (fec.FeatureNameList)                
        wr.WriteLine(fnl)
        let resultList, featureList = this.RunInternal(fec, conf, extractFeaturesOnly)
        // Check cols
        for r in featureList do
            if r.Count <> fec.FeatureNameList.Length then
                failwith "Error"
        for r = 0 to featureList.Count - 1 do
            if not extractFeaturesOnly then
                for c = 0 to resultList.[r].Count - 1 do
                    let item = resultList.[r].[c]
                    wr.Write(item.ToString() + ";")  
                    if gwr <> null then
                        gwr.Write(item.ToString() + ";")          
            for c = 0 to featureList.[r].Count - 2 do
                let item = featureList.[r].[c]
                wr.Write(item.ToString() + ";")
                if gwr <> null then
                    gwr.Write(item.ToString() + ";")   
            wr.Write(featureList.[r].Last().ToString() + wr.NewLine)
            if gwr <> null then
                gwr.Write(featureList.[r].Last().ToString() + gwr.NewLine)  
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
