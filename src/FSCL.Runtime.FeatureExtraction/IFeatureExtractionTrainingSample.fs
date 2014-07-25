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
    abstract member RunInternal: FeatureExtractionChain * XDocument -> obj list list

    member this.Run(fec:FeatureExtractionChain) =
        let conf = this.Configuration()
        let wr = new StreamWriter(this.TrainingSampleID + "_Features.csv", false)
        let fnl = String.concat ";" (this.ResultColumnIDs @ fec.FeatureNameList)
        wr.WriteLine(fnl)
        let flist = this.RunInternal(fec, conf)
        for row in flist do
            let rowList = String.concat ";" (List.map (fun a -> a.ToString()) row)
            wr.WriteLine(rowList)
        wr.Close()

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
