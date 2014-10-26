namespace FSCL.Runtime.Scheduling.FRTSchedulingEngine

open System
open MathNet
open MathNet.Numerics
open FSCL
open FSCL.Compiler
open System.Collections.Generic
open System.IO
open System.Xml
open System.Xml.Serialization
open System.Xml.Linq
open FSCL.Runtime.Scheduling
open Microsoft.FSharp.Quotations
open System.Reflection
open FSCL.Runtime.Scheduling.FRTSchedulingEngine.TrainingSamples

[<AllowNullLiteral>]
type FRTConfiguration() =
    let mutable (featureExtractors:FRTFeatureExtractorSet) = null
    let mutable (trainingSamples:FRTFeatureExtractionTrainingSampleSet) = null

    member this.FeatureExtractorSet
        with get() =
            featureExtractors
        and set(v) =
            featureExtractors <- v

    member this.TrainingSampleSet 
        with get() =
            trainingSamples
        and set(v) =
            trainingSamples <- v

type FRTProfilingData() =
    let mutable (features:float32[,]) = array2D [[]] 
    let mutable (times:float32[,]) = array2D [[]]

    member this.Features 
        with get() =
            features
        and set(v) =
            features <- v

    member this.Times 
        with get() =
            times
        and set(v) =
            times <- v

[<AllowNullLiteral>]
type FRTRegressionData() =
    let mutable (regressionData:Dictionary<string, float32[]>) = new Dictionary<string, float32[]>()

    member this.RegressionData 
        with get() =
            regressionData
        and set(v) =
            regressionData <- v

type FRTSchedulingEngine(feat: FRTFeatureExtractor[], 
                         samples: FRTFeatureExtractionTrainingSample[], 
                         loadDefault: bool,
                         runtimeRun: obj -> obj) = 
    inherit ISchedulingEngine<IKernelModule, obj[]>()

    let mutable regressionData = null
    let mutable devices = null
    let mutable configuration = null    
    do
        FRTSchedulingEngine.CheckOrCreateConfigurationEnvironment() 
        configuration <- FRTSchedulingEngine.CreateConfiguration(feat, samples, loadDefault)
        
    static member private CheckOrCreateConfigurationEnvironment() =
        if not (Directory.Exists(FRTSchedulingEngine.defConfRoot)) then
            Directory.CreateDirectory(FRTSchedulingEngine.defConfRoot) |> ignore            
        if not (Directory.Exists(FRTSchedulingEngine.defCompRoot)) then
            Directory.CreateDirectory(FRTSchedulingEngine.defCompRoot) |> ignore 

    static member private LoadComponentsFromAssembly(assembly:Assembly) =
        let featureExtractors = new List<FRTFeatureExtractor>()
        let samples = new List<FRTFeatureExtractionTrainingSample>()
        for t in assembly.GetTypes() do
            if t.GetCustomAttribute<FRTFeatureExtractorAttribute>() <> null then
                // Instantiate feature extractor
                let extr = t.GetConstructor([||]).Invoke([||]) :?> FRTFeatureExtractor
                featureExtractors.Add(extr)
            else if t.GetCustomAttribute<FRTFeatureExtractionTrainingSampleAttribute>() <> null then
                // Instantiate training sample
                let sampl = t.GetConstructor([||]).Invoke([||]) :?> FRTFeatureExtractionTrainingSample
                samples.Add(sampl)
        featureExtractors, samples  
        
    static member private CreateConfiguration(feat: FRTFeatureExtractor list, 
                                              samples: FRTFeatureExtractionTrainingSample[],
                                              mergeWithDefault: bool) =
        let defFeat, defSamples = 
            if mergeWithDefault then
                FRTSchedulingEngine.LoadComponentsFromAssembly(typeof<ConvolutionTrainingSample>.Assembly)
            else
                new List<FRTFeatureExtractor>(), new List<FRTFeatureExtractionTrainingSample>()
        for f in feat do
            if (defFeat |> Seq.tryFind(fun item -> item.GetType() = f.GetType())).IsNone then
                defFeat.Add(f)
        for s in samples do
            if (defSamples |> Seq.tryFind(fun item -> item.GetType() = s.GetType())).IsNone then
                defSamples.Add(s)
        let conf = new FRTConfiguration()
        conf.FeatureExtractorSet <- new FRTFeatureExtractorSet(defFeat |> Array.ofSeq|> box |> unbox)
        conf.TrainingSampleSet <- new FRTFeatureExtractionTrainingSampleSet(defSamples |> Array.ofSeq |> box |> unbox)
        conf
                
    // Configuration directories and files
    static member private defConfRoot = Path.Combine(Environment.GetFolderPath(
                                                        Environment.SpecialFolder.LocalApplicationData), 
                                                     "FSCL.Runtime", 
                                                     "Scheduling",
                                                     "FRTSchedulingEngine")
                                                     
    static member private defCompRoot = Path.Combine(Environment.GetFolderPath(
                                                         Environment.SpecialFolder.LocalApplicationData), 
                                                         "FSCL.Runtime", 
                                                         "Scheduling",
                                                         "FeretSchedulingEngine",
                                                         "Components")
                                                                                                                  
    static member private profilingDataFile = Path.Combine(FRTSchedulingEngine.defConfRoot,
                                                           "ProfilingData.xml")

    static member private regressionDataFile = Path.Combine(FRTSchedulingEngine.defConfRoot,
                                                            "RegressionData.xml")
                                                            
    static member private configurationFile = Path.Combine(FRTSchedulingEngine.defConfRoot,
                                                           "Configuration.xml")


    new(file: string, runtimeRun: obj -> obj) as this =
        FRTSchedulingEngine([||], [||], false, runtimeRun)
        then
            let el = XDocument.Load(file)  
            let loadDefault = 
                let attr = el.Root.Attributes(XName.Get("LoadDefault")) |> Seq.toList
                if attr.Length > 0 then
                    bool.Parse(attr.[0].Value)
                else
                    true
            let extractors = new List<FRTFeatureExtractor>()
            for f in el.Elements(XName.Get("FeatureExtractors")) do
                let typeName = f.Attribute(XName.Get("Type")).Value
                let assemblyPath = 
                    Path.GetFullPath(f.Attribute(XName.Get("AssemblyPath")).Value)
                let configurationPath =                     
                    let attr = el.Root.Attributes(XName.Get("ConfigurationPath")) |> Seq.toList
                    if attr.Length > 0 then
                        Some(Path.GetFullPath(attr.[0].Value))
                    else
                        None
                let assembly = Assembly.LoadFile(assemblyPath)
                let typ = assembly.GetType(typeName)
                let extractor = Activator.CreateInstance(typ) :?> FRTFeatureExtractor
                if configurationPath.IsSome then
                    extractor.ConfigureFromXml(XDocument.Load(configurationPath.Value).Root)
                extractors.Add(extractor)
            
            let samples = new List<FRTFeatureExtractionTrainingSample>()
            for f in el.Elements(XName.Get("TrainingSamples")) do
                let typeName = f.Attribute(XName.Get("Type")).Value
                let assemblyPath = 
                    Path.GetFullPath(f.Attribute(XName.Get("AssemblyPath")).Value)
                let configurationPath =                     
                    let attr = el.Root.Attributes(XName.Get("ConfigurationPath")) |> Seq.toList
                    if attr.Length > 0 then
                        Some(Path.GetFullPath(attr.[0].Value))
                    else
                        None
                let assembly = Assembly.LoadFile(assemblyPath)
                let typ = assembly.GetType(typeName)
                let sample = Activator.CreateInstance(typ) :?> FRTFeatureExtractionTrainingSample
                if configurationPath.IsSome then
                    sample.ConfigureFromXml(XDocument.Load(configurationPath.Value).Root)
                samples.Add(sample)

            this.Configuration <- FRTSchedulingEngine.CreateConfiguration(extractors |> Seq.toArray,
                                                                          samples |> Seq.toArray,
                                                                          loadDefault)

    new(runtimeRun) =
        // Load default components 
        let featureExtractors, samples = 
            new List<FRTFeatureExtractor>(), new List<FRTFeatureExtractionTrainingSample>()
        
        // Check if there is a configuration file
        if not (File.Exists(FRTSchedulingEngine.configurationFile)) then
            if Directory.Exists(FRTSchedulingEngine.defCompRoot) then
                // Load samples and features from directories
                for dll in Directory.GetFiles(FRTSchedulingEngine.defCompRoot, "*.dll") do
                    let assembly = Assembly.LoadFile(dll)
                    let newFeat, newSampl = FRTSchedulingEngine.LoadComponentsFromAssembly(assembly)
                    featureExtractors.AddRange(newFeat)
                    samples.AddRange(newSampl)

            // Now create engine
            FRTSchedulingEngine(featureExtractors |> Array.ofSeq, samples |> Array.ofSeq, true, runtimeRun)
        else
            FRTSchedulingEngine(FRTSchedulingEngine.configurationFile, runtimeRun)

    member this.Configuration
        with get() =
            configuration
        and private set v =
            configuration <- v

    member this.DumpConf() =
        let serializer = new XmlSerializer(typeof<FRTConfiguration>)
        let stream = new FileStream(FRTSchedulingEngine.configurationFile, FileMode.OpenOrCreate)
        let conf = serializer.Serialize(stream,configuration)
        ()

    member private this.ComputeRegressionData(features: float32[,], times: float32[,]) =
        let X = LinearAlgebra.Single.DenseMatrix.OfArray(features)
        let y = LinearAlgebra.Single.DenseMatrix.OfArray(times)
        let svd = X.Svd(true)
        let W = svd.W
        let s = svd.S

        let tolerance = (2.0e-6f) * (float32)(Math.Max(X.RowCount, X.ColumnCount)) * W.[0, 0]
        for i = 0 to s.Count - 1 do
            if s.[i] < tolerance then
                s.[i] <- 0.0f
            else
                s.[i] <- 1.0f / s.[i]
        W.SetDiagonal(s)

        // (U * W * VT)T is equivalent with V * WT * UT 
        (svd.U * W * svd.VT).Transpose().Multiply(y).ToArray()

    member this.CurrentDevices
        with get() =
            devices
        and private set v =
            devices <- v    

    override this.OnRuntimeLoad(devices: OpenCLDeviceSet) =
        // Store devices 
        this.CurrentDevices <- devices

        // Here regressionData should be null
        if regressionData = null then
            // Check if already did regression               
            if File.Exists(FRTSchedulingEngine.regressionDataFile) then
                // Load data
                let serializer = new XmlSerializer(typeof<FRTRegressionData>)
                use stream = new FileStream(FRTSchedulingEngine.regressionDataFile, FileMode.Open) 
                regressionData <- serializer.Deserialize(stream) :?> FRTRegressionData
            else
                // Check if platform profiled
                let profilingData =
                    if File.Exists(FRTSchedulingEngine.profilingDataFile) then
                        // Load data
                        let serializer = new XmlSerializer(typeof<FRTProfilingData>)
                        use stream = new FileStream(FRTSchedulingEngine.profilingDataFile, FileMode.Open) 
                        serializer.Deserialize(stream) :?> FRTProfilingData
                     else
                        // Must profile     
                        let opts = new Dictionary<string, obj>()
                        opts.Add("RunningMode", TrainingSampleRunningMode.FeaturesAndExecutionTime)
                        opts.Add("RuntimeRun", runtimeRun)

                        let featureValues, times = configuration.TrainingSampleSet.Run(configuration.FeatureExtractorSet, devices, opts) |>
                                                   Array.reduce (Array.append) |> 
                                                   Array.unzip ||> 
                                                   (fun feat time ->
                                                        (Array2D.init feat.Length feat.[0].Length (fun r c -> feat.[r].[c])), 
                                                        (Array2D.init time.Length time.[0].Length (fun r c -> time.[r].[c])))
                        new FRTProfilingData(Features = featureValues, Times = times)

                // Save profiling data
                let serializer = new XmlSerializer(typeof<FRTProfilingData>)
                use stream = new FileStream(FRTSchedulingEngine.profilingDataFile, FileMode.Create) 
                serializer.Serialize(stream, profilingData)

                // Do regression
                let regressionData = this.ComputeRegressionData(profilingData.Features, profilingData.Times)
                
                // Save regression data
                let serializer = new XmlSerializer(typeof<FRTRegressionData>)
                use stream = new FileStream(FRTSchedulingEngine.regressionDataFile, FileMode.Create) 
                serializer.Serialize(stream, regressionData)

    override this.OnKernelCompile(k: IKernelModule) =
        configuration.FeatureExtractorSet.BuildFinalizers(k)

    override this.OnKernelRun(k, finalizers, args, opts) =
        let fv = configuration.FeatureExtractorSet.EvaluateFinalizers(k, finalizers, args)
        OpenCL.OpenCLPlatform.Platforms.[0].Devices.[0]
            


    