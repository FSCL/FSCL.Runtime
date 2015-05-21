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

    member this.ToCsv(s:StreamWriter) =
        s.WriteLine(this.Features.GetLength(1).ToString() + ";" + this.Times.GetLength(1).ToString() + ";" + this.Features.GetLength(0).ToString())
        for r = 0 to this.Features.GetLength(0) - 1 do
            for c = 0 to this.Features.GetLength(1) - 1 do
                s.Write(this.Features.[r, c].ToString())
                if (c < this.Features.GetLength(1) - 1) then
                    s.Write(";")
            s.Write(s.NewLine)
        for r = 0 to this.Times.GetLength(0) - 1 do
            for c = 0 to this.Times.GetLength(1) - 1 do
                s.Write(this.Times.[r, c].ToString())
                if (c < this.Times.GetLength(1) - 1) then
                    s.Write(";")
            s.Write(s.NewLine)
        
    member this.FromCsv(s:StreamReader) =
        let head = s.ReadLine().Split([| ';' |])
        let featCols = Int32.Parse(head.[0])
        let timeCols = Int32.Parse(head.[1])
        let rows = Int32.Parse(head.[2])
        this.Features <- Array2D.zeroCreate<float32> rows featCols
        this.Times <- Array2D.zeroCreate<float32> rows timeCols

        let mutable r = 0
        while r < rows && not (s.EndOfStream) do
            let cols = s.ReadLine().Split([| ';' |])
            for c = 0 to cols.Length - 1 do
                this.Features.[r, c] <- Single.Parse(cols.[c])
            r <- r + 1            
        r <- 0
        while r < rows && not (s.EndOfStream) do
            let cols = s.ReadLine().Split([| ';' |])
            for c = 0 to cols.Length - 1 do
                this.Times.[r, c] <- Single.Parse(cols.[c])
            r <- r + 1

[<AllowNullLiteral>]
type FRTRegressionData() =
    let mutable (regressionData:Dictionary<string, float32[]>) = new Dictionary<string, float32[]>()

    member this.RegressionData 
        with get() =
            regressionData
        and set(v) =
            regressionData <- v

type FRTSchedulingEngine(feat: list<FRTFeatureExtractor>, 
                         samples: list<FRTFeatureExtractionTrainingSample>, 
                         loadDefault: bool,
                         runtimeRun: obj -> obj) = 
    inherit ISchedulingEngine<IKernelModule, obj list>()

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
                                              samples: FRTFeatureExtractionTrainingSample list,
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
        conf.FeatureExtractorSet <- new FRTFeatureExtractorSet(defFeat |> List.ofSeq |> box |> unbox)
        conf.TrainingSampleSet <- new FRTFeatureExtractionTrainingSampleSet(defSamples |> List.ofSeq |> List.map(fun i -> i :> FeatureExtractionTrainingSample<IKernelModule, float32, (float32 list * float32 list) list>))
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
        FRTSchedulingEngine([], [], false, runtimeRun)
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

            this.Configuration <- FRTSchedulingEngine.CreateConfiguration(extractors |> Seq.toList,
                                                                          samples |> Seq.toList,
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
            FRTSchedulingEngine(featureExtractors |> List.ofSeq, samples |> List.ofSeq, true, runtimeRun)
        else
            FRTSchedulingEngine(FRTSchedulingEngine.configurationFile, runtimeRun)
            
    new(runtimeRun, f:FRTFeatureExtractor list, s:FRTFeatureExtractionTrainingSample list) =
        // Load default components 
        FRTSchedulingEngine(f, s, false, runtimeRun)

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
                        use s = new System.IO.StreamReader(new FileStream(FRTSchedulingEngine.profilingDataFile, FileMode.Open))
                        let pd = new FRTProfilingData()
                        pd.FromCsv(s)
                        pd
                     else
                        // Must profile     
                        let opts = Map.empty.Add("RunningMode", box TrainingSampleRunningMode.FeaturesAndExecutionTime).Add("RuntimeRun", box runtimeRun)

                        let featureValues, times = 
                            let data = configuration.TrainingSampleSet.Run(configuration.FeatureExtractorSet, devices, opts) 
                            data |> List.reduce (@) |> 
                                                   List.unzip ||> 
                                                   (fun feat time ->
                                                        (Array2D.init feat.Length feat.[0].Length (fun r c -> feat.[r].[c])), 
                                                        (Array2D.init time.Length time.[0].Length (fun r c -> time.[r].[c])))
                        new FRTProfilingData(Features = featureValues, Times = times)

                // Save profiling data
                use s = new StreamWriter(new FileStream(FRTSchedulingEngine.profilingDataFile, FileMode.Create))
                profilingData.ToCsv(s)

                // Do regression
                let regressionData = this.ComputeRegressionData(profilingData.Features, profilingData.Times)
                ()
                // Save regression data
                (*let serializer = new XmlSerializer(typeof<FRTRegressionData>)
                use stream = new FileStream(FRTSchedulingEngine.regressionDataFile, FileMode.Create) 
                serializer.Serialize(stream, regressionData)*)

    override this.OnKernelCompile(k: IKernelModule) =
        configuration.FeatureExtractorSet.BuildFinalizers(k)

    override this.OnKernelRun(k, finalizers, args, opts) =
        let fv = configuration.FeatureExtractorSet.EvaluateFinalizers(k, finalizers, args)
        OpenCL.OpenCLPlatform.Platforms.[0].Devices.[0]
            


    