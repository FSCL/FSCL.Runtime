namespace FSCL.Runtime.Scheduling

open System
open MathNet
open MathNet.Numerics
open FSCL
open FSCL.Compiler
open System.Collections.Generic
open System.IO
open System.Xml.Serialization

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

type FRTSchedulingEngine(features: FeatureExtractorSet<IKernelModule>, 
                         samples: FeatureExtractionTrainingSampleSet<IKernelModule, (float32[] * float32[]) list>) = 
    inherit SchedulingEngine<IKernelModule, obj list>()

    let mutable regressionData = null
    let computeRegressionData(features: float32[,], times: float32[,]) =
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

    static member private defConfRoot = Path.Combine(Environment.GetFolderPath(
                                                        Environment.SpecialFolder.LocalApplicationData), 
                                                     "FSCL.Runtime", 
                                                     "Scheduling",
                                                     "FeretSchedulingEngine")

    static member private profilingDataFile = Path.Combine(FRTSchedulingEngine.defConfRoot,
                                                           "ProfilingData.xml")

    static member private regressionDataFile = Path.Combine(FRTSchedulingEngine.defConfRoot,
                                                            "RegressionData.xml")
    

    override this.OnRuntimeLoad() =
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
                        let featureValues, times = samples.Run(features, TrainingSampleRunningMode.FeaturesAndExecutionTime) |>
                                                   List.reduce (@) |> 
                                                   List.unzip ||> 
                                                   (fun feat time ->
                                                        (Array2D.init feat.Length feat.[0].Length (fun r c -> feat.[r].[c])), 
                                                        (Array2D.init time.Length time.[0].Length (fun r c -> time.[r].[c])))
                        new FRTProfilingData(Features = featureValues, Times = times)

                // Do regression
                let regr = computeRegressionData(profilingData.Features, profilingData.Times)
                ()

    override this.OnKernelCompile(k: IKernelModule) =
        features.BuildFinalizers(k)

    override this.OnKernelRun(k, finalizers, args, opts) =
        let fv = features.EvaluateFinalizers(k, finalizers, args, opts)

    