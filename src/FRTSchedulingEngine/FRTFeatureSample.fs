namespace FSCL.Runtime.Scheduling.FRTSchedulingEngine

open FSCL.Runtime.Scheduling
open FSCL.Compiler
open System
open System.Reflection
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open System.Xml
open System.Xml.Linq
open System.Diagnostics

[<AllowNullLiteral>]
type FRTFeatureExtractorAttribute(id: string) =
    inherit Attribute()
    member val ID = id with get

[<AllowNullLiteral>]
type FRTFeatureExtractionTrainingSampleAttribute(id: string) =
    inherit Attribute()
    member val ID = id with get

type FRTFeatureExtractor = FeatureExtractor<IKernelModule, float32>  
type FRTDefaultFeatureExtractor = DefaultFeatureExtractor<IKernelModule, float32> 
type FRTFeatureExtractorSet = FeatureExtractorSet<IKernelModule, float32>

[<AbstractClass>]
type FRTFeatureExtractionTrainingSample() =
    inherit FeatureExtractionTrainingSample<IKernelModule, float32, (float32[] * float32[])[]>()
    
type FRTFeatureExtractionTrainingSampleSet = 
    FeatureExtractionTrainingSampleSet<IKernelModule, float32, (float32[] * float32[])[]>

module FRTUtil = 
    let GetAvgAndStdDevCompletionTime(iterations, comp: unit -> unit) =        
        let watch = new Stopwatch()
        let data = Array.zeroCreate<double> iterations
        for i = 1 to iterations do   
            watch.Restart()                   
            comp()
            watch.Stop()
            data.[i] <- (double)watch.ElapsedMilliseconds 
        let avg = data |> Array.average
        let stddev  = Math.Sqrt(data |> Array.map(fun d -> Math.Pow(d - avg, 2.0)) |> Array.average)  
        avg, stddev
                                                