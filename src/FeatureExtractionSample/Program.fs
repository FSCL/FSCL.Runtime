module Test

open FSCL
open FSCL.Runtime
open FSCL.Runtime.Scheduling
open FSCL.Runtime.Scheduling.FRTSchedulingEngine
open FSCL.Runtime.Scheduling.FRTSchedulingEngine.FeatureExtraction
open FSCL.Runtime.Scheduling.FRTSchedulingEngine.TrainingSamples
open Microsoft.FSharp.Quotations
open System.Collections.Generic

[<EntryPoint>]
let main argv = 
    let engine = new FRTSchedulingEngine((fun e -> (e :?> Expr).Run()), 
                                         [ new ArithmeticOperationCounter();
                                           new DataSizeCounter();
                                           new WorkSizeCounter();
                                           new BranchCounter();
                                           new TimeToEvaluateFeatures(new FRTFeatureExtractorSet([ new ArithmeticOperationCounter();
                                                                                                   new DataSizeCounter();
                                                                                                   new WorkSizeCounter();
                                                                                                   new BranchCounter() ]), 100) ],
                                         [ new VectorAddTrainingSample();
                                           new MatrixMultAdvancedTrainingSample();
                                           new ConvolutionTrainingSample();
                                           new TransposeNaiveTrainingSample() ])

    //engine.DumpConf()
    engine.OnRuntimeLoad(FSCL.Runtime.GetOpenCLPlatforms())

    0 // return an integer exit code

