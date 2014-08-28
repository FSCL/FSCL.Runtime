﻿module Test

open FSCL.Compiler
open FSCL.Runtime.Scheduling
open FSCL.Runtime.Scheduling.FeatureExtraction
open FSCL.Language
open System.Collections.Generic
open System
open System.IO
open FSCL.Runtime
open FSCL.Language
open System.Diagnostics

open VectorAddTrainingSample
open MatrixMultTrainingSample
open ConvolutionTrainingSample
open SobelFilterTrainingSample
open PrefixSumTrainingSample
open ReductionTrainingSample
open TransposeTrainingSample
open LUDecompositionTrainingSample

[<EntryPoint>]
let main argv = 
    //opts.Add(CompilerOptions.ParseOnly, ())

    // Micro benchmarks
    //let waveFrontBenchmark = new WavefrontBenchmark.WavefrontBenchmark()
    //waveFrontBenchmark.Execute(0, 1)

    //let ldsBenchmark = new LDSBenchmark.LDSBenchmark()
    //ldsBenchmark.Execute(0, 1)

    // Training samples profiling
    let chain = new FeatureExtractionChain([| 
                                              //new BranchCounter();
                                              //new MemoryAccessCounter();
                                              //new ArithmeticOperationCounter();
                                              //new DataSizeCounter();
                                              //new WorkSizeCounter();
                                              new MemoryAccessPatternAnalyser()
                                           |])
    
    let samples = [|
                        //new VectorAddTrainingSample() :> IFeatureExtractionTrainingSample;
                        new MatrixMultSimpleTrainingSample() :> IFeatureExtractionTrainingSample;
                        //new MatrixMultAdvancedTrainingSample() :> IFeatureExtractionTrainingSample;
                        //new SobelFilterTrainingSample() :> IFeatureExtractionTrainingSample;
                        //new ConvolutionTrainingSample() :> IFeatureExtractionTrainingSample; 
                        //new SimpleReductionTrainingSample() :> IFeatureExtractionTrainingSample; 
                        //new AdvancedReductionTrainingSample() :> IFeatureExtractionTrainingSample;
                        //new PrefixSumTrainingSample() :>  IFeatureExtractionTrainingSample;
                        //new TransposeTrainingSample() :> IFeatureExtractionTrainingSample;
                        // ERROR new TransposeFloat4TrainingSample() :> IFeatureExtractionTrainingSample;
                        //new LUDecompositionTrainingSample() :> IFeatureExtractionTrainingSample;
                        //new LUDecompositionOpenCLDirectTrainingSample() :> IFeatureExtractionTrainingSample;
                  |]

    for sample in samples do
        sample.Run(chain)

    50 // return an integer exit code

