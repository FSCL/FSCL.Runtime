module Test

open FSCL.Compiler
open FSCL.Runtime.Scheduling
open FSCL.Runtime.Scheduling.FeatureExtraction
open FSCL.Language
open System.Collections.Generic
open System
open System.IO
open FSCL.Runtime
open FSCL.Runtime.Language
open System.Diagnostics

open VectorAddTrainingSample
open MatrixMultTrainingSample
open ConvolutionTrainingSample
open SobelFilterTrainingSample
open PrefixSumTrainingSample

[<EntryPoint>]
let main argv = 
    //opts.Add(CompilerOptions.ParseOnly, ())

    // Test vector add
    //let km = compiler.Compile(<@ VectorAdd(a, b, c) @>, opts) :?> IKernelModule
    //let precomputedFeatures = fec.Precompute(km)
    //let features = fec.Evaluate(km, precomputedFeatures, [| a; b; c |])
    

    // Matrix multiplication
    //MatrixMultFeatures.TestFeatures 64L 1024L 100

    // Convolution
    let chain = new FeatureExtractionChain([| new BranchCounter();
                                              new MemoryAccessCounter();
                                              new ArithmeticOperationCounter();
                                              new DataSizeCounter();
                                              new WorkSizeCounter() |])
    
    let samples = [|
                        //new VectorAddTrainingSample() :> IFeatureExtractionTrainingSample;
                        //new MatrixMultSimpleTrainingSample() :> IFeatureExtractionTrainingSample;
                        //new MatrixMultAdvancedTrainingSample() :> IFeatureExtractionTrainingSample;
                        new SobelFilterTrainingSample() :> IFeatureExtractionTrainingSample
                  |]

    for sample in samples do
        sample.Run(chain)

    0 // return an integer exit code

