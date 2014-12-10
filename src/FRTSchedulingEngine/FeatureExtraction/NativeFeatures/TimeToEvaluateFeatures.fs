namespace FSCL.Runtime.Scheduling.FRTSchedulingEngine.FeatureExtraction

open FSCL.Compiler
open FSCL.Runtime.Scheduling
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open System.Reflection
open Microsoft.FSharp.Linq.RuntimeHelpers
open FSCL.Runtime.Scheduling.FRTSchedulingEngine
open System
open FSCL.Compiler.Util
open FSCL.Compiler.FunctionPostprocessing
open FSCL.Language
open System.Runtime.InteropServices
open System.Diagnostics

[<FRTFeatureExtractor("TimeToEvalFeatures")>]
type TimeToEvaluateFeatures(features: FeatureExtractorSet<IKernelModule, float32>, iters: int) = 
    inherit FRTFeatureExtractor() 
    
    override this.FeatureIDs
        with get() =
            [ "Time to eval features" ]

    override this.BuildFinalizers(m: IKernelModule) =
        () :> obj

    override this.EvaluateFinalizers(m, _, args) =
        let precomputedFeatures = features.BuildFinalizers(m :?> KernelModule)
        let w = new Stopwatch()
        w.Start()
        for i = 1 to iters do
            features.EvaluateFinalizers(m :?> KernelModule, precomputedFeatures, args) |> ignore
        w.Stop()
        [ (float32)((double)w.ElapsedMilliseconds/(double)iters) ]

     
        
            
                

                