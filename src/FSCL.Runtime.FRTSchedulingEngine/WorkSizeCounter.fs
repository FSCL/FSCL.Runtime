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

[<FRTFeatureExtractor("WorkSizeCounter")>]
type WorkSizeCounter() = 
    inherit FRTFeatureExtractor() 
    
    override this.FeatureIDs
        with get() =
            [ "Global work size dim 1 (number of threads)"; "Global work size dim 2 (number of threads)"; "Global work size dim 3 (number of threads)";
               "Local work size dim 1 (number of threads)"; "Local work size dim 2 (number of threads)"; "Local work size dim 3 (number of threads)" ]

    override this.BuildFinalizers(m: IKernelModule) =
        () :> obj

    override this.EvaluateFinalizers(m, _, args) =
        let globalSize, localSize =
            let ws = args |> List.find(fun o -> o :? WorkSize) :?> WorkSize
            ws.GlobalSize(), ws.LocalSize()

        let data = [ globalSize.[0] |> float32; (if globalSize.Length > 1 then globalSize.[1] |> float32 else 1.0f); (if globalSize.Length > 2 then globalSize.[2] |> float32 else 1.0f);
                      localSize.[0] |> float32; (if localSize.Length > 1 then localSize.[1] |> float32 else 1.0f); (if localSize.Length > 2 then localSize.[2] |> float32 else 1.0f) ]
        data
        
            
                

                