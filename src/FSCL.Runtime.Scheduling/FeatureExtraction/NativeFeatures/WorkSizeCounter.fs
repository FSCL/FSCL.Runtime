namespace FSCL.Runtime.Scheduling.FeatureExtraction

open FSCL.Compiler
open FSCL.Runtime.Scheduling
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open System.Reflection
open Microsoft.FSharp.Linq.RuntimeHelpers
//open QuotEval.QuotationEvaluation
open System
open FSCL.Compiler.Util
open FSCL.Compiler.FunctionPostprocessing
open FSCL.Language
open System.Runtime.InteropServices

type WorkSizeCounter() = 
    inherit IFeatureExtractor()
    override this.FeatureNameList 
        with get() =
            [ "Global work size dim 1 (number of threads)"; "Global work size dim 2 (number of threads)"; "Global work size dim 3 (number of threads)";
              "Local work size dim 1 (number of threads)"; "Local work size dim 2 (number of threads)"; "Local work size dim 3 (number of threads)" ]

    override this.Precompute(m: IKernelModule) =
        [] :> obj

    override this.Evaluate(m, precomputed, args, opts) =
        let globalSize, localSize =
            let ws = args |> List.find(fun (o:obj) -> o.GetType() = typeof<WorkSize>) |> (fun o -> o :?> WorkSize)
            ws.GlobalSize(), ws.LocalSize()

        let data = [ globalSize.[0]; (if globalSize.Length > 1 then globalSize.[1] else 1L); (if globalSize.Length > 2 then globalSize.[2] else 1L);
                     localSize.[0]; (if localSize.Length > 1 then localSize.[1] else 1L); (if localSize.Length > 2 then localSize.[2] else 1L) ]
        List.mapi (fun i s -> (data.[i] :> obj)) (this.FeatureNameList)
            
                

                