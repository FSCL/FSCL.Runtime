namespace FSCL.Runtime.KernelDiscovery

open FSCL.Runtime
open FSCL.Compiler
open FSCL.Compiler.Core.Util
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Quotations

[<KernelDiscoveryProcessor("FSCL_LAMBDA_DISCOVERY_PROCESSOR", Dependencies = [| "FSCL_REFERENCE_DISCOVERY_PROCESSOR" |])>]
type KernelLambdaDiscovery() =      
    interface IKernelDiscoveryProcessor with
        
        member this.Run(obj, step) =
            if (obj :? Expr) then
                match QuotationAnalysis.LambdaToMethod(obj :?> Expr) with
                | Some(mi, b) -> 
                    Some([ (mi, []) ])
                | _ ->
                    None
            else
                None
            