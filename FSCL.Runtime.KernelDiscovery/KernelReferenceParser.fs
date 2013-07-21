namespace FSCL.Runtime.KernelDiscovery

open System
open FSCL.Runtime
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Quotations

[<assembly:DefaultComponentAssembly>]
do()

[<KernelDiscoveryProcessor("FSCL_REFERENCE_DISCOVERY_PROCESSOR", Dependencies = [| "FSCL_CALL_EXPRESSION_DISCOVERY_PROCESSOR" |])>]
type KernelReferenceDiscovery() =      
    interface IKernelDiscoveryPRocessors with
    
        member this.Run(obj, step) =
            if (obj :? Expr) then
                match QuotationAnalysis.GetKernelFromName(expr :?> Expr) with
                | Some(mi, b) -> 
                    // Create signleton kernel call graph
                    let kcg = new ModuleCallGraph()
                    kcg.AddKernel(new KernelInfo(mi, b))
                    // Create module
                    let km = new KernelModule(kcg)
                    Some(km)
                | _ ->
                    None
            else
                None
            