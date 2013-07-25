namespace FSCL.Runtime.CacheInspection

open System
open System.Reflection
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open FSCL.Compiler
open FSCL.Runtime

[<Step("FSCL_CACHE_INSPECTION_STEP", 
       Dependencies = [| "FSCL_MODULE_PARSING_STEP" |], 
       Before = [| "FSCL_MODULE_PREPROCESSING_STEP" |])>] 
type CacheInspectionStep(tm: TypeManager,
                          processors: ICompilerStepProcessor list) = 
    inherit CompilerStep<KernelModule, KernelModule>(tm, processors)

    override this.Run(kmodule) =
        if kmodule.CustomInfo.ContainsKey("RUNTIME_CACHE") then
            // Get cache
            let cache = kmodule.CustomInfo.["RUNTIME_CACHE"] :?> RuntimeCache
            // Skip kernels already compiled
            for k in kmodule.CallGraph.Kernels do
                // If a mathing kernel has been cached and it contains the opencl source code
                if cache.Kernels.ContainsKey(k.ID) && cache.Kernels.[k.ID].OpenCLCode.IsSome then
                    let cachedKernel = cache.Kernels.[k.ID]
                    k.Skip <- true
                    k.Body <- cachedKernel.Info.Body
                    k.Codegen <- cachedKernel.Info.Codegen
                    for item in cachedKernel.Info.CustomInfo do
                        k.CustomInfo.Add(item.Key, item.Value)
                    for item in cachedKernel.Info.ParameterInfo do
                        k.ParameterInfo.Add(item.Key, item.Value)
                    k.Signature <- cachedKernel.Info.Signature
        kmodule
        

