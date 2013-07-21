namespace FSCL.Compiler.MedulePreparsing

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
        // Get cache
        if kmodule.CustomInfo.ContainsKey("RUNTIME_CACHE") then
            let cache = kmodule.CustomInfo.["RUNTIME_CACHE"] :?> GlobalRuntimeCache
            for k in kmodule
        

