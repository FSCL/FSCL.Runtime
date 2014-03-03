namespace FSCL.Runtime.ModulePreparsing

open System
open System.Reflection
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open FSCL.Compiler
open FSCL.Runtime

[<Step("FSCL_MODULE_PREPARSING_STEP", Before = [| "FSCL_MODULE_PARSING_STEP" |])>] 
type ModulePreparsingStep(tm: TypeManager,
                          processors: ICompilerStepProcessor list) = 
    inherit CompilerStep<obj * KernelModule, obj * KernelModule>(tm, processors)

    override this.Run((expr, kmodule), opts) =
        if FSharpType.IsTuple(expr.GetType()) then
            let fields = FSharpValue.GetTupleFields(expr)
            if fields.[1] :? RuntimeCache then
                // Store into custom data
                kmodule.CustomInfo.Add("RUNTIME_CACHE", fields.[1])
                (fields.[0], kmodule)
            else
                raise (KernelCompilationException("The expected preparsing input type is obj * RuntimeGlobalData but this is not the type of the input received"))
        else
            raise (KernelCompilationException("The expected preparsing input type is obj * RuntimeGlobalData but this is not the type of the input received"))
                

        

