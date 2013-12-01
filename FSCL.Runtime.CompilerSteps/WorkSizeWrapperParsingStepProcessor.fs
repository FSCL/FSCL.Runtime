namespace FSCL.Runtime.ModulePreparsing

open System
open System.Reflection
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open FSCL.Compiler
open FSCL.Runtime
open FSCL.Runtime.HostLanguage
open FSCL.Compiler.ModuleParsing
open Microsoft.FSharp.Linq.QuotationEvaluation

[<StepProcessor("FSCL_WORKSIZE_WRAPPER_PARSING_STEP_PROCESSOR", "FSCL_MODULE_PARSING_STEP")>] 
type WorkSizeWrapperPreparsingStepProcessor() = 
    inherit ModuleParsingProcessor()

    override this.Run(obj, rawStep) =
        let step = rawStep :?> ModuleParsingStep
        if obj :? Expr then
            match obj :?> Expr with
            | DerivedPatterns.SpecificCall <@ worksize @> (exp, typeList, args) ->
                let kModule = step.TryProcess(args.[0])
                if kModule.IsSome then
                    // Store global and local size as custom info
                    // only if tryProcess returned a kernel module with one only kernel instance
                    let k = kModule.Value.FlowGraph
                    k.CustomInfo.Add("GLOBAL_SIZE", args.[1].EvalUntyped())
                    k.CustomInfo.Add("LOCAL_SIZE", args.[2].EvalUntyped())                        
                    kModule
                else
                    None
            | _ ->
                None
        else
            None

