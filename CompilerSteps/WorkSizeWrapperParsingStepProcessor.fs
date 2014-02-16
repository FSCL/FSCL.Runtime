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
open Microsoft.FSharp.Linq.RuntimeHelpers

[<StepProcessor("FSCL_WORKSIZE_WRAPPER_PARSING_STEP_PROCESSOR", 
                "FSCL_MODULE_PARSING_STEP",
                Before = [| "FSCL_METHOD_INFO_PARSING_PROCESSOR"; "FSCL_CALL_EXPRESSION_PARSING_PROCESSOR" |])>] 
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
                    k.CustomInfo.Add("GLOBAL_SIZE", LeafExpressionConverter.EvaluateQuotation(args.[1]))
                    k.CustomInfo.Add("LOCAL_SIZE", LeafExpressionConverter.EvaluateQuotation(args.[2]))                        
                    kModule
                else
                    None
            | _ ->
                None
        else
            None

