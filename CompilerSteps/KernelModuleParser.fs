namespace FSCL.Runtime.CompilerSteps

open System
open FSCL.Compiler
open FSCL.Compiler.Util
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Quotations
open FSCL.Compiler.ModuleParsing

[<assembly:DefaultComponentAssembly>]
do()

[<StepProcessor("FSCL_MODULE_PARSING_PROCESSOR", 
                "FSCL_MODULE_PARSING_STEP")>]
type KernelModuleParser() =      
    inherit ModuleParsingProcessor()
    
    override this.Run(expr, en, opts) =
        let engine = en :?> ModuleParsingStep
        if (expr :? KernelModule) then
            Some(expr :?> KernelModule)
        else
            None
            