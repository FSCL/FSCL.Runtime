namespace FSCL.Runtime.RuntimeSteps

open System
open FSCL.Compiler
open FSCL.Compiler.Util
open FSCL.Language
open FSCL.Runtime
open FSCL.Runtime.Managers
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Linq.RuntimeHelpers
open OpenCL
open Microsoft.FSharp.Quotations
open System.Threading

[<assembly:DefaultComponentAssembly>]
do()
        
[<StepProcessor("FSCL_RUNTIME_EXECUTION_VALUE_PROCESSOR", 
                "FSCL_RUNTIME_EXECUTION_STEP")>]
type ValueExecutionProcessor() =      
    inherit CompilerStepProcessor<IKFGNode * Dictionary<Var, obj> * bool, ExecutionOutput option>()
        
    override this.Run((fnode, env, isRoot), s, opts) =
        let step = s :?> NodeExecutionStep
        let pool = step.BufferPoolManager

        match fnode with 
        | :? IKFGDataNode as node ->
            // Replace ref to evn vars in data with the current values
            let data = QuotationAnalysis.FunctionsManipulation.ReplaceVarsWithValues(node.Data, env)
            // This is a regular input argument, evaluate and return
            Some(ReturnedValue(LeafExpressionConverter.EvaluateQuotation(data)))
        | _ ->
            None 
            
