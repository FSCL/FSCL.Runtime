namespace FSCL.Runtime.RuntimeSteps

open System
open FSCL.Compiler
open FSCL.Runtime
open FSCL.Runtime.Managers
open Microsoft.FSharp.Linq.RuntimeHelpers
open Microsoft.FSharp.Quotations

[<Step("FSCL_FLOW_GRAPH_BUILDING_STEP")>]
type FlowGraphBuildingStep(tm: TypeManager,
                           processors: ICompilerStepProcessor list) = 
    inherit CompilerStep<Expr * KernelCreationManager * BufferPoolManager, FlowGraphNode>(tm, processors)

    let mutable opts = null
    let mutable kernelCreationManager:KernelCreationManager = null
                        
    member this.Process(input:Expr) =
        // Compile kernel
            // Compile kernel
        match kernelCreationManager.Process(input, opts) with
        | Some(creationResult) ->            
            // Now execute processors to build a flow graph node for this kernel
            let mutable index = 0
            let mutable output = None
            while (output.IsNone) && (index < processors.Length) do
                output <- processors.[index].Execute(creationResult, this, opts) :?> FlowGraphNode option
                index <- index + 1
            if output.IsNone then
                raise (KernelExecutionException("The runtime is not able to create a flow graph node for " + input.ToString()))
            output
        | None ->
            None

    override this.Run((input, creationManager, poolManager), opt) =
        opts <- opt
        kernelCreationManager <- creationManager                
        let creationResult = (this.Process(input)).Value
        ContinueCompilation(creationResult, poolManager)