namespace FSCL.Runtime.FlowGraphBullding

open System
open FSCL.Compiler
open FSCL.Runtime
open Microsoft.FSharp.Linq.RuntimeHelpers

[<Step("FSCL_FLOW_GRAPH_BUILDING_STEP")>]
type FlowGraphBuildingStep(tm: TypeManager,
                           processors: ICompilerStepProcessor list) = 
    inherit CompilerStep<KernelFlowGraphBuildingInput * KernelCreationManager * BufferPoolManager, FlowGraphNode>(tm, processors)

    let mutable opts = null
    let mutable bufferPoolManager = null

    member this.BufferPoolManager
        with get() =
            bufferPoolManager
        and private set(v) =
            bufferPoolManager <- v

    member this.TryProcess(input:KernelExecutionInput) =
        let mutable index = 0
        let mutable output = None
        while (output.IsNone) && (index < processors.Length) do
            output <- processors.[index].Execute(input, this, opts) :?> KernelExecutionOutput option
            index <- index + 1
        output
        
    member this.Process(input:KernelFlowGraphBuildingInput, creationManager, pool) =
        let mutable index = 0
        let mutable output = None
        while (output.IsNone) && (index < processors.Length) do
            output <- processors.[index].Execute(input, this, opts) :?> FlowGraphNode option
            index <- index + 1
        if output.IsNone then
            raise (KernelExecutionException("The runtime is not able to create a flow graph node for " + input.Expression.ToString()))
        output.Value

    override this.Run((input, creationManager, pool), opt) =
        opts <- opt
        this.BufferPoolManager <- pool
        ValidResult(this.Process(input, creationManager, pool))