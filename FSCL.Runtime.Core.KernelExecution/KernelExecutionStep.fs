namespace FSCL.Runtime.RuntimeSteps

open System
open FSCL.Compiler
open FSCL.Runtime
open FSCL.Runtime.Managers
open Microsoft.FSharp.Linq.RuntimeHelpers
open System.Collections.Generic

[<Step("FSCL_RUNTIME_EXECUTION_STEP")>]
type KernelExecutionStep(tm: TypeManager,
                         processors: ICompilerStepProcessor list) = 
    inherit CompilerStep<FlowGraphNode * BufferPoolManager, ExecutionOutput>(tm, processors)

    let mutable opts = null
    let mutable bufferPoolManager = null

    member this.BufferPoolManager
        with get() =
            bufferPoolManager
        and private set(v) =
            bufferPoolManager <- v
                    
    member this.Process(input:FlowGraphNode, isRoot: bool) =
        let mutable index = 0
        let mutable output = None
        while (output.IsNone) && (index < processors.Length) do
            output <- processors.[index].Execute((input, isRoot), this, opts) :?> ExecutionOutput option
            index <- index + 1
        if output.IsNone then
            raise (KernelExecutionException("The runtime is not able to determine the way to execute kernel " + input.KernelData.Kernel.ID.ToString()))
        output.Value
        
    override this.Run((input, pool), opt) =
        opts <- opt
        this.BufferPoolManager <- pool
        let execResult = this.Process((input, true))
        ContinueCompilation(execResult)