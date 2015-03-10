namespace FSCL.Runtime.RuntimeSteps

open System
open FSCL.Compiler
open FSCL.Runtime
open FSCL.Runtime.Managers
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Linq.RuntimeHelpers
open System.Collections.Generic

[<Step("FSCL_RUNTIME_EXECUTION_STEP")>]
type NodeExecutionStep(tm: TypeManager,
                       processors: ICompilerStepProcessor list) = 
    inherit CompilerStep<IKernelExpression * BufferPoolManager * KernelCreationManager, ExecutionOutput>(tm, processors)

    let mutable opts = null
    let mutable bufferPoolManager = null
    let mutable kernelCreationManager = null

    member this.BufferPoolManager
        with get() =
            bufferPoolManager
        and private set(v) =
            bufferPoolManager <- v
            
    member this.KernelCreationManager
        with get() =
            kernelCreationManager
        and private set(v) =
            kernelCreationManager <- v
                    
    member this.Process(input:IKFGNode, collectionVars:Dictionary<Var, obj>, isRoot: bool) =
        let mutable index = 0
        let mutable output = None
        while (output.IsNone) && (index < processors.Length) do
            output <- processors.[index].Execute((input, collectionVars, isRoot), this, opts) :?> ExecutionOutput option
            index <- index + 1
        if output.IsNone then
            raise (KernelExecutionException("The runtime is not able to determine the way to execute the node " + input.ToString()))
        output.Value
        
    override this.Run((input, pool, creation), opt) =
        opts <- opt
        this.BufferPoolManager <- pool
        this.KernelCreationManager <- creation
        let execResult = this.Process((input.KFGRoot, new Dictionary<Var, obj>(), true))
        ContinueCompilation(execResult)