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
        
[<StepProcessor("FSCL_RUNTIME_EXECUTION_COLLECTION_COMPOSITION_PROCESSOR", 
                "FSCL_RUNTIME_EXECUTION_STEP")>]
type CollectionCompositionExecutionProcessor() =      
    inherit CompilerStepProcessor<IKFGNode * Dictionary<Var, obj> * bool, ExecutionOutput option>()
       
    override this.Run((fnode, env, isRoot), s, opts) =
        let step = s :?> NodeExecutionStep
        let pool = step.BufferPoolManager

        match fnode with 
        | :? IKFGCollectionCompositionNode as node ->
            // Ok, this is a collection composition
            // Now check which composition is this
            match node.CompositionID.Name with
            | "Map" ->
                // Execute input (map has only one input)
                let input = node.Input |> Seq.map(fun i -> step.Process(i, env, false, opts)) |> Seq.head

                // Ensure the input has a managed representation up-to-date
                let managedInput =
                    match input with
                    | ReturnedBuffer(buffer) ->
                        pool.BeginOperateOnBuffer(buffer, true)
                    | ReturnedValue(v) ->
                        v :?> Array

                // Run subgraph
                // Create task
                let task = fun (el:obj) ->
                            // Associate el to collection vars
                            let collectionVars = Util.AssociateObjArgToLambdaVars(el, node.CollectionVars)
                            Util.MergeLambdaVarsToObjArgDictionaries(collectionVars, env)

                            // Run subgraph
                            let result = step.Process(node.SubGraph, collectionVars, isRoot, opts)
                            match result with
                            | ReturnedBuffer(b) ->
                                pool.ReadBuffer(b) :> obj 
                            | ReturnedValue(v) -> 
                                v

                // Spawn threads to execute subgraphs
                let tasksResult = Util.RunParallelAsync(task, managedInput, node.OutputType.GetElementType())

                // Now return 
                let result = ReturnedValue(tasksResult)

                // End operate on buffer                
                match input with
                | ReturnedBuffer(buffer) ->
                    pool.EndOperateOnBuffer(buffer, managedInput, false)
                | _ ->
                    ()

                // Return the objects that the F# kernels eventually returns as a tuple (if more than 1)
                Some(result)

            | "Map2" ->
                // Execute input (map2 has 2 inputs)
                let input = node.Input |> 
                            Seq.map(fun i -> 
                                    async {                            
                                        return step.Process(i, env, false, opts)        
                                    }) |> Async.Parallel |> Async.RunSynchronously

                let managedInput = 
                    input |> Array.map(fun r ->                          
                                        match r with
                                        | ReturnedBuffer(buffer) ->
                                            pool.BeginOperateOnBuffer(buffer, true)
                                        | ReturnedValue(v) ->
                                            v :?> Array)                                    
                // Run subgraph
                // Create task
                let task = fun (el:obj) ->
                            // Associate el to collection vars
                            let collectionVars = Util.AssociateObjArgToLambdaVars(el, node.CollectionVars)
                            Util.MergeLambdaVarsToObjArgDictionaries(collectionVars, env)

                            // Run subgraph
                            let result = step.Process(node.SubGraph, collectionVars, isRoot, opts)
                            match result with
                            | ReturnedBuffer(b) ->
                                pool.ReadBuffer(b) :> obj 
                            | ReturnedValue(v) -> 
                                v

                // Spawn threads to execute subgraphs
                let tasksResult = Util.RunParallelAsync(task, managedInput.[0], managedInput.[1], node.OutputType.GetElementType())

                // Now return 
                let result = ReturnedValue(tasksResult)

                // End operate on buffer 
                input |> Array.iteri(fun i item ->
                                        match item with       
                                        | ReturnedBuffer(buffer) ->
                                            pool.EndOperateOnBuffer(buffer, managedInput.[i], false)
                                        | _ ->
                                            ())

                // Return the objects that the F# kernels eventually returns as a tuple (if more than 1)
                Some(result)
                
            | "Fold" ->
                // Execute input (fold has 2 inputs)
                let input = node.Input |> 
                            Seq.map(fun i -> 
                                    step.Process(i, env, false, opts)        
                                    ) |> Seq.toArray

                let initialState =                           
                    match input.[0] with
                    | ReturnedBuffer(b) ->
                        pool.BeginOperateOnBuffer(b, true) :> obj
                    | ReturnedValue(v) ->
                        v    
                        
                let managedInput = 
                    match input.[1] with
                    | ReturnedBuffer(buffer) ->
                        let v = pool.BeginOperateOnBuffer(buffer, true)
                        pool.EndOperateOnBuffer(buffer, initialState :?> Array, false)
                        v
                    | ReturnedValue(v) ->
                        v :?> Array
                                       
                // Run sequentially
                let mutable currentState = initialState
                for item in managedInput do                
                    let collectionVars = Util.AssociateObjArgToLambdaVars((currentState, item) :> obj, node.CollectionVars)
                    Util.MergeLambdaVarsToObjArgDictionaries(collectionVars, env)
                    let result = step.Process(node.SubGraph, collectionVars, isRoot, opts)
                    match result with
                    | ReturnedBuffer(b) ->
                        currentState <- pool.BeginOperateOnBuffer(b, true)
                        pool.EndOperateOnBuffer(b, currentState :?> Array, false)
                    | ReturnedValue(v) -> 
                        currentState <- v

                // Now return 
                let result = ReturnedValue(currentState)

                // End operate on buffer 
                match input.[1] with
                | ReturnedBuffer(buffer) ->
                    pool.EndOperateOnBuffer(buffer, managedInput, false)
                | _ ->
                    ()

                // Return the objects that the F# kernels eventually returns as a tuple (if more than 1)
                Some(result)
            | _ ->
                failwith "Error"
        | _ ->
            None