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
            | "Array.map" ->
                // Execute input (map has only one input)
                let input = node.Input |> Seq.map(fun i -> step.Process(i, env, false)) |> Seq.head

                // Ensure the input has a managed representation up-to-date
                let managedInput =
                    match input with
                    | ReturnedTrackedBuffer(_, managed) ->
                        managed
                    | ReturnedUntrackedBuffer(buffer) ->
                        failwith "Error"
                    | ReturnedValue(v) ->
                        v :?> Array

                // Run subgraph
                // Create task
                let task = fun (el:obj) ->
                            // Associate el to collection vars
                            let collectionVars = Util.AssociateObjArgToLambdaVars(el, node.CollectionVars)
                            Util.MergeLambdaVarsToObjArgDictionaries(collectionVars, env)

                            // Run subgraph
                            let result = step.Process(node.SubGraph, collectionVars, true)
                            match result with
                            | ReturnedTrackedBuffer(_, b:Array) ->
                                box b
                            | ReturnedValue(v) -> 
                                v
                            | _ ->
                                failwith "Error"

                // Spawn threads to execute subgraphs
                let tasks = seq {
                                for item in managedInput do
                                    yield async { 
                                        return task item
                                    }
                            } 
                // Now return 
                let result = ReturnedValue(tasks)

                // Return the objects that the F# kernels eventually returns as a tuple (if more than 1)
                Some(result)
            | _ ->
                failwith "Error"
        | _ ->
            None