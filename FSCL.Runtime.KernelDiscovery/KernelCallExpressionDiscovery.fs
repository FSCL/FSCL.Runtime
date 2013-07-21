namespace FSCL.Runtime.KernelDiscovery

open FSCL.Runtime
open System.Collections.Generic
open System.Reflection
open System
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection

[<KernelDiscoveryProcessor("FSCL_CALL_EXPRESSION_DISCOVERY_PROCESSOR")>]
type KernelCallExpressionDiscovery() =   
    let rec LiftArgExtraction(expr) =
        match expr with
        | Patterns.Let(v, value, body) ->
            match value with
            | Patterns.TupleGet(te, i) ->
                LiftArgExtraction(body)
            | _ ->
                (expr)
        | _ ->
            (expr)
             
    interface IKernelDiscoveryProcessor with
        member this.Run(obj, step) =
            if (obj :? Expr) then
                let kernels = new List<KernelDiscoveryResult>()
                let arguments = new List<KernelArgExpressionType>()

                match obj :?> Expr with
                // Case k2(k1(args), ...) where k1 doesn't return a tuple value
                | Patterns.Call(o, m, args) ->
                    // Extract sub kernels
                    let subkernels = List.map(fun (e: Expr) -> 
                        try 
                            let sk = step.Process(e)
                            kernels.AddRange(sk)
                            arguments.Add(KernelOutputExpression)
                        with
                            | :? KernelDiscoveryException -> 
                                arguments.Add(DataExpression(e))) args
                    kernels.
                             
                | Patterns.Let(v, value, body) ->
                    (* 
                     * Check if we have something like:
                     * Let(tupleArg, CALLTOSUBKERNEL, Let(a..., Let(b..., CALLTOKERNEL)))
                     * This means we are returning a tuple value from the subkernel and using it
                     * to assign multiple arguments of the outer kernel
                     * This seems to happen only if KERNEL is f(a,b..z) and SUBKERNEL returns (a,b...z)
                     * (i.e. the subkernel "fills" all the parameters of kernel)
                     * but not otherwise (e.g. kernel is f(a,b,...z) and subkernel returns (a,b...x < z)
                     *)
                    if v.Name = "tupledArg" then
                        let lifted = LiftArgExtraction(body)
                        match lifted with
                        | Patterns.Call(o, mi, args) ->                                       
                            let subkernel = 
                                try 
                                    engine.Process(value)
                                with
                                    | :? CompilerException -> null
                            // Add the subkernel to the call graph
                            if (subkernel <> null) then
                                kcg.MergeWith(subkernel.Source)
                            // Get endpoints
                            let endpoints = kcg.EndPoints
                            // Add the current kernel
                            let currentKernel = 
                                try 
                                    engine.Process(mi)
                                with
                                    | :? CompilerException -> null
                            if currentKernel = null then
                                None
                            else
                                kcg.AddKernel(kcg.GetKernel(currentKernel.Source.KernelIDs.[0]))
                                // Setup connections ret-type -> parameter                            
                                if subkernel <> null then   
                                    let retTypes =
                                        if FSharpType.IsTuple(subkernel.Source.EndPoints.[0].ReturnType) then
                                            FSharpType.GetTupleElements(subkernel.Source.EndPoints.[0].ReturnType)
                                        else
                                            [| subkernel.Source.EndPoints.[0].ReturnType |]
                                    for i = 0 to retTypes.Length - 1 do                     
                                        kcg.AddConnection(
                                            endpoints.[0], 
                                            currentKernel.Source.KernelIDs.[0], 
                                            ReturnValue(i), ParameterIndex(i))
                                Some(new KernelModule(kcg))
                        | _ ->
                            None
                    else
                        None
                | _ ->
                    None
            else
                None
            