namespace FSCL.Runtime.Scheduling.FRTSchedulingEngine.FeatureExtraction

open FSCL.Compiler
open FSCL.Runtime.Scheduling
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open System.Reflection
open Microsoft.FSharp.Linq.RuntimeHelpers
open FSCL.Compiler.Util
open FSCL.Language
open FSCL.Runtime.Scheduling.FRTSchedulingEngine

[<FRTFeatureExtractor("MemoryAccessCounter")>]
type MemoryAccessCounter() = 
    inherit FRTDefaultFeatureExtractor()

    override this.FeatureIDs 
        with get() =
            [ "Read accesses to global memory (per thread)"; 
               "Write accesses to global memory (per thread)"; 
               "Read accesses to local memory (per thread)"; 
               "Write accesses to local memory (per thread)"; 
               "Read accesses to constant memory (per thread)" ]

    override this.BuildFinalizers(m: IKernelModule) =
        let parameters = m.Kernel.OriginalParameters |> 
                         Seq.map(fun (p: IOriginalFunctionParameter) -> 
                                    (p.OriginalParamterInfo, p.OriginalPlaceholder)) |>
                         Array.ofSeq   

        let isLoadingOrStoringFrom(e: Expr, parameters: (ParameterInfo * Var) array, space: AddressSpace) =
            let rec isLoadingOrStoringInternal(e:Expr) =
                match e with
                | Patterns.Var(arrayVar) ->
                    let matchingParam = Array.tryFind (fun (p, v) -> v = arrayVar) parameters
                    if matchingParam.IsSome then
                        let paramSpace = m.Kernel.GetParameter((matchingParam.Value |> snd).Name).Value.Meta.Get<AddressSpaceAttribute>().AddressSpace
                        match paramSpace, space with
                        | AddressSpace.Global, AddressSpace.Auto
                        | AddressSpace.Auto, AddressSpace.Global ->
                            true
                        | _, _ ->
                            paramSpace = space
                    else
                        false
                | Patterns.Call(o, mi, a) ->
                    if mi.Name.EndsWith("pasum") then
                        isLoadingOrStoringInternal(a.[0]) || isLoadingOrStoringInternal(a.[1])
                    else
                        false
                | Patterns.Coerce(e, t) ->
                    isLoadingOrStoringInternal(e)
                | _ ->
                    false
            isLoadingOrStoringInternal(e)
                    
        // Count global read
        let rgcount = ExpressionCounter.Count(m.Kernel.OriginalBody,
                                                  parameters,
                                                  (fun (e, parameters, continuation) ->
                                                    match e with
                                                    | Patterns.Call(e, i, l) ->    
                                                        // vload
                                                        if i.Name = "vload" && isLoadingOrStoringFrom(l.[1], parameters, AddressSpace.Global) then
                                                            let fc = continuation(l.[0]) 
                                                            let sc = continuation(l.[1]) 
                                                            Value(<@ 1.0f + %fc + %sc @>)
                                                        // Classic access                                         
                                                        else if i.DeclaringType.Name = "IntrinsicFunctions" then
                                                            if i.Name = "GetArray" || i.Name = "GetArray2D" || i.Name = "GetArray3D" then
                                                                let arrayVar = 
                                                                    match l.[0] with 
                                                                    | Patterns.Var(v) -> 
                                                                        Some(v)
                                                                    | _ -> 
                                                                        None
                                                                if arrayVar.IsSome then
                                                                    let matchingParam = Array.tryFind (fun (p, v) -> v = arrayVar.Value) parameters
                                                                    if matchingParam.IsSome && 
                                                                       (m.Kernel.GetParameter((matchingParam.Value |> snd).Name).Value.Meta.Get<AddressSpaceAttribute>().AddressSpace <> AddressSpace.Local) && 
                                                                       (m.Kernel.GetParameter((matchingParam.Value |> snd).Name).Value.Meta.Get<AddressSpaceAttribute>().AddressSpace <> AddressSpace.Constant) then 
                                                                        let lc = continuation(l.[1])                                                                                                           
                                                                        Value(<@ 1.0f + %lc @>)
                                                                    else
                                                                        Continue
                                                                 else
                                                                    Continue
                                                            else
                                                                Continue
                                                        else
                                                            Continue
                                                    | _ ->
                                                        Continue),
                                                  false)
        // Count global write
        let wgcount = ExpressionCounter.Count(m.Kernel.OriginalBody,
                                                  parameters,
                                                  (fun (e, parameters, continuation) ->
                                                    match e with
                                                    | Patterns.Call(e, i, l) ->                                
                                                        // vload
                                                        if i.Name = "vstore" && isLoadingOrStoringFrom(l.[1], parameters, AddressSpace.Global) then
                                                            let fc = continuation(l.[0]) 
                                                            let sc = continuation(l.[1]) 
                                                            Value(<@ 1.0f + %fc + %sc @>)
                                                        // Classic access                                         
                                                        else if i.DeclaringType.Name = "IntrinsicFunctions" then
                                                            if i.Name = "SetArray" || i.Name = "SetArray2D" || i.Name = "SetArray3D" then
                                                                let arrayVar = 
                                                                    match l.[0] with 
                                                                    | Patterns.Var(v) -> 
                                                                        Some(v)
                                                                    | _ -> 
                                                                        None
                                                                if arrayVar.IsSome then
                                                                    let matchingParam = Array.tryFind (fun (p, v) -> v = arrayVar.Value) parameters
                                                                    if matchingParam.IsSome && 
                                                                       (m.Kernel.GetParameter((matchingParam.Value |> snd).Name).Value.Meta.Get<AddressSpaceAttribute>().AddressSpace <> AddressSpace.Local) && 
                                                                       (m.Kernel.GetParameter((matchingParam.Value |> snd).Name).Value.Meta.Get<AddressSpaceAttribute>().AddressSpace <> AddressSpace.Constant) then 
                                                                         let lc1 = continuation(l.[1])
                                                                         let lc2 = continuation(l.[2])                                                                                                               
                                                                         Value(<@ 1.0f + %lc1 + %lc2 @>)
                                                                    else
                                                                        Continue
                                                                 else
                                                                    Continue
                                                            else
                                                                Continue
                                                        else
                                                            Continue
                                                    | _ ->
                                                        Continue),
                                                  false)       
        // Count local read
        let rlcount = ExpressionCounter.Count(m.Kernel.OriginalBody,
                                                  parameters,
                                                  (fun (e, parameters, continuation) ->
                                                    match e with
                                                    | Patterns.Call(e, i, l) ->                               
                                                        // vload
                                                        if i.Name = "vload" && isLoadingOrStoringFrom(l.[1], parameters, AddressSpace.Local) then
                                                            let fc = continuation(l.[0]) 
                                                            let sc = continuation(l.[1]) 
                                                            Value(<@ 1.0f + %fc + %sc @>)
                                                        // Classic access                                         
                                                        else if i.DeclaringType.Name = "IntrinsicFunctions" then
                                                            if i.Name = "GetArray" || i.Name = "GetArray2D" || i.Name = "GetArray3D" then
                                                                let arrayVar = 
                                                                    match l.[0] with 
                                                                    | Patterns.Var(v) -> 
                                                                        Some(v)
                                                                    | _ -> 
                                                                        None
                                                                if arrayVar.IsSome then
                                                                    let localEmbedDeclared = Seq.tryFind (fun v -> v = arrayVar.Value) (m.Kernel.LocalVars.Keys)
                                                                    let matchingParam = Array.tryFind (fun (p, v) -> v = arrayVar.Value) parameters
                                                                    if localEmbedDeclared.IsSome ||
                                                                       (matchingParam.IsSome && 
                                                                        m.Kernel.GetParameter((matchingParam.Value |> snd).Name).Value.Meta.Get<AddressSpaceAttribute>().AddressSpace = AddressSpace.Local) then 
                                                                         let lc1 = continuation(l.[1])                                                                                                          
                                                                         Value(<@ 1.0f + %lc1 @>)
                                                                    else
                                                                        Continue
                                                                 else
                                                                    Continue
                                                            else
                                                                Continue
                                                        else
                                                            Continue
                                                    | _ ->
                                                        Continue),
                                                  false)
        // Count local write
        let wlcount = ExpressionCounter.Count(m.Kernel.OriginalBody,
                                                  parameters,
                                                  (fun (e, parameters, continuation) ->
                                                    match e with
                                                    | Patterns.Call(e, i, l) ->                                                       
                                                        // vload
                                                        if i.Name = "vstore" && isLoadingOrStoringFrom(l.[1], parameters, AddressSpace.Local) then
                                                            let fc = continuation(l.[0]) 
                                                            let sc = continuation(l.[1]) 
                                                            Value(<@ 1.0f + %fc + %sc @>)
                                                        // Classic access                                         
                                                        else if i.DeclaringType.Name = "IntrinsicFunctions" then
                                                            if i.Name = "SetArray" || i.Name = "SetArray2D" || i.Name = "SetArray3D" then
                                                                let arrayVar = 
                                                                    match l.[0] with 
                                                                    | Patterns.Var(v) -> 
                                                                        Some(v)
                                                                    | _ -> 
                                                                        None
                                                                if arrayVar.IsSome then
                                                                    let localEmbedDeclared = Seq.tryFind (fun v -> v = arrayVar.Value) (m.Kernel.LocalVars.Keys)
                                                                    let matchingParam = Array.tryFind (fun (p, v) -> v = arrayVar.Value) parameters
                                                                    if localEmbedDeclared.IsSome ||
                                                                       (matchingParam.IsSome && 
                                                                        m.Kernel.GetParameter((matchingParam.Value |> snd).Name).Value.Meta.Get<AddressSpaceAttribute>().AddressSpace = AddressSpace.Local) then 
                                                                        let lc1 = continuation(l.[1])
                                                                        let lc2 = continuation(l.[2])                                                                                                               
                                                                        Value(<@ 1.0f + %lc1 + %lc2 @>)
                                                                    else
                                                                        Continue
                                                                 else
                                                                    Continue
                                                            else
                                                                Continue
                                                        else
                                                            Continue
                                                    | _ ->
                                                        Continue),
                                                  false)
                                        
        // Count constant read
        let rccount = ExpressionCounter.Count(m.Kernel.OriginalBody,
                                                  parameters,
                                                  (fun (e, parameters, continuation) ->
                                                    match e with
                                                    | Patterns.Call(e, i, l) ->  
                                                        // vload
                                                        if i.Name = "vload" && isLoadingOrStoringFrom(l.[1], parameters, AddressSpace.Constant) then
                                                            let fc = continuation(l.[0]) 
                                                            let sc = continuation(l.[1]) 
                                                            Value(<@ 1.0f + %fc + %sc @>)
                                                        // Classic access                                         
                                                        else if i.DeclaringType.Name = "IntrinsicFunctions" then
                                                            if i.Name = "GetArray" || i.Name = "GetArray2D" || i.Name = "GetArray3D" then
                                                                let arrayVar = 
                                                                    match l.[0] with 
                                                                    | Patterns.Var(v) -> 
                                                                        Some(v)
                                                                    | _ -> 
                                                                        None
                                                                if arrayVar.IsSome then
                                                                    let matchingParam = Array.tryFind (fun (p, v) -> v = arrayVar.Value) parameters
                                                                    if (matchingParam.IsSome && 
                                                                        m.Kernel.GetParameter((matchingParam.Value |> snd).Name).Value.Meta.Get<AddressSpaceAttribute>().AddressSpace = AddressSpace.Constant) then 
                                                                         let lc1 = continuation(l.[1])                                                                                                              
                                                                         Value(<@ 1.0f + %lc1 @>)
                                                                    else
                                                                        Continue
                                                                 else
                                                                    Continue
                                                            else
                                                                Continue
                                                        else
                                                            Continue
                                                    | _ ->
                                                        Continue),
                                                  false)
                       
        [ LeafExpressionConverter.EvaluateQuotation(rgcount);
          LeafExpressionConverter.EvaluateQuotation(wgcount); 
          LeafExpressionConverter.EvaluateQuotation(rlcount);
          LeafExpressionConverter.EvaluateQuotation(wlcount);
          LeafExpressionConverter.EvaluateQuotation(rccount) ] |> box
        