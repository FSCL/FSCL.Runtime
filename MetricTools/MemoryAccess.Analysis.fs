namespace FSCL.Runtime.MetricTools
    
open Cloo
open System
open Microsoft.FSharp.Reflection
open System.Runtime.InteropServices
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Quotations

module AccessAnalyzer =
    let Analyze (meth:MethodInfo) =
        let parms = meth.GetParameters()        
        let parmsAccess = new Dictionary<string, BufferAccess>()

        let rec AnalyzeInner (expr) =
            match expr with            
            | Patterns.Call(e, i, l) -> 
                if i.DeclaringType.Name = "IntrinsicFunctions" then
                    if i.Name = "GetArray" || i.Name = "GetArray2D" || i.Name = "GetArray3D" then
                        let arrayVar = 
                            match l.[0] with 
                            | Patterns.Var(v) -> Some(v)
                            | _ -> None
                        if arrayVar.IsSome then
                            let matchingParam = Array.tryFind (fun (p:ParameterInfo) -> p.Name = arrayVar.Value.Name && p.ParameterType = arrayVar.Value.Type) parms
                            if matchingParam.IsSome then
                                if parmsAccess.ContainsKey(arrayVar.Value.Name) then
                                    if parmsAccess.[arrayVar.Value.Name] = BufferAccess.WRITE_ONLY then
                                        parmsAccess.[arrayVar.Value.Name] <- BufferAccess.READ_WRITE
                                    if parmsAccess.[arrayVar.Value.Name] = BufferAccess.NO_ACCESS then
                                        parmsAccess.[arrayVar.Value.Name] <- BufferAccess.READ_ONLY
                                else
                                    parmsAccess.Add(arrayVar.Value.Name, BufferAccess.READ_ONLY)
                    if i.Name = "SetArray" || i.Name = "SetArray2D" || i.Name = "SetArray3D" then
                        let arrayVar = 
                            match l.[0] with 
                            | Patterns.Var(v) -> Some(v)
                            | _ -> None
                        if arrayVar.IsSome then
                            let matchingParam = Array.tryFind (fun (p:ParameterInfo) -> p.Name = arrayVar.Value.Name && p.ParameterType = arrayVar.Value.Type) parms
                            if matchingParam.IsSome then
                                if parmsAccess.ContainsKey(arrayVar.Value.Name) then
                                    if parmsAccess.[arrayVar.Value.Name] = BufferAccess.READ_ONLY then
                                        parmsAccess.[arrayVar.Value.Name] <- BufferAccess.READ_WRITE
                                    if parmsAccess.[arrayVar.Value.Name] = BufferAccess.NO_ACCESS then
                                        parmsAccess.[arrayVar.Value.Name] <- BufferAccess.WRITE_ONLY
                                else
                                    parmsAccess.Add(arrayVar.Value.Name, BufferAccess.WRITE_ONLY)
            | ExprShape.ShapeVar(v) ->
                ()
            | ExprShape.ShapeLambda(v, e) ->
                AnalyzeInner (e)
            | ExprShape.ShapeCombination(o, a) ->
                List.iter (fun el -> AnalyzeInner(el)) a
        
        match meth with
        | DerivedPatterns.MethodWithReflectedDefinition(b) ->
            // Fill dictionary with parameters, setting the access mode of each of them to NO_ACCESS
            Array.iter (fun (p:ParameterInfo) -> parmsAccess.Add(p.Name, BufferAccess.NO_ACCESS)) (meth.GetParameters())
            // Perform analysis
            do AnalyzeInner(b)
            // Convert to list of pairs (param, access)
            let pAccess = new Dictionary<ParameterInfo, BufferAccess>()
            Array.iter (fun (p:ParameterInfo) -> 
                pAccess.Add(p, parmsAccess.[p.Name])) (meth.GetParameters())
            pAccess
        | _ ->
            raise (new AccessException("Cannon analyze a function not labeled with ReflectedDefinition attribute"))
            