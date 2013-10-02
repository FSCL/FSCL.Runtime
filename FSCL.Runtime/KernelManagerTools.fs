namespace FSCL.Runtime

open FSCL.Compiler
open System.Reflection
open Microsoft.FSharp.Quotations
open Cloo
open System.Runtime.InteropServices
open System

type internal KernelManagerTools() =
    static member IsOpenCLAvailable() =
        if ComputePlatform.Platforms.Count = 0 then
            false
        else
            // At least one device in a platform available
            ComputePlatform.Platforms |> 
                Seq.map(fun (p: ComputePlatform) -> p.Devices.Count) |>
                Seq.reduce(fun a b -> a + b) > 0

    // Kernel extraction tools
    static member GetKernelArrayDimensions (t:System.Type) =
        // If not array return 0
        if t.IsArray then
            // Any better way to do this?
            let dimensionsString = t.FullName.Split([| '['; ']' |]).[1]
            let dimensions = ref 1
            String.iter (fun c -> if (c = ',') then dimensions := !dimensions + 1) dimensionsString
            !dimensions
        else
            0
            
    static member GetArrayAllocationSize (o:Object) =
        // If not array return 0
        if o.GetType().IsArray then
            let dimensions = KernelManagerTools.GetKernelArrayDimensions(o.GetType())
            let mutable size = 1
            for d in 0 .. dimensions - 1 do
                let dimSize = o.GetType().GetMethod("GetLength").Invoke(o, [| d |]) :?> int
                size <- size * dimSize
            size * Marshal.SizeOf(o.GetType().GetElementType())
        else
            0

    static member GetKernelArrayLength (o) =
        if o.GetType().IsArray then
            Some(o.GetType().GetProperty("Length").GetValue(o) :?> int)
        else
            None
            
    static member GetKernelAdditionalParameters(t:System.Type) =
        // If not array return 0
        if t.IsArray then
            // Any better way to do this?
            let dimensionsString = t.FullName.Split([| '['; ']' |]).[1]
            let dimensions = ref 1
            String.iter (fun c -> if (c = ',') then dimensions := !dimensions + 1) dimensionsString
            !dimensions
        else
            0
        
    static member ExtractMethodInfo (expr:Expr) =
        let rec ExtractMethodInfoInner (expr) = 
            match expr with
            | Patterns.Lambda(v, e) -> 
                ExtractMethodInfoInner (e)
            | Patterns.Let (v, e1, e2) ->
                ExtractMethodInfoInner (e2)
            | Patterns.Call (e, i, a) ->
                match i with
                | DerivedPatterns.MethodWithReflectedDefinition(b) ->                    
                    (i, Array.mapi (fun i (p:ParameterInfo) -> (p, KernelManagerTools.GetKernelArrayDimensions(p.ParameterType), a.[i])) (i.GetParameters()))
                | _ ->
                    raise (CompilerException("A kernel definition must provide a function marked with ReflectedDefinition attribute"))
            | _-> 
                raise (CompilerException("Cannot find a kernel function definition inside the expression"))
        
        ExtractMethodInfoInner(expr)
        

