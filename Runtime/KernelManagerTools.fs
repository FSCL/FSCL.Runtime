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
        

