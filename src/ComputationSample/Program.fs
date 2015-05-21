open FSCL
open FSCL.Language
open FSCL.Runtime
open System.Reflection
open System.Reflection.Emit
open System
open System.Collections.Generic
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Linq.RuntimeHelpers
open OpenCL
open System.Diagnostics

[<EntryPoint>]
let main argv =
    // Check opencl devices
    let plats = GetOpenCLPlatforms()
    if plats.Count = 0 then
        Console.WriteLine("No OpenCL-enabled device found on this platform")
    else
        // Show OpenCL devices
        Console.WriteLine("Your OpenCL-enabled devices are listed below")
                (*
        Console.WriteLine("::::::::::: Algorithms Sample :::::::::::")
        AlgorithmSample.Run()
        Console.WriteLine(":::::::::::::::::::::::::::::::::::::::::")
        Console.WriteLine("\n::::::::::: Utility Functions Sample :::::::::::")
        UtilityFunctionSample.Run() 
        Console.WriteLine("::::::::::::::::::::::::::::::::::::::::::::::::")
        Console.WriteLine("\n::::::::::: Static/Dynamic Defines Sample :::::::::::")
        DynamicDefineSample.Run() 
        Console.WriteLine(":::::::::::::::::::::::::::::::::::::::::::::::::::::")
        Console.WriteLine("\n::::::::::: Data Types Sample :::::::::::")
        DataTypeSample.Run()
        Console.WriteLine(":::::::::::::::::::::::::::::::::::::::::")
        Console.WriteLine("\n::::::::::: Return Types Sample :::::::::::")
        ReturnTypeSample.Run()
        Console.WriteLine(":::::::::::::::::::::::::::::::::::::::::::")
        Console.WriteLine("\n::::::::::: Accelerated Collections Sample :::::::::::")
        AcceleratedCollectionSample.Run()
        Console.WriteLine("::::::::::::::::::::::::::::::::::::::::::::::::::::::")
        Console.WriteLine("\n::::::::::: Collection Data Types Sample :::::::::::")
        AcceleratedCollectionDataTypeSample.Run()
        Console.WriteLine("::::::::::::::::::::::::::::::::::::::::::::::::::::")
        Console.WriteLine("\n::::::::::: Multithread Sample :::::::::::")
        SequentialAndMultithreadSample.Run()
        Console.WriteLine(":::::::::::::::::::::::::::::::::::::::::::::::::::::::::") 
        Console.WriteLine("\n::::::::::: Collection Composition Sample :::::::::::")
        CollectionCompositionSample.Run()
        Console.WriteLine(":::::::::::::::::::::::::::::::::::::::::::::::::::::::::") 
        Console.WriteLine("\n::::::::::: Complex Composition Sample :::::::::::") *)
        CompositionSample.Run()
        Console.WriteLine(":::::::::::::::::::::::::::::::::::::::::::::::::::::::::")

    Console.WriteLine("Press Enter to exit...")
    Console.Read() |> ignore
    0


    