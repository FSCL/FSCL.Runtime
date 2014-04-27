open FSCL.Compiler.Language
open FSCL.Runtime.Language
open FSCL.Runtime
open FSCL.Runtime.MetricTools
open System
open Kernels
open VectorAddTest
    
[<EntryPoint>]
let main argv = 
    // Min and max size
    let minVectorSize = 8L <<< 20
    let maxVectorSize = 8L <<< 20
    let perTestDuration = 5000.0

    // Vector add
    Console.WriteLine(":::::::::::::::: Vector Addition ::::::::::::::::")
    VectorAddTest.DoTest(minVectorSize, maxVectorSize, perTestDuration)
    Console.WriteLine(":::::::::::::::::::::::::::::::::::::::::::::::::")
    Console.Read() |> ignore
    0

            
            

