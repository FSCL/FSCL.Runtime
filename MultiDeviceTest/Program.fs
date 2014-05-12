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
    let minVectorSize = 2L <<< 10
    let maxVectorSize = 4L <<< 10
    let minMatSize = 64L
    let maxMatSize = 2048L
    let perTestDuration = 2000.0

    // Vector add
    (*
    Console.WriteLine(":::::::::::::::: Vector Addition ::::::::::::::::")
    VectorAddTest.DoTest(minVectorSize, maxVectorSize, 100)
    Console.WriteLine(":::::::::::::::::::::::::::::::::::::::::::::::::")

    Console.WriteLine("::::::::::::: Matrix Multiplication :::::::::::::")
    MatrixMultTest.DoTest(minMatSize, maxMatSize, 100)
    Console.WriteLine(":::::::::::::::::::::::::::::::::::::::::::::::::")
    *)
    Console.WriteLine(":::::::::::::::: Vector Reduction :::::::::::::::")
    ReduceTest.DoTest(minVectorSize, maxVectorSize, 10)
    Console.WriteLine(":::::::::::::::::::::::::::::::::::::::::::::::::")

    Console.Read() |> ignore
    0

            
            

