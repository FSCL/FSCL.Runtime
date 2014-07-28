
#r @"C:\Users\Gabriele\Desktop\FSCL.Runtime\FSCL.Runtime\bin\Debug\FSCL.Runtime.dll"
#r @"C:\Users\Gabriele\Desktop\FSCL.Runtime\FSCL.Runtime\bin\Debug\FSCL.Compiler.dll"

open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra.Double
open System.IO
open System

let lines = File.ReadAllLines(@"C:\Users\Gabriele\Documents\Visual Studio 2013\Projects\ConsoleApplication1\ConsoleApplication1\MatrixMultAdvancedTrainingSample_Features.csv")

let featureMatrix, perfMatrix = 
    seq {
        for l = 1 to lines.Length - 1 do
            let tokens = lines.[l].Split([| ';' |]) 
            let filtered = tokens |> Array.mapi(fun i e -> (i, e)) |> Array.filter(fun (i, e) -> i <> 3 && i <> 4) |> Array.unzip |> snd
            let completionTimesZip, featuresZip = filtered |> Array.mapi(fun i e -> (i, e)) |> Array.partition(fun (i, e) -> i < 3)
   
            let completionTimes = completionTimesZip |> Array.unzip |> snd |> Array.map(fun i -> Double.Parse(i))
            let features = featuresZip |> Array.unzip |> snd |> Array.map(fun i -> Double.Parse(i))
            yield (features, completionTimes)
     } |> Seq.toArray |> Array.unzip

let A = DenseMatrix.OfRowArrays featureMatrix
let b = DenseVector.OfArray (perfMatrix |> Array.map(fun a -> a.[0]))
let x = A.Svd(true).Solve(b)



    

