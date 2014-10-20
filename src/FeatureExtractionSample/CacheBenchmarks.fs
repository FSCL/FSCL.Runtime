module CacheBenchmarks

open FSCL.Compiler
open FSCL.Runtime.Scheduling
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open System.Reflection
open Microsoft.FSharp.Linq.RuntimeHelpers
//open QuotEval.QuotationEvaluation
open System
open FSCL
open FSCL.Language
open FSCL.Runtime
open FSCL.Compiler.Util
open FSCL.Compiler.AcceleratedCollections
open System.Diagnostics

[<ReflectedDefinition>]
let CacheLineSizeBenchmark(input: int[], output: int[], multipliers: int[], stride: int, ws: WorkItemInfo) =
    let mutable sum = 0
    let mutable sum2 = 0
    let i = ws.GlobalID(0)
    for j = 0 to 10 do 
        sum <- sum + input.[(i * stride) + (stride * multipliers.[j])]
        sum2 <- sum2 + input.[(i * stride) + (stride * multipliers.[j + 1])]
    output.[ws.GlobalID(0)] <- sum + sum2

[<ReflectedDefinition>]
let CacheSizeBenchmark(input: int[], output: int[], stride: int, ws: WorkItemInfo) =
    let mutable sum = 0
    let mutable sum2 = 0
    let gid = ws.GlobalID(0)
    sum <- sum + input.[(stride * 0) % input.Length] + sum2
    sum2 <- sum2 + input.[(stride * 0) % input.Length] + sum
    sum <- sum + input.[(stride * 1) % input.Length] + sum2
    sum2 <- sum2 + input.[(stride * 1) % input.Length] + sum
    sum <- sum + input.[(stride * 2) % input.Length] + sum2
    sum2 <- sum2 + input.[(stride * 2) % input.Length] + sum
    sum <- sum + input.[(stride * 3) % input.Length] + sum2
    sum2 <- sum2 + input.[(stride * 3) % input.Length] + sum
    sum <- sum + input.[(stride * 4) % input.Length] + sum2
    sum2 <- sum2 + input.[(stride * 4) % input.Length] + sum
    sum <- sum + input.[(stride * 5) % input.Length] + sum2
    sum2 <- sum2 + input.[(stride * 5) % input.Length] + sum
    sum <- sum + input.[(stride * 6) % input.Length] + sum2
    sum2 <- sum2 + input.[(stride * 6) % input.Length] + sum
    sum <- sum + input.[(stride * 7) % input.Length] + sum2
    sum2 <- sum2 + input.[(stride * 7) % input.Length] + sum
    sum <- sum + input.[(stride * 8) % input.Length] + sum2
    sum2 <- sum2 + input.[(stride * 8) % input.Length] + sum
    sum <- sum + input.[(stride * 9) % input.Length] + sum2
    sum2 <- sum2 + input.[(stride * 9) % input.Length] + sum
    sum <- sum + input.[(stride * 10) % input.Length] + sum2
    sum2 <- sum2 + input.[(stride * 10) % input.Length] + sum
    output.[ws.GlobalID(0)] <- sum + sum2

type CacheBenchmarks() =        
    static member DetermineCacheSize(p: int, d: int, ls: int) =
        let size = 8 <<< 20
        let ws = WorkSize(size |> int64, 1L)
        let output = Array.zeroCreate<int> (ws.GlobalSize(0))
        let minStride = 32
        let maxStride = size / 10
        let iterations = 10
        let mutable op = 0
        let rnd = new System.Random()
        
        for stride in minStride .. minStride .. maxStride do
            let input = Array.init (size) (fun i -> rnd.Next(i) % 10)
            let multipliers = Array.init 100 (fun i -> i)
            <@ CacheSizeBenchmark(input, output, stride, ws) @>.Run() 
            let watch = new Stopwatch()
            watch.Start()
            for i = 1 to iterations do
                <@ CacheSizeBenchmark(input, output, stride, ws) @>.Run() 
                op <- op + output.[0]
            watch.Stop()
            let ttime = ((double)watch.ElapsedMilliseconds) /(iterations |> double)
                                    
            // Dump
            Console.WriteLine("---------------- op: " + (op % 2).ToString() + ":" + String.Format("{0,11:######0.0000}", ttime) + "ms (" + String.Format("{0,10:#########0}", (stride)) + " stride)")
            
    static member DetermineCacheLineSize(p: int, d: int) =
        let ws = WorkSize(2048L, 32L)
        let output = Array.zeroCreate<int> (ws.GlobalSize(0))
        let size = 16 <<< 20
        let minLineSize = 2
        let maxLineSize = 512
        let iterations = 1000
        let mutable op = 0
        let rnd = new System.Random()

        // Random multipliers for stride (disable prefetching)
        let swap (a: _[]) x y =
            let tmp = a.[x]
            a.[x] <- a.[y]
            a.[y] <- tmp

        let shuffle a =
            Array.iteri (fun i _ -> swap a i (rnd.Next(i, Array.length a))) a

        for lineSize in minLineSize .. 2 .. maxLineSize do
            let input = Array.init (size) (fun i -> rnd.Next(i) % 10)
            let multipliers = Array.init 100 (fun i -> i)
            multipliers |> shuffle
            <@ CacheLineSizeBenchmark(input, output, multipliers, lineSize, ws) @>.Run() 
            let watch = new Stopwatch()
            watch.Start()
            for i = 1 to iterations do
                <@ CacheLineSizeBenchmark(input, output, multipliers, lineSize, ws) @>.Run() 
                op <- op + output.[0]
            watch.Stop()
            let ttime = ((double)watch.ElapsedMilliseconds) /(iterations |> double)
                                    
            // Dump
            Console.WriteLine("---------------- op: " + (op % 2).ToString() + ":" + String.Format("{0,11:######0.0000}", ttime) + "ms (" + String.Format("{0,10:#########0}", (lineSize)) + " line size)")
            