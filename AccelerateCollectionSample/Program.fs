open FSCL
open FSCL.Language
open FSCL.Runtime
open System.Reflection
open System.Reflection.Emit
open System
open System.Collections.Generic
open System.Diagnostics
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Linq.RuntimeHelpers

[<ReflectedDefinition>]
let Distance (p1 : int2) (p2:int2) =
    let d = p1 - p2
    let d2 = d * d
    sqrt(float32 (d2.x + d2.y))    
    
        
[<ReflectedDefinition>]
let MatrixxVector (m:float32[,]) (v:float32[]) =
    let result = Array.zeroCreate<float32> (m.GetLength(0))

    let myId = get_global_id(0)
    let mutable sum = 0.0f
    for k = 0 to m.GetLength(1) - 1 do
        sum <- sum + m.[myId, k] * v.[k]
    result.[myId] <- sum

    result

[<ReflectedDefinition>]
let VectorAdd (a:float32[]) (b:float32[]) =
    let c = Array.zeroCreate<float32> a.Length
    let myId = get_global_id(0)
    c.[myId] <- a.[myId] + b.[myId]
    c
    
[<ReflectedDefinition>]
let Reduction ([<MemoryFlags(MemoryFlags.ReadWrite)>]input:float32[]) ([<MemoryFlags(MemoryFlags.ReadWrite)>]output:float32[]) =
    let myId = get_global_id(0)
    if (myId * 2 < input.Length) then
        output.[myId] <- input.[myId] + input.[myId + 1]

let ReductionExecutor (input:float32[]) =
    let output = Array.zeroCreate<float32> (input.Length / 2)
    let mutable currentOutputSize = output.Length
    let mutable currentInput = input
    while (currentOutputSize > 1) do
        <@ Reduction currentInput output @>.Run(output.LongLength, if currentOutputSize > 16 then 16L else 1L)
        currentOutputSize <- currentOutputSize / 2
        currentInput <- output
    output.[0]
    
[<EntryPoint>]
let main argv = 
    let array = Array.create 1024 2.0f
    let array2 = Array.create 1024 3.0f
    let array3 = Array.init 1024 (fun i -> i |> float32)

    // Array map
    Console.Write("1) Testing Array.map...")
    let result = <@ Array.map (fun a -> a + 1.0f) array @>.Run(array.LongLength, 128L);
    if result = Array.map (fun a -> a + 1.0f) array then
        Console.WriteLine("OK!")
    else
        Console.WriteLine("ERROR!")
        
    // Array map2
    Console.Write("2) Testing Array.map2...")
    let result = <@ Array.map2 (fun a b -> a * b) array array2 @>.Run(array.LongLength, 128L);
    if result = Array.map2 (fun a b -> a * b) array array2 then
        Console.WriteLine("OK!")
    else
        Console.WriteLine("ERROR!")
        
    // Array mapi
    Console.Write("3) Testing Array.mapi...")
    let result = <@ Array.mapi (fun i a -> if i % 2 = 0 then a + 1.0f else a - 1.0f) array @>.Run(array.LongLength, 128L);
    if result = Array.mapi (fun i a -> if i % 2 = 0 then a + 1.0f else a - 1.0f) array then
        Console.WriteLine("OK!")
    else
        Console.WriteLine("ERROR!")
        
    // Array mapi2
    Console.Write("4) Testing Array.mapi2...")
    let result = <@ Array.mapi2 (fun i a b -> if i % 2 = 0 then a + b else a - b) array array2 @>.Run(array.LongLength, 128L);
    if result = Array.mapi2 (fun i a b -> if i % 2 = 0 then a + b else a - b) array array2 then
        Console.WriteLine("OK!")
    else
        Console.WriteLine("ERROR!")
        
    // Array reduce
    Console.Write("5) Testing Array.reduce...")
    let result = <@ Array.reduce (fun a b -> a + b) array @>.Run(array.LongLength, 128L);
    if result = Array.reduce (fun a b -> a + b) array then
        Console.WriteLine("OK!")
    else
        Console.WriteLine("ERROR!")
        
    // Array reverse
    Console.Write("6) Testing Array.sum...")
    let result = <@ Array.sum array @>.Run(array.LongLength, 128L);
    if result = Array.sum array then
        Console.WriteLine("OK!")
    else
        Console.WriteLine("ERROR!")

    // Array reverse
    Console.Write("7) Testing Array.rev...")
    let result = <@ Array.rev array @>.Run(array.LongLength, 128L);
    if result = Array.rev array then
        Console.WriteLine("OK!")
    else
        Console.WriteLine("ERROR!")

    // Combination
    Console.Write("8) Testing kernel expression with collection functions...")
    let result = <@ Array.rev (Array.map2 (fun a b -> a * b) (Array.map (fun a -> a + 1.0f) array) (Array.mapi (fun i a -> a + float32(i)) array2)) @>.Run(array.LongLength, 128L);
    if result = Array.rev (Array.map2 (fun a b -> a * b) (Array.map (fun a -> a + 1.0f) array) (Array.mapi (fun i a -> a + float32(i)) array2)) then
        Console.WriteLine("OK!")
    else
        Console.WriteLine("ERROR!")

    // Other example
    Console.Write("9) Testing another kernel expression with collection functions...")
    let points = Array.init 1024 (fun i -> int2(i, i * 2))
    let xy = int2(1, 0)
    let result = <@ Array.sum (Array.map (fun p -> Distance xy p) points) @>.Run(array.LongLength, 128L)
    if result = Array.sum (Array.map (fun p -> Distance xy p) points) then
        Console.WriteLine("OK!")
    else
        Console.WriteLine("ERROR!")
        

    let v2 = Array.create 64 1.0f
    let r = <@ ReductionExecutor v2 @>.Run()

    Console.WriteLine("Press enter to exit...")
    Console.Read() |> ignore
    0
