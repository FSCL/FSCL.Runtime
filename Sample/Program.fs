open FSCL.Compiler
open FSCL.Compiler.KernelLanguage
open FSCL.Runtime.HostLanguage
open FSCL.Runtime
open FSCL.Compiler.Plugins.AcceleratedCollections
open FSCL.Compiler.Configuration
open System.Reflection
open System.Reflection.Emit
open FSCL.Runtime.KernelRunner
open System
open System.Collections.Generic
open System.Diagnostics
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Linq.QuotationEvaluation
        
// Vector addition
[<Device(0,0)>][<ReflectedDefinition>]
let VectorAdd(a: float32[], b: float32[], c: float32[]) =
    let gid = get_global_id(0)
    c.[gid] <- a.[gid] + b.[gid]
    
// Vector4 addition
[<Device(0,0)>][<ReflectedDefinition>]
let Vector4Add(a: float4[], b: float4[], c: float4[]) =
    let gid = get_global_id(0)
    c.[gid] <- a.[gid] + b.[gid]
    
// Matrix addition
[<Device(0,0)>][<ReflectedDefinition>]
let MatrixAdd(a: float32[,], b: float32[,], c: float32[,]) =
    let x = get_global_id(0)

    for k = 0 to a.GetLength(0) - 1 do
        c.[x,k] <- a.[x,k] + b.[x,k]

// Matrix multiplication
[<Device(0,0)>][<ReflectedDefinition>]
let MatrixMult(a: float32[,], b: float32[,]) =
    let result = Array2D.zeroCreate (a.GetLength(0)) (b.GetLength(1))
    let x = get_global_id(0)
    let y = get_global_id(1)

    let mutable accum = 0.0f
    for k = 0 to a.GetLength(1) - 1 do
        accum <- accum + (a.[x,k] * b.[k,y])
    result.[x,y] <- accum

    result

[<EntryPoint>]
let main argv =
    let timer = new Stopwatch()
    let size = 2048
    let aaa = <@@ argv.GetLongLength(0) @@>.EvalUntyped()
    // Create buffers
    let a = Array.create size 2.0f
    let b = Array.create size 3.0f
    let mutable c = Array.zeroCreate<float32> (size)
    let correctMapResult = Array.create size 5.0f
    // Float4 vectors
    let a4 = Array.create size (float4(2.0f, 2.0f, 2.0f, 2.0f))
    let b4 = Array.create size (float4(3.0f, 3.0f, 3.0f, 3.0f))
    let c4 = Array.zeroCreate<float4> (size)
    let correctMapResult4 = Array.create size (float4(5.0f, 5.0f, 5.0f, 5.0f))
    // Matrices
    let am = Array2D.create 64 64 2.0f
    let bm = Array2D.create 64 64 3.0f
    let cm = Array2D.create 64 64 116.0f
    let dm = Array2D.zeroCreate<float32> 64 64
    let correctMatMulAdd = Array2D.create 64 64 (2.0f * 3.0f * 64.0f + 116.0f)

    // Init the runtime to include accelerated collections
    let conf = new CompilerConfiguration(true, [ SourceConfiguration(FileSource("FSCL.Compiler.AcceleratedCollections.dll")) ])
    KernelRunner.Init(new Compiler(conf), None)

    // Check opencl devices
    if KernelRunner.ListDevices().Length = 0 then
        Console.WriteLine("No OpenCL-enabled device found on this platform")
    else
        // Show OpenCL devices
        Console.WriteLine("Your OpenCL-enabled devices are listed below")
        let platforms = KernelRunner.ListDevices()
        for platformIndex = 0 to platforms.Length - 1 do
            Console.WriteLine("- Platform " + platformIndex.ToString())
            for (device, deviceName) in platforms.[platformIndex] do
                Console.WriteLine("  - Device " + device.ToString() + ": " + deviceName)
       
        // Simple vector add
        Console.WriteLine("")
        Console.WriteLine("# Testing simple vector add with OpenCL on the first device")
        // Execute vector add in OpenCL mode
        timer.Start()
        <@ VectorAdd(a, b, c) @>.RunOpenCL(size, 64) |> ignore
        timer.Stop()
        // Check result
        let mutable isResultCorrect = true
        for i = 0 to correctMapResult.Length - 1 do
            if correctMapResult.[i] <> c.[i] then
                isResultCorrect <- false
        if not isResultCorrect then
            Console.WriteLine("  First vector add returned a wrong result!")
        else
            Console.WriteLine("  First vector add execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")

            // Re-execute vector add exploiting runtime caching for kernels    
            timer.Restart()
            <@ VectorAdd(a, b, c) @>.RunOpenCL(2048, 64) |> ignore
            timer.Stop()
            Console.WriteLine("  Second vector add execution time (kernel is taken from cache): " + timer.ElapsedMilliseconds.ToString() + "ms")
                       
        // Simple vector add with embedded work size specification
        Console.WriteLine("")
        Console.WriteLine("# Testing vector add with embedded work size specification, using OpenCL on the first device")
        // Execute vector add in OpenCL mode
        timer.Start()
        <@ worksize(VectorAdd(a, b, c), [| size |], [| 64 |]) @>.Run() |> ignore
        timer.Stop()
        // Check result
        let mutable isResultCorrect = true
        for i = 0 to correctMapResult.Length - 1 do
            if correctMapResult.[i] <> c.[i] then
                isResultCorrect <- false
        if not isResultCorrect then
            Console.WriteLine("  First vector add returned a wrong result!")
        else
            Console.WriteLine("  First vector add execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")

            // Re-execute vector add exploiting runtime caching for kernels    
            timer.Restart()
            <@ VectorAdd(a, b, c) @>.RunOpenCL(2048, 64) |> ignore
            timer.Stop()
            Console.WriteLine("  Second vector add execution time (kernel is taken from cache): " + timer.ElapsedMilliseconds.ToString() + "ms")
        
        // Float4 vector add
        Console.WriteLine("")
        Console.WriteLine("# Testing float4 vector add with OpenCL on the first device")
        // Execute vector add in OpenCL mode
        timer.Start()
        <@ Vector4Add(a4, b4, c4) @>.RunOpenCL(size, 64) |> ignore
        timer.Stop()
        // Check result
        let mutable isResultCorrect = true
        for i = 0 to correctMapResult4.Length - 1 do
            if correctMapResult4.[i] <> c4.[i] then
                isResultCorrect <- false
        if not isResultCorrect then
            Console.WriteLine("  First float4 add returned a wrong result!")
        else
            Console.WriteLine("  First float4 add execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")

            // Re-execute vector add exploiting runtime caching for kernels    
            timer.Restart()
            <@ Vector4Add(a4, b4, c4) @>.RunOpenCL(size, 64) |> ignore
            timer.Stop()
            Console.WriteLine("  Second float4 add execution time (kernel is taken from cache): " + timer.ElapsedMilliseconds.ToString() + "ms")
        
        // Accelerated collection
        Console.WriteLine("")
        Console.WriteLine("# Testing accelerated vector sum on array (Array.map2 f a b) on the first device")
        timer.Start()
        c <- <@ Array.map2 (fun el1 el2 -> el1 + el2) a b @>.RunOpenCL(size, 64)
        timer.Stop()
        // Check result
        let mutable isResultCorrect = true
        for i = 0 to correctMapResult.Length - 1 do
            if correctMapResult.[i] <> c.[i] then
                isResultCorrect <- false
        if not isResultCorrect then
            Console.WriteLine("  First accelerated vector sum returned a wrong result!")
        else
            Console.WriteLine("  First accelerated vector sum execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")

            // Re-execute vector add exploiting runtime caching for kernels    
            timer.Restart()
            c <- <@ Array.map2 (fun el1 el2 -> el1 + el2) a b @>.RunOpenCL(size, 64)
            timer.Stop()
            Console.WriteLine("  Second accelerated vector sum execution time (kernel is taken from cache): " + timer.ElapsedMilliseconds.ToString() + "ms")

        // Expression of multiple kernels
        Console.WriteLine("")
        Console.WriteLine("# Testing expressions made of multiple kernels (matrix multiplication followed by a sum) on the first device")
        timer.Start()
        <@ worksize(
            MatrixAdd(
                worksize(
                    MatrixMult(am, bm), 
                    [| 64; 64 |], [| 8; 8 |]), 
                cm, dm),
            [| 64 |], [| 8 |]) @>.Run()
        timer.Stop()        
        // Check result
        let mutable isResultCorrect = true
        for i = 0 to correctMapResult.Length - 1 do
            if correctMapResult.[i] <> c.[i] then
                isResultCorrect <- false
        if not isResultCorrect then
            Console.WriteLine("  First accelerated vector sum returned a wrong result!")
        else
            Console.WriteLine("  First accelerated vector sum execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")

            // Re-execute vector add exploiting runtime caching for kernels    
            timer.Restart()
            <@ worksize(
                MatrixAdd(
                    worksize(
                        MatrixMult(am, bm), 
                        [| 64; 64 |], [| 8; 8 |]), 
                    cm, dm),
                [| 64 |], [| 8 |]) @>.Run()
            timer.Stop()
            Console.WriteLine("  Second accelerated vector sum execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")

            
            
    (*
    let a = Array.create (size) (float4(2.0f, 2.0f, 2.0f, 2.0f))
    let b = Array.create (size) (float4(3.0f, 3.0f, 3.0f, 3.0f))
    let c = Array.zeroCreate<float4> (size)
    <@@ Vector4Add(a, b, notused(c)) @@>.Run(size, 8)
    
    // Do reduce
    let mutable numberOfReduceWorkers = size / 2
    while numberOfReduceWorkers >= 4 do
        <@@ Vector4Reduce(notused(c), numberOfReduceWorkers * 2) @@>.Run(numberOfReduceWorkers, 4)
        numberOfReduceWorkers <-  numberOfReduceWorkers / 2
    <@@ Vector4Reduce(notused(c), numberOfReduceWorkers * 2) @@>.Run(numberOfReduceWorkers, 2)

    let result = c.[0].x + c.[0].y + c.[0].z + c.[0].w + c.[1].x + c.[1].y + c.[1].z + c.[1].w

    // Check computation is ok
    let correctResult = 
        Array.reduce(fun (a1:float4) (a2:float4) -> a1 + a2) (Array.map2(fun (a1:float4) (b1:float4) -> a1 + b1) a b)
    let correctValue = correctResult.x + correctResult.y + correctResult.z + correctResult.w

    Console.WriteLine("OpenCL result: " + result.ToString() + " - Correct result: " + correctValue.ToString())
    0
    *)
    Console.WriteLine("Press any key to exit...")
    Console.Read() |> ignore
    0


    