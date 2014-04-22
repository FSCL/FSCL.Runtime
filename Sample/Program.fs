open FSCL.Compiler
open FSCL.Compiler.Language
open FSCL.Runtime.Language
open FSCL.Runtime
open FSCL.Compiler.Plugins.AcceleratedCollections
open FSCL.Compiler.Configuration
open System.Reflection
open System.Reflection.Emit
open System
open System.Collections.Generic
open System.Diagnostics
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Linq.RuntimeHelpers
        
// Vector addition
[<Device(0,0)>][<ReflectedDefinition>]
let VectorAdd(a: float32[], b: float32[], c: float32[]) =
    let gid = get_global_id(0)
    c.[gid] <- a.[gid] + b.[gid]
    
[<Device(0,0)>][<ReflectedDefinition>]
let VectorAddReturn(a: float32[], b: float32[]) =
    let c = Array.zeroCreate<float32> (a.GetLength(0))
    let gid = get_global_id(0)
    c.[gid] <- a.[gid] + b.[gid]
    c
    
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
    
// Matrix multiplication with local and reference to global var (BLOCK_SIZE)
[<ReflectedDefinition>]
let BLOCK_SIZE = 16
[<ReflectedDefinition>]
let MatrixMultAdvanced(matA: float32[,], matB: float32[,], matC: float32[,]) =
    let bx = get_group_id(0)
    let by = get_group_id(1) 
    let tx = get_local_id(0)
    let ty = get_local_id(1)
    let wa = matA.GetLength(0)
    let wb = matB.GetLength(0)

    let bCol = bx * BLOCK_SIZE
    let bBeginRow = 0
    let bStep  = BLOCK_SIZE
    let mutable bRow = bBeginRow
    let mutable Csub = 0.0f
 
    let As = local(Array2D.zeroCreate<float32> BLOCK_SIZE BLOCK_SIZE)
    let Bs = local(Array2D.zeroCreate<float32> BLOCK_SIZE BLOCK_SIZE)

    for aCol in 0 .. BLOCK_SIZE .. (wa - 1) do
        As.[ty, tx] <- matA.[by * BLOCK_SIZE, aCol]
        Bs.[ty, tx] <- matB.[bRow, bCol]
        barrier(CLK_LOCAL_MEM_FENCE)
 
        for k = 0 to BLOCK_SIZE - 1 do
            Csub <- Csub + (As.[ty,k] * Bs.[k,tx])
        barrier(CLK_LOCAL_MEM_FENCE)

        bRow <- bRow + bStep
    matC.[by * BLOCK_SIZE + ty, bx * BLOCK_SIZE + tx] <- Csub
 
[<ReflectedDefinition>]
let sum a b =
    a + b

[<EntryPoint>]
let main argv =
    let timer = new Stopwatch()
    let size = 1 <<< 20
    let lsize = size |> int64
    // Create buffers
    let a = Array.create size 2.0f
    let b = Array.create size 3.0f
    let mutable c = Array.zeroCreate<float32> (size)
    let correctMapResult = Array.create size 5.0f
    let correctReduceResult = Array.reduce (fun a b -> a + b) a
    // Float4 vectors
    let a4 = Array.create size (float4(2.0f, 2.0f, 2.0f, 2.0f))
    let b4 = Array.create size (float4(3.0f, 3.0f, 3.0f, 3.0f))
    let c4 = Array.zeroCreate<float4> (size)
    let correctMapResult4 = Array.create size (float4(5.0f, 5.0f, 5.0f, 5.0f))
    // Matrices
    let matSize = 32
    let matSizel = matSize |> int64
    let am = Array2D.create matSize matSize 2.0f
    let bm = Array2D.create matSize matSize 3.0f
    let cm = Array2D.create matSize matSize 116.0f
    let dm = Array2D.zeroCreate<float32> matSize matSize
    let correctMatMul = Array2D.create matSize matSize (2.0f * 3.0f * (matSize |> float32))
    let correctMatMulAdd = Array2D.create matSize matSize (2.0f * 3.0f * (matSize |> float32) + 116.0f)

    // Init the runtime to include accelerated collections
    let conf = new PipelineConfiguration(true, [| SourceConfiguration(FileSource("FSCL.Compiler.AcceleratedCollections.dll")) |])
    let compiler = new Compiler(conf)
    Init(compiler, None)

    // Check opencl devices
    let plats = ListDevices()
    if plats.Count = 0 then
        Console.WriteLine("No OpenCL-enabled device found on this platform")
    else
        // Show OpenCL devices
        Console.WriteLine("Your OpenCL-enabled devices are listed below")
        
        for platformIndex = 0 to plats.Count - 1 do
            Console.WriteLine("- Platform " + platformIndex.ToString())
            for deviceName in plats.[platformIndex] do
                Console.WriteLine("  - Device " + ": " + deviceName)
       
        // Simple vector add
        Console.WriteLine("")
        Console.WriteLine("# Testing simple vector add with OpenCL on the first device")
        // Execute vector add in OpenCL mode
        c <- Array.zeroCreate<float32> (size)
        timer.Start()
        <@ VectorAdd(a, b, c) @>.Run(lsize, 64L) |> ignore
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
            c <- Array.zeroCreate<float32> (size)
            timer.Restart()
            <@ VectorAdd(a, b, c) @>.Run(lsize, 64L) |> ignore
            timer.Stop()
            Console.WriteLine("  Second vector add execution time (kernel is taken from cache): " + timer.ElapsedMilliseconds.ToString() + "ms")
                       
        // Simple vector add with embedded work size specification
        Console.WriteLine("")
        Console.WriteLine("# Testing vector add with embedded work size specification, using OpenCL on the first device")
        // Execute vector add in OpenCL mode
        c <- Array.zeroCreate<float32> (size)
        timer.Start()
        <@ WORKSIZE([| lsize |], [| 64L |], VectorAdd(a, b, c)) @>.Run() |> ignore
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
            c <- Array.zeroCreate<float32> (size)
            timer.Restart()
            <@ WORKSIZE([| lsize |], [| 64L |], VectorAdd(a, b, c)) @>.Run() |> ignore
            timer.Stop()
            Console.WriteLine("  Second vector add execution time (kernel is taken from cache): " + timer.ElapsedMilliseconds.ToString() + "ms")
        
        // Float4 vector add
        Console.WriteLine("")
        Console.WriteLine("# Testing float4 vector add with OpenCL on the first device")
        // Execute vector add in OpenCL mode
        timer.Start()
        <@ Vector4Add(a4, b4, c4) @>.Run(lsize, 64L) |> ignore
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
            <@ Vector4Add(a4, b4, c4) @>.Run(lsize, 64L) |> ignore
            timer.Stop()
            Console.WriteLine("  Second float4 add execution time (kernel is taken from cache): " + timer.ElapsedMilliseconds.ToString() + "ms")
        
        // Matrix multiplication
        Console.WriteLine("")
        Console.WriteLine("# Testing advanced matrix multiplication with local vectors")
        timer.Start()
        <@ MatrixMultAdvanced(am, bm, dm) @>.Run([| matSizel; matSizel |], [| BLOCK_SIZE |> int64; BLOCK_SIZE |> int64 |])
        timer.Stop()
        // Check result
        let mutable isResultCorrect = true
        for i = 0 to correctMatMul.GetLength(0) - 1 do
            for j = 0 to correctMatMul.GetLength(1) - 1 do
                if correctMatMul.[i, j] <> dm.[i, j] then
                    isResultCorrect <- false
        if not isResultCorrect then
            Console.WriteLine("  First advanced matrix multiplication returned a wrong result!")
        else
            Console.WriteLine("  First advanced matrix multiplication execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")

            // Re-execute vector add exploiting runtime caching for kernels    
            timer.Restart()
            <@ MatrixMultAdvanced(am, bm, dm) @>.Run([| matSizel; matSizel |], [| BLOCK_SIZE |> int64; BLOCK_SIZE |> int64 |])
            timer.Stop()
            Console.WriteLine("  Second advanced matrix multiplication execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")

        // Accelerated collection
        Console.WriteLine("")
        Console.WriteLine("# Testing accelerated vector sum on array (Array.map2 f a b) on the first device")
        c <- Array.zeroCreate<float32> (size)
        timer.Start()
        c <- <@ Array.map2 (fun el1 el2 -> el1 + el2) a b @>.Run(lsize, 64L)
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
            c <- Array.zeroCreate<float32> (size) 
            timer.Restart()
            c <- <@ Array.map2 (fun el1 el2 -> el1 + el2) a b @>.Run(lsize, 64L)
            timer.Stop()
            Console.WriteLine("  Second accelerated vector sum execution time (kernel is taken from cache): " + timer.ElapsedMilliseconds.ToString() + "ms")

        // Accelerated collection reduce
        Console.WriteLine("")
        Console.WriteLine("# Testing accelerated vector reduce on array (Array.reduce f a) on the first device")
        timer.Start()
        let mutable reduce_sum = <@ Array.reduce sum a @>.Run(lsize, 64L)
        timer.Stop()
        // Check result
        let isResultCorrect = (reduce_sum = correctReduceResult)
        if not isResultCorrect then
            Console.WriteLine("  First accelerated vector reduce returned a wrong result!")
        else
            Console.WriteLine("  First accelerated vector reduce execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")

            // Re-execute vector add exploiting runtime caching for kernels    
            timer.Restart()
            reduce_sum <- <@ Array.reduce sum a @>.Run(lsize, 64L)
            timer.Stop()
            Console.WriteLine("  Second accelerated vector reduce execution time (kernel is taken from cache): " + timer.ElapsedMilliseconds.ToString() + "ms")
            
        // Accelerated collection reduce (lambda fun)
        Console.WriteLine("")
        Console.WriteLine("# Testing accelerated vector reduce on array (Array.reduce f a) with lambda fun on the first device")
        timer.Start()
        let mutable reduce_sum = <@ Array.reduce (fun e1 e2 -> e1 + e2) a @>.Run(lsize, 64L)
        timer.Stop()
        // Check result
        let isResultCorrect = (reduce_sum = correctReduceResult)
        if not isResultCorrect then
            Console.WriteLine("  First accelerated vector reduce returned a wrong result!")
        else
            Console.WriteLine("  First accelerated vector reduce execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")

            // Re-execute vector add exploiting runtime caching for kernels    
            timer.Restart()
            reduce_sum <- <@ Array.reduce (fun el1 el2 -> el1 + el2) a @>.Run(lsize, 64L)
            timer.Stop()
            Console.WriteLine("  Second accelerated vector reduce execution time (kernel is taken from cache): " + timer.ElapsedMilliseconds.ToString() + "ms")
            
        // Expression of multiple kernels
        Console.WriteLine("")
        Console.WriteLine("# Testing expressions made of multiple kernels (matrix multiplication followed by a sum) on the first device")
        timer.Start()
        let r = <@ WORKSIZE([| matSizel |], [| 8L |],
                    MatrixAdd(
                        WORKSIZE(
                            [| matSizel; matSizel |], [| 8L; 8L |], 
                            MatrixMult(am, bm)),
                        cm, dm)
                    ) @>.Run()
        timer.Stop()
        // Check result
        let mutable isResultCorrect = true
        for i = 0 to correctMatMulAdd.GetLength(0) - 1 do
            for j = 0 to correctMatMulAdd.GetLength(1) - 1 do
                if correctMatMulAdd.[i, j] <> dm.[i, j] then
                    isResultCorrect <- false
        if not isResultCorrect then
            Console.WriteLine("  First accelerated vector sum returned a wrong result!")
        else
            Console.WriteLine("  First accelerated vector sum execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")

            // Re-execute vector add exploiting runtime caching for kernels    
            timer.Restart()
            <@ WORKSIZE([| matSizel |], [| 8L |],
                MatrixAdd(
                    WORKSIZE(
                        [| matSizel; matSizel |], [| 8L; 8L |], 
                        MatrixMult(am, bm)),
                    cm, dm)
                ) @>.Run() |> ignore
            timer.Stop()
            Console.WriteLine("  Second accelerated vector sum execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")

    Console.WriteLine("Press Enter to exit...")
    Console.Read() |> ignore
    0


    