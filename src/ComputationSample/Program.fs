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
open OpenCL
open System.Runtime.InteropServices
open System.Runtime

type MyStruct =
    struct
        val mutable x: float32
        val mutable y: float32
        new(a, b) = { x = a; y = b}
    end

[<StructLayout(LayoutKind.Sequential)>]
type MyRecord = {
    mutable x: float32;
    mutable y: float32    
}

// Vector addition
[<Device(0,0)>][<ReflectedDefinition>]
let VectorAdd(a: float32[], b: float32[], c: float32[], wi: WorkItemInfo) =
    let gid = wi.GlobalID(0)
    c.[gid] <- a.[gid] + b.[gid]
    
[<Device(0,0)>][<ReflectedDefinition>]
let VectorAddReturn(a: float32[], b: float32[], wi: WorkItemInfo) =
    let c = Array.zeroCreate<float32> (a.GetLength(0))
    let gid = wi.GlobalID(0)
    c.[gid] <- a.[gid] + b.[gid]
    c
    
// Vector addition with struct
[<Device(0,0)>][<ReflectedDefinition>]
let VectorAddStruct(a: MyStruct[], b: MyStruct[], c: MyStruct[], wi: WorkItemInfo) =
    let gid = wi.GlobalID(0)
    let mutable newStruct = new MyStruct()
    newStruct.x <- a.[gid].x + b.[gid].x
    newStruct.y <- a.[gid].y + b.[gid].y
    c.[gid] <- newStruct
    
[<Device(0,0)>][<ReflectedDefinition>]
let VectorAddStructConstructor(a: MyStruct[], b: MyStruct[], c: MyStruct[], wi: WorkItemInfo) =
    let gid = wi.GlobalID(0)
    let mutable newStruct = new MyStruct(a.[gid].x + b.[gid].x, a.[gid].y + b.[gid].y)
    c.[gid] <- newStruct
    
// Vector addition with record
[<Device(0,0)>][<ReflectedDefinition>]
let VectorAddRecord(a: MyRecord[], b: MyRecord[], c: MyRecord[], wi: WorkItemInfo) =
    let gid = wi.GlobalID(0)
    let newRecord = { x = a.[gid].x + b.[gid].x; y = a.[gid].y + b.[gid].y }
    c.[gid] <- newRecord

[<ReflectedDefinition>]
let sum(a, b) =
    a + b
[<Device(0,0)>][<ReflectedDefinition>]
let VectorAddWithUtilityFunction(a: float32[], b: float32[], c: float32[], wi:WorkItemInfo) =
    let gid = wi.GlobalID(0)
    c.[gid] <- sum(a.[gid], b.[gid])

// Vector4 addition
[<Device(0,0)>][<ReflectedDefinition>]

let Vector4Add(a: float4[], b: float4[], c: float4[], wi: WorkItemInfo) =
    let gid = wi.GlobalID(0)
    c.[gid] <- a.[gid] + b.[gid]

// Matrix addition
[<Device(0,0)>][<ReflectedDefinition>]
let MatrixAdd(a: float32[,], b: float32[,], c: float32[,], wi: WorkItemInfo) =
    let x = wi.GlobalID(0)

    for k = 0 to a.GetLength(0) - 1 do
        c.[x,k] <- a.[x,k] + b.[x,k]

// Matrix multiplication
[<Device(0,0)>][<ReflectedDefinition>]
let MatrixMult(a: float32[,], b: float32[,], wi: WorkItemInfo) =
    let result = Array2D.zeroCreate (a.GetLength(0)) (b.GetLength(1))
    let x = wi.GlobalID(0)
    let y = wi.GlobalID(1)

    let mutable accum = 0.0f
    for k = 0 to a.GetLength(1) - 1 do
        accum <- accum + (a.[x,k] * b.[k,y])
    result.[x,y] <- accum

    result
    
// Matrix multiplication with local and reference to global var (BLOCK_SIZE)
[<ReflectedDefinition>]
let BLOCK_SIZE = 16
[<ReflectedDefinition>]
let MatrixMultAdvanced(matA: float32[,], matB: float32[,], matC: float32[,], wi: WorkItemInfo) =
    let bx = wi.GroupID(0)
    let by = wi.GroupID(1) 
    let tx = wi.LocalID(0)
    let ty = wi.LocalID(1)
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
        wi.Barrier(CLK_LOCAL_MEM_FENCE)
 
        for k = 0 to BLOCK_SIZE - 1 do
            Csub <- Csub + (As.[ty,k] * Bs.[k,tx])
        wi.Barrier(CLK_LOCAL_MEM_FENCE)

        bRow <- bRow + bStep
    matC.[by * BLOCK_SIZE + ty, bx * BLOCK_SIZE + tx] <- Csub
 
[<ReflectedDefinition>]
let sumCurried a b =
    a + b

let SimpleSequentialComp(a:float32[], b:float32[]) =
    let c = Array.zeroCreate<float32> (a.Length/2)
    for i = 0 to a.Length / 2 - 1 do
        c.[i] <- a.[i] + b.[i]
    c

let FirstDeviceSupportMultiDimensionalWorkItems() =
    let device = OpenCLPlatform.Platforms.[0].Devices.[0]
    if device.MaxWorkItemDimensions > 1L then
        true
    else
        false

[<EntryPoint>]
let main argv =
    let timer = new Stopwatch()
    let size = 1 <<< 10
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
    // Struct vectors
    let aStruct = Array.create size (new MyStruct(1.0f, 2.0f))
    let bStruct = Array.create size (new MyStruct(4.0f, 3.0f))
    let mutable cStruct = Array.zeroCreate<MyStruct> (size)
    let correctMapResultStruct = Array.create size (new MyStruct(5.0f, 5.0f))
    // Record vectors
    let aRecord = Array.create size ({ x = 1.0f; y = 2.0f })
    let bRecord = Array.create size ({ x = 4.0f; y = 3.0f })
    let mutable cRecord = Array.create size ({ x = 4.0f; y = 3.0f })
    let correctMapResultRecord = Array.create size ({ x = 5.0f; y = 5.0f })
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
    let opts = new Dictionary<string, obj>()
    //opts.Add(CompilerOptions.VerboseLevel, 2)

    // Check opencl devices
    let plats = GetOpenCLPlatforms()
    if plats.Count = 0 then
        Console.WriteLine("No OpenCL-enabled device found on this platform")
    else
        // Show OpenCL devices
        Console.WriteLine("Your OpenCL-enabled devices are listed below")
        
        for pIndex, pName, devs in plats do
            Console.WriteLine("- Platform " + pIndex.ToString())
            for dIndex, dName, dType in devs do
                Console.WriteLine("  - Device " + ": " + dName + "(" + dType.ToString() + ")")
       
        // Simple vector add
        Console.WriteLine("")
        Console.WriteLine("# Testing simple vector add with OpenCL on the first device")
        // Execute vector add in OpenCL mode
        c <- Array.zeroCreate<float32> (size)
        let worksize = new WorkSize(lsize, 64L)
        timer.Start()        
        <@ VectorAdd(a, b, c, worksize) @>.Run(opts) |> ignore
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
            <@ VectorAdd(a, b, c, worksize) @>.Run() |> ignore
            timer.Stop()
            Console.WriteLine("  Second vector add execution time (kernel is taken from cache): " + timer.ElapsedMilliseconds.ToString() + "ms")
                              
        // Simple vector add with utility function
        Console.WriteLine("")
        Console.WriteLine("# Testing vector add with utility function with OpenCL on the first device")
        // Execute vector add in OpenCL mode
        c <- Array.zeroCreate<float32> (size)
        timer.Start()
        <@ VectorAddWithUtilityFunction(a, b, c, worksize) @>.Run(opts) |> ignore
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
            <@ VectorAddWithUtilityFunction(a, b, c, worksize) @>.Run() |> ignore
            timer.Stop()
            Console.WriteLine("  Second vector add execution time (kernel is taken from cache): " + timer.ElapsedMilliseconds.ToString() + "ms")
                       
        // Float4 vector add
        Console.WriteLine("")
        Console.WriteLine("# Testing float4 vector add with OpenCL on the first device")
        // Execute vector add in OpenCL mode
        timer.Start()
        <@ Vector4Add(a4, b4, c4, worksize) @>.Run(opts) |> ignore
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
            <@ Vector4Add(a4, b4, c4, worksize) @>.Run() |> ignore
            timer.Stop()
            Console.WriteLine("  Second float4 add execution time (kernel is taken from cache): " + timer.ElapsedMilliseconds.ToString() + "ms")
        
        // Vector add with struct
        Console.WriteLine("")
        Console.WriteLine("# Testing struct vector add with OpenCL on the first device")
        // Execute vector add in OpenCL mode
        let worksize = new WorkSize(lsize, 64L)
        timer.Start()        
        <@ VectorAddStruct(aStruct, bStruct, cStruct, worksize) @>.Run(opts) |> ignore
        timer.Stop()
        // Check result
        let mutable isResultCorrect = true
        for i = 0 to correctMapResultStruct.Length - 1 do
            if correctMapResultStruct.[i] <> cStruct.[i] then
                isResultCorrect <- false
        if not isResultCorrect then
            Console.WriteLine("  First struct vector add returned a wrong result!")
        else
            Console.WriteLine("  First struct vector add execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")

            // Re-execute vector add exploiting runtime caching for kernels    
            cStruct <- Array.zeroCreate<MyStruct> (size)
            timer.Restart()
            <@ VectorAddStruct(aStruct, bStruct, cStruct, worksize) @>.Run() |> ignore
            timer.Stop()
            Console.WriteLine("  Second struct vector add execution time (kernel is taken from cache): " + timer.ElapsedMilliseconds.ToString() + "ms")
        
        // Vector add with struct with contructor
        Console.WriteLine("")
        Console.WriteLine("# Testing struct with non-default constructor vector add with OpenCL on the first device")
        // Execute vector add in OpenCL mode
        let worksize = new WorkSize(lsize, 64L)
        timer.Start()        
        <@ VectorAddStructConstructor(aStruct, bStruct, cStruct, worksize) @>.Run(opts) |> ignore
        timer.Stop()
        // Check result
        let mutable isResultCorrect = true
        for i = 0 to correctMapResultStruct.Length - 1 do
            if correctMapResultStruct.[i] <> cStruct.[i] then
                isResultCorrect <- false
        if not isResultCorrect then
            Console.WriteLine("  First struct with non-default constructor vector add returned a wrong result!")
        else
            Console.WriteLine("  First struct with non-default constructor vector add execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")

            // Re-execute vector add exploiting runtime caching for kernels    
            cStruct <- Array.zeroCreate<MyStruct> (size)
            timer.Restart()
            <@ VectorAddStructConstructor(aStruct, bStruct, cStruct, worksize) @>.Run() |> ignore
            timer.Stop()
            Console.WriteLine("  Second struct with non-default constructor vector add execution time (kernel is taken from cache): " + timer.ElapsedMilliseconds.ToString() + "ms")
                                     
        // Vector add with record
        Console.WriteLine("")
        Console.WriteLine("# Testing record vector add with OpenCL on the first device")
        // Execute vector add in OpenCL mode
        let worksize = new WorkSize(lsize, 64L)
        timer.Start()        
        <@ VectorAddRecord(aRecord, bRecord, cRecord, worksize) @>.Run(opts) |> ignore
        timer.Stop()
        // Check result
        let mutable isResultCorrect = true
        for i = 0 to correctMapResultRecord.Length - 1 do
            if correctMapResultRecord.[i] <> cRecord.[i] then
                isResultCorrect <- false
        if not isResultCorrect then
            Console.WriteLine("  First record vector add returned a wrong result!")
        else
            Console.WriteLine("  First record vector add execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")

            // Re-execute vector add exploiting runtime caching for kernels    
            cRecord <- Array.zeroCreate<MyRecord> (size)
            timer.Restart()
            <@ VectorAddRecord(aRecord, bRecord, cRecord, worksize) @>.Run(opts) |> ignore
            timer.Stop()
            Console.WriteLine("  Second record vector add execution time (kernel is taken from cache): " + timer.ElapsedMilliseconds.ToString() + "ms")
                                           
        // Matrix multiplication
        if FirstDeviceSupportMultiDimensionalWorkItems() then
            Console.WriteLine("")
            Console.WriteLine("# Testing advanced matrix multiplication with local vectors")
            let worksize = new WorkSize([| matSizel; matSizel |], [| BLOCK_SIZE |> int64; BLOCK_SIZE |> int64 |])
            timer.Start()
            <@ MatrixMultAdvanced(am, bm, dm, worksize) @>.Run(opts)
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
                <@ MatrixMultAdvanced(am, bm, dm, worksize) @>.Run()
                timer.Stop()
                Console.WriteLine("  Second advanced matrix multiplication execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")

        // Accelerated collection
        Console.WriteLine("")
        Console.WriteLine("# Testing accelerated vector sum on array (Array.map2 f a b) on the first device")
        c <- Array.zeroCreate<float32> (size)
        timer.Start()
        c <- <@ Array.map2 (fun el1 el2 -> el1 + el2) a b @>.Run(opts)
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
            c <- <@ Array.map2 (fun el1 el2 -> el1 + el2) a b @>.Run(opts)
            timer.Stop()
            Console.WriteLine("  Second accelerated vector sum execution time (kernel is taken from cache): " + timer.ElapsedMilliseconds.ToString() + "ms")

        // Accelerated collection reduce
        Console.WriteLine("")
        Console.WriteLine("# Testing accelerated vector reduce on array (Array.reduce f a) on the first device")
        timer.Start()
        let mutable reduce_sum = <@ Array.reduce sumCurried a @>.Run(opts)
        timer.Stop()
        // Check result
        let isResultCorrect = (reduce_sum = correctReduceResult)
        if not isResultCorrect then
            Console.WriteLine("  First accelerated vector reduce returned a wrong result!")
        else
            Console.WriteLine("  First accelerated vector reduce execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")

            // Re-execute vector add exploiting runtime caching for kernels    
            timer.Restart()
            reduce_sum <- <@ Array.reduce sumCurried a @>.Run(opts)
            timer.Stop()
            Console.WriteLine("  Second accelerated vector reduce execution time (kernel is taken from cache): " + timer.ElapsedMilliseconds.ToString() + "ms")
            
        // Accelerated collection reduce (lambda fun)
        Console.WriteLine("")
        Console.WriteLine("# Testing accelerated vector reduce on array (Array.reduce f a) with lambda fun on the first device")
        timer.Start()
        let mutable reduce_sum = <@ Array.reduce (fun e1 e2 -> e1 + e2) a @>.Run(opts)
        timer.Stop()
        // Check result
        let isResultCorrect = (reduce_sum = correctReduceResult)
        if not isResultCorrect then
            Console.WriteLine("  First accelerated vector reduce returned a wrong result!")
        else
            Console.WriteLine("  First accelerated vector reduce execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")

            // Re-execute vector add exploiting runtime caching for kernels    
            timer.Restart()
            reduce_sum <- <@ Array.reduce (fun el1 el2 -> el1 + el2) a @>.Run(opts)
            timer.Stop()
            Console.WriteLine("  Second accelerated vector reduce execution time (kernel is taken from cache): " + timer.ElapsedMilliseconds.ToString() + "ms")
            
        if FirstDeviceSupportMultiDimensionalWorkItems() then
            // Expression of multiple kernels
            Console.WriteLine("")
            Console.WriteLine("# Testing expression made of multiple kernels (matrix multiplication followed by a sum) on the first device")
            let multWorkSize = new WorkSize([| matSizel; matSizel |], [| 8L; 8L |])
            let addWorkSize = new WorkSize(matSizel, 8L)
            timer.Start()
            let r = <@ MatrixAdd(
                            MatrixMult(am, bm, multWorkSize),
                            cm, dm, addWorkSize)
                     @>.Run(opts)
            timer.Stop()
            // Check result
            let mutable isResultCorrect = true
            for i = 0 to correctMatMulAdd.GetLength(0) - 1 do
                for j = 0 to correctMatMulAdd.GetLength(1) - 1 do
                    if correctMatMulAdd.[i, j] <> dm.[i, j] then
                        isResultCorrect <- false
            if not isResultCorrect then
                Console.WriteLine("  Multikernel returned a wrong result!")
            else
                Console.WriteLine("  First multikernel execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")

        // Sequential computation
        Console.WriteLine("")
        Console.WriteLine("# Testing expressions containing a sequential (not reflected) function call")
        timer.Restart()
        let r = <@ SimpleSequentialComp(a, b) @>.Run()
        timer.Stop()
        Console.WriteLine("  Sequential function execution time: " + timer.ElapsedMilliseconds.ToString() + "ms")
        
        // Composition of collection functions and custom kernel
        let workSize = new WorkSize(a.LongLength, 128L)
        Console.WriteLine("")
        Console.WriteLine("# Testing expressions containing a collection functions, custom kernels and sequential computations")
        timer.Restart()
        let r = <@ Array.reduce sumCurried 
                                (SimpleSequentialComp ((Array.map2 sumCurried a b), 
                                                        VectorAddReturn(a, b, worksize))) @>.Run()
        timer.Stop()
        Console.WriteLine("  Expression execution time: " + timer.ElapsedMilliseconds.ToString() + "ms")
        timer.Restart()
        let r = <@ Array.reduce sumCurried 
                                (SimpleSequentialComp ((Array.map2 sumCurried a b), 
                                                        VectorAddReturn(a, b, workSize))) @>.Run()
        timer.Stop()
        Console.WriteLine("  Expression execution time (kernel is not compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")
        
        
        // Composition of collection functions and custom kernel
        Console.WriteLine("")
        Console.WriteLine("# Testing expression with sub-executor")
        // This is a wrapper for a multi-kernel computation
        let MapReduce (a:float32[]) (b:float32[]) =
            <@ Array.reduce sumCurried (Array.map2 sumCurried a b) @>.Run()

        timer.Restart()
        let r = <@ MapReduce (VectorAddReturn(a, b, workSize)) b @>.Run()
        timer.Stop()
        Console.WriteLine("  Expression with sub-executor execution time: " + timer.ElapsedMilliseconds.ToString() + "ms")
        timer.Restart()
        let r = <@ MapReduce (VectorAddReturn(a, b, workSize)) b @>.Run()
        timer.Stop()
        Console.WriteLine("  Expression with sub-executor execution time (kernel is not compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")

    Console.WriteLine("Press Enter to exit...")
    Console.Read() |> ignore
    0


    