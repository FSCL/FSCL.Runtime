module AlgorithmSample

    open FSCL
    open FSCL.Language
    open FSCL.Runtime
    open System
    open System.Diagnostics
    open OpenCL

    // Vector addition
    [<Device(0,0)>][<ReflectedDefinition>]
    let VectorAdd(a: float32[], b: float32[], c: float32[], wi: WorkItemInfo) =
        let gid = wi.GlobalID(0)
        c.[gid] <- a.[gid] + b.[gid]

    // Matrix multiplication
    [<Device(0,0)>][<ReflectedDefinition>]
    let MatrixMult(a: float32[,], b: float32[,], c: float32[,], wi: WorkItemInfo) =
        let x = wi.GlobalID(0)
        let y = wi.GlobalID(1)

        let mutable accum = 0.0f
        for k = 0 to a.GetLength(1) - 1 do
            accum <- accum + (a.[y,k] * b.[k,x])
        c.[y,x] <- accum

    let Run() =    
        let FirstDeviceSupportMultiDimensionalWorkItems() =
            let device = OpenCLPlatform.Platforms.[0].Devices.[0]
            if device.MaxWorkItemDimensions > 1L then
                true
            else
                false

        let timer = new Stopwatch()

        // Vectors
        let size = 1 <<< 10
        let lsize = size |> int64
        let a = Array.create size 2.0f
        let b = Array.create size 3.0f
        let c = Array.zeroCreate<float32> (size)
        let correctMapResult = Array.create size 5.0f
        let correctReduceResult = Array.reduce (fun a b -> a + b) a
                
        // Matrices
        let matSize = 16
        let matSizel = matSize |> int64
        let am = Array2D.create matSize (matSize * 2) 2.0f
        let bm = Array2D.create (matSize * 2) matSize 3.0f
        let dm = Array2D.zeroCreate<float32> matSize matSize
        let em = Array2D.create matSize matSize 1.0f
        let correctMatMul = Array2D.create matSize matSize (2.0f * 3.0f * (matSize |> float32) * 2.0f)
        let correctMatMulAdd = Array2D.create matSize matSize (2.0f * 3.0f * (matSize |> float32) * 2.0f + 1.0f)

        // ***************************************************************************************************
        // Simple vector add ****************************************************************************
        Console.WriteLine("")
        let test = "[ Vector Add ]"
        Console.WriteLine("# Testing " + test + " with OpenCL")
        // Execute vector add in OpenCL mode
        let worksize = new WorkSize(lsize, 64L)
        timer.Start()        
        <@ VectorAdd(a, b, c, worksize) @>.Run() |> ignore
        timer.Stop()
        // Check result
        let mutable isResultCorrect = true
        for i = 0 to correctMapResult.Length - 1 do
            if correctMapResult.[i] <> c.[i] then
                isResultCorrect <- false
        if not isResultCorrect then
            Console.WriteLine("  " + test + " returned a wrong result!")
        else
            Console.WriteLine("  " + test + " execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")                       
            timer.Restart()        
            <@ VectorAdd(a, b, c, worksize) @>.Run() |> ignore
            timer.Stop()
            Console.WriteLine("  " + test + " execution time (kernel is not compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")                       
        // ***************************************************************************************************
         
        // ***************************************************************************************************
        // Matrix multiplication ****************************************************************************
        if FirstDeviceSupportMultiDimensionalWorkItems() then
            Console.WriteLine("")
            let test = "[ MatMul ]"
            Console.WriteLine("# Testing " + test + " with OpenCL")
            let worksize = new WorkSize([| matSizel; matSizel |], [| 16 |> int64; 16 |> int64 |])
            timer.Start()
            <@ MatrixMult(am, bm, dm, worksize) @>.Run()
            timer.Stop()
            // Check result
            let mutable isResultCorrect = true
            for i = 0 to correctMatMul.GetLength(0) - 1 do
                for j = 0 to correctMatMul.GetLength(1) - 1 do
                    if correctMatMul.[i, j] <> dm.[i, j] then
                        isResultCorrect <- false
            if not isResultCorrect then
                Console.WriteLine("  " + test + " returned a wrong result!")
            else
                Console.WriteLine("  " + test + " execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")
                timer.Restart()        
                <@ MatrixMult(am, bm, dm, worksize) @>.Run()
                timer.Stop()
                Console.WriteLine("  " + test + " execution time (kernel is not compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")                                
