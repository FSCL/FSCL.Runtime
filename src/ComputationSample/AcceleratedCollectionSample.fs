module AcceleratedCollectionSample

    open FSCL
    open FSCL.Language
    open FSCL.Runtime
    open System
    open System.Diagnostics
    open OpenCL
    
    [<ReflectedDefinition>]
    let sumCurried a b =
        a + b
        
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
        let correctMapResult = Array.create size 5.0f
        let correctReduceResult = Array.reduce (fun a b -> a + b) a
                
        // Matrices
        let matSize = 32
        let matSizel = matSize |> int64
        let am = Array2D.create matSize matSize 2.0f
        let bm = Array2D.create matSize matSize 3.0f
        let dm = Array2D.zeroCreate<float32> matSize matSize
        let correctMatMul = Array2D.create matSize matSize (2.0f * 3.0f * (matSize |> float32))
        let correctMatMulAdd = Array2D.create matSize matSize (2.0f * 3.0f * (matSize |> float32) + 116.0f)
        
        // ***************************************************************************************************
        // Accelerated collection ****************************************************************************
        Console.WriteLine("")
        let test = "[ Accelerated Vector Sum ]"
        Console.WriteLine("# Testing " + test + "")
        timer.Start()
        let c = <@ Array.map2 (fun el1 el2 -> el1 + el2) a b @>.Run()
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
            let c = <@ Array.map2 (fun el1 el2 -> el1 + el2) a b @>.Run()
            timer.Stop()
            Console.WriteLine("  " + test + " execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")
        // ***************************************************************************************************
        
        // ***************************************************************************************************
        // Accelerated collection reduce ****************************************************************************
        Console.WriteLine("")
        let test = "[ Accelerated Vector Reduce With Utility Function ]"
        Console.WriteLine("# Testing " + test + "")
        timer.Start()
        let reduce_sum = <@ Array.reduce sumCurried a @>.Run()
        timer.Stop()
        // Check result
        let isResultCorrect = (reduce_sum = correctReduceResult)
        if not isResultCorrect then
            Console.WriteLine("  " + test + " returned a wrong result!")
        else
            Console.WriteLine("  " + test + " execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")
            timer.Restart()
            let reduce_sum = <@ Array.reduce sumCurried a @>.Run()
            timer.Stop()
            Console.WriteLine("  " + test + " execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")
        // ***************************************************************************************************
        
        // Accelerated collection reduce with lambda ****************************************************************************
        Console.WriteLine("")
        let test = "[ Accelerated Vector Reduce With Lambda ]"
        Console.WriteLine("# Testing " + test + "")
        timer.Start()
        let reduce_sum = <@ Array.reduce (fun e1 e2 -> e1 + e2) a @>.Run()
        timer.Stop()
        // Check result
        let isResultCorrect = (reduce_sum = correctReduceResult)
        if not isResultCorrect then
            Console.WriteLine("  " + test + " returned a wrong result!")
        else
            Console.WriteLine("  " + test + " execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")
            timer.Restart()
            let reduce_sum = <@ Array.reduce (fun e1 e2 -> e1 + e2) a @>.Run()
            timer.Stop()
            Console.WriteLine("  " + test + " execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")
        // ***************************************************************************************************
 