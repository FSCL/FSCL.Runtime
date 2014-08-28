module ReturnTypeSample

    open FSCL
    open FSCL.Language
    open FSCL.Runtime
    open System
    open System.Diagnostics
    open OpenCL

    // Vector addition returning local allocated array
    [<Device(0,0)>][<ReflectedDefinition>]
    let VectorAddReturn(a: float32[], b: float32[], wi: WorkItemInfo) =
        let c = Array.zeroCreate<float32> (a.GetLength(0))
        let gid = wi.GlobalID(0)
        c.[gid] <- a.[gid] + b.[gid]
        c
        
    // Vector addition returning parameter
    [<Device(0,0)>][<ReflectedDefinition>]
    let VectorAddReturnPar(a: float32[], b: float32[], c: float32[], wi: WorkItemInfo) =
        let gid = wi.GlobalID(0)
        c.[gid] <- a.[gid] + b.[gid]
        c    

    let Run() =    
        let timer = new Stopwatch()

        // Vectors
        let size = 1 <<< 10
        let lsize = size |> int64
        let a = Array.create size 2.0f
        let b = Array.create size 3.0f
        let c = Array.zeroCreate<float32> (size)
        let correctMapResult = Array.create size 5.0f
        let correctReduceResult = Array.reduce (fun a b -> a + b) a
        
        // ***************************************************************************************************
        // Simple vector add return ****************************************************************************
        Console.WriteLine("")
        let test = "[ Vector Add With Return ]"
        Console.WriteLine("# Testing " + test + " with OpenCL")
        // Execute vector add in OpenCL mode
        let worksize = new WorkSize(lsize, 64L)
        timer.Start()        
        let d = <@ VectorAddReturn(a, b, worksize) @>.Run()
        timer.Stop()
        // Check result
        let mutable isResultCorrect = true
        for i = 0 to correctMapResult.Length - 1 do
            if correctMapResult.[i] <> d.[i] then
                isResultCorrect <- false
        if not isResultCorrect then
            Console.WriteLine("  " + test + " returned a wrong result!")
        else
            Console.WriteLine("  " + test + " execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")                       
            timer.Restart()        
            <@ VectorAddReturn(a, b, worksize) @>.Run() |> ignore
            timer.Stop()
            Console.WriteLine("  " + test + " execution time (kernel is not compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")                       
        // ***************************************************************************************************
        
        // ***************************************************************************************************
        // Simple vector add return par ****************************************************************************
        Console.WriteLine("")
        let test = "[ Vector Add With Return Par ]"
        Console.WriteLine("# Testing " + test + " with OpenCL")
        // Execute vector add in OpenCL mode
        let worksize = new WorkSize(lsize, 64L)
        timer.Start()        
        let d = <@ VectorAddReturnPar(a, b, c, worksize) @>.Run()
        timer.Stop()
        // Check result
        let mutable isResultCorrect = true
        for i = 0 to correctMapResult.Length - 1 do
            if correctMapResult.[i] <> d.[i] then
                isResultCorrect <- false
        if not isResultCorrect then
            Console.WriteLine("  " + test + " returned a wrong result!")
        else
            Console.WriteLine("  " + test + " execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")                       
            timer.Restart()        
            <@ VectorAddReturnPar(a, b, c, worksize) @>.Run() |> ignore
            timer.Stop()
            Console.WriteLine("  " + test + " execution time (kernel is not compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")                       
        // ***************************************************************************************************
