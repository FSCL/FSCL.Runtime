module SequentialAndMultithreadSample

    open FSCL
    open FSCL.Language
    open FSCL.Runtime
    open System
    open System.Diagnostics
    open OpenCL
    
    // Sequential computation
    let SimpleSequentialComp(a:float32[], b:float32[]) =
        let c = Array.zeroCreate<float32> (a.Length)
        for i = 0 to a.Length - 1 do
            c.[i] <- a.[i] + b.[i]
        c
        
    // Vector addition 
    [<Device(0,0)>][<ReflectedDefinition>]
    let VectorAdd(a: float32[], b: float32[], c: float32[], wi: WorkItemInfo) =
        let gid = wi.GlobalID(0)
        c.[gid] <- a.[gid] + b.[gid]
        
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
        let correctMapResult = Array.create size 5.0f
        let correctReduceResult = Array.reduce (fun a b -> a + b) a
                
        // ***************************************************************************************************
        // Simple sequential computation ****************************************************************************
        Console.WriteLine("")
        let test = "[ Sequential Computation ]"
        Console.WriteLine("# Testing " + test + " on CPU")
        // Execute vector add in OpenCL mode
        let worksize = new WorkSize(lsize, 64L)
        timer.Start()        
        let c = <@ SimpleSequentialComp(a, b) @>.Run()
        timer.Stop()
        // Check result
        let mutable isResultCorrect = true
        for i = 0 to correctMapResult.Length - 1 do
            if correctMapResult.[i] <> c.[i] then
                isResultCorrect <- false
        if not isResultCorrect then
            Console.WriteLine("  " + test + " returned a wrong result!")
        else
            Console.WriteLine("  " + test + " execution time (kernel is never compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")                       
            timer.Restart()        
            <@ SimpleSequentialComp(a, b) @>.Run() |> ignore
            timer.Stop()
            Console.WriteLine("  " + test + " execution time (kernel is never compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")                       
        // ***************************************************************************************************
          
        // ***************************************************************************************************
        // Simple vector add mutithread ****************************************************************************
        Console.WriteLine("")
        let test = "[ Multithread Vector Add ]"
        Console.WriteLine("# Testing " + test + " on CPU")
        // Execute vector add in OpenCL mode
        let worksize = new WorkSize(lsize, 64L)
        let d = Array.zeroCreate<float32> size
        timer.Start()        
        <@ VectorAdd(a, b, d, worksize) @>.RunMultithread()
        timer.Stop()
        // Check result
        let mutable isResultCorrect = true
        for i = 0 to correctMapResult.Length - 1 do
            if correctMapResult.[i] <> d.[i] then
                isResultCorrect <- false
        if not isResultCorrect then
            Console.WriteLine("  " + test + " returned a wrong result!")
        else
            Console.WriteLine("  " + test + " execution time (kernel is never compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")                       
            timer.Restart()        
            <@ VectorAdd(a, b, d, worksize) @>.RunMultithread()
            timer.Stop()
            Console.WriteLine("  " + test + " execution time (kernel is never compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")                       
        // ***************************************************************************************************
        
        // ***************************************************************************************************
        // Simple vector add sequential ****************************************************************************
        Console.WriteLine("")
        let test = "[ Sequential Vector Add ]"
        Console.WriteLine("# Testing " + test + " on CPU")
        // Execute vector add in OpenCL mode
        let worksize = new WorkSize(lsize, 64L)
        let d = Array.zeroCreate<float32> size
        timer.Start()        
        <@ VectorAdd(a, b, d, worksize) @>.RunSequential()
        timer.Stop()
        // Check result
        let mutable isResultCorrect = true
        for i = 0 to correctMapResult.Length - 1 do
            if correctMapResult.[i] <> d.[i] then
                isResultCorrect <- false
        if not isResultCorrect then
            Console.WriteLine("  " + test + " returned a wrong result!")
        else
            Console.WriteLine("  " + test + " execution time (kernel is never compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")                       
            timer.Restart()        
            <@ VectorAdd(a, b, d, worksize) @>.RunSequential()
            timer.Stop()
            Console.WriteLine("  " + test + " execution time (kernel is never compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")                       
        // ***************************************************************************************************
         
        // ***************************************************************************************************
        // Simple vector add with return par multithread ****************************************************************************
        Console.WriteLine("")
        let test = "[ Multithread Vector Add With Returned Par ]"
        Console.WriteLine("# Testing " + test + " on CPU")
        // Execute vector add in OpenCL mode
        let worksize = new WorkSize(lsize, 64L)
        timer.Start()        
        let d = <@ VectorAddReturnPar(a, b, Array.zeroCreate<float32> size, worksize) @>.RunMultithread()
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
            let d = <@ VectorAddReturnPar(a, b, Array.zeroCreate<float32> size, worksize) @>.RunMultithread()
            timer.Stop()
            Console.WriteLine("  " + test + " execution time (kernel is not compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")                       
        // ***************************************************************************************************
                
        // ***************************************************************************************************
        // Simple vector add with return par sequential ****************************************************************************
        Console.WriteLine("")
        let test = "[ Sequential Vector Add With Returned Par ]"
        Console.WriteLine("# Testing " + test + " on CPU")
        // Execute vector add in OpenCL mode
        let worksize = new WorkSize(lsize, 64L)
        timer.Start()        
        let d = <@ VectorAddReturnPar(a, b, Array.zeroCreate<float32> size, worksize) @>.RunSequential()
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
            let d = <@ VectorAddReturnPar(a, b, Array.zeroCreate<float32> size, worksize) @>.RunSequential()
            timer.Stop()
            Console.WriteLine("  " + test + " execution time (kernel is not compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")                       
        // ***************************************************************************************************
        