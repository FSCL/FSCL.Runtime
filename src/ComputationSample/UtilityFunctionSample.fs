module UtilityFunctionSample

    open FSCL
    open FSCL.Language
    open FSCL.Runtime
    open System
    open System.Diagnostics
    open OpenCL

    // Vector addition with utility taking values
    [<ReflectedDefinition>]
    let sumValues(a, b) =
        a + b

    [<Device(0,0)>][<ReflectedDefinition>]
    let VectorAddWithUtilitySummingValues(a: float32[], b: float32[], c: float32[], wi:WorkItemInfo) =
        let gid = wi.GlobalID(0)
        c.[gid] <- sumValues(a.[gid], b.[gid])
                
    // Vector addition with utility taking arrays
    [<ReflectedDefinition>]
    let sumArrays(a:float32[], b:float32[], i:int) =
        a.[i] + b.[i]

    [<Device(0,0)>][<ReflectedDefinition>]
    let VectorAddWithUtilitySummingArrays(a: float32[], b: float32[], c: float32[], wi:WorkItemInfo) =
        let gid = wi.GlobalID(0)
        c.[gid] <- sumArrays(a, b, gid)
        
    // Vector addition with nested utility
    [<ReflectedDefinition>]
    let sumArraysNested(a:float32[], b:float32[], i:int) =
        sumValues(a.[i], b.[i])

    [<Device(0,0)>][<ReflectedDefinition>]
    let VectorAddWithNestedUtility(a: float32[], b: float32[], c: float32[], wi:WorkItemInfo) =
        let gid = wi.GlobalID(0)
        c.[gid] <- sumArraysNested(a, b, gid)

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
        // Simple vector add with utility ****************************************************************************
        Console.WriteLine("")
        let test = "[ Vector Add With Utility Taking Values ]"
        Console.WriteLine("# Testing " + test + " with OpenCL")
        // Execute vector add in OpenCL mode
        let worksize = new WorkSize(lsize, 64L)
        timer.Start()        
        <@ VectorAddWithUtilitySummingValues(a, b, c, worksize) @>.Run()
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
            <@ VectorAddWithUtilitySummingValues(a, b, c, worksize) @>.Run()
            timer.Stop()
            Console.WriteLine("  " + test + " execution time (kernel is not compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")                       
        // ***************************************************************************************************
        
        // MUST FIX
        // ***************************************************************************************************
        // Simple vector add with utility taking vectors ****************************************************************************
        (*
        Console.WriteLine("")
        let test = "[ Vector Add With Utility Taking Arrays ]"
        Console.WriteLine("# Testing " + test + " with OpenCL")
        // Execute vector add in OpenCL mode
        let worksize = new WorkSize(lsize, 64L)
        timer.Start()        
        <@ VectorAddWithUtilitySummingArrays(a, b, c, worksize) @>.Run()
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
            <@ VectorAddWithUtilitySummingArrays(a, b, c, worksize) @>.Run()
            timer.Stop()
            Console.WriteLine("  " + test + " execution time (kernel is not compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")                       
        // ***************************************************************************************************
               
        // ***************************************************************************************************
        // Simple vector add with utility nested ****************************************************************************
        Console.WriteLine("")
        let test = "[ Vector Add With Nested Utility ]"
        Console.WriteLine("# Testing " + test + " with OpenCL")
        // Execute vector add in OpenCL mode
        let worksize = new WorkSize(lsize, 64L)
        timer.Start()        
        <@ VectorAddWithNestedUtility(a, b, c, worksize) @>.Run()
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
            <@ VectorAddWithNestedUtility(a, b, c, worksize) @>.Run()
            timer.Stop()
            Console.WriteLine("  " + test + " execution time (kernel is not compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")                       
        // ***************************************************************************************************
        *)