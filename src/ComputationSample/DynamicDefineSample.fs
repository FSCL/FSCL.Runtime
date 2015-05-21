module DynamicDefineSample

    open FSCL
    open FSCL.Language
    open FSCL.Runtime
    open FSCL.Compiler
    open System
    open System.Diagnostics
    open OpenCL

    [<ReflectedDefinition>]
    let staticMultiplier = 2.0f
    
    [<ReflectedDefinition>]
    let mutable dynamicMultiplier = 2.0f

    // Vector addition referring to an immutable global variable
    [<ReflectedDefinition;Kernel>]
    let VectorAddWithStaticDef(a: float32[], b: float32[], c: float32[], wi: WorkItemInfo) =
        let gid = wi.GlobalID(0)
        // staticMultiplier is replaced with its value (2.0f) at FSCL-Compiler time
        c.[gid] <- (a.[gid] + b.[gid]) * staticMultiplier
        
    // Vector addition referring to a mutable global variable
    [<ReflectedDefinition;Kernel>]
    let VectorAddWithDynamicDef(a: float32[], b: float32[], c: float32[], wi: WorkItemInfo) =
        let gid = wi.GlobalID(0)
        // dynamicMultiplier is replaced with its value (2.0f) at OpenCL-to-executable compile time
        c.[gid] <- (a.[gid] + b.[gid]) * dynamicMultiplier

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
        // Vector add with static define ****************************************************************************
        Console.WriteLine("")
        let test = "[ Vector Add With Static Define ]"
        Console.WriteLine("# Testing " + test + " with OpenCL")
        // Execute vector add in OpenCL mode
        let worksize = new WorkSize(lsize, 64L)
        timer.Start()        
        <@ VectorAddWithStaticDef(a, b, c, worksize) @>.Run() |> ignore
        timer.Stop()
        // Check result
        let mutable isResultCorrect = true
        for i = 0 to correctMapResult.Length - 1 do
            if correctMapResult.[i] * staticMultiplier <> c.[i] then
                isResultCorrect <- false
        if not isResultCorrect then
            Console.WriteLine("  " + test + " returned a wrong result!")
        else
            Console.WriteLine("  " + test + " execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")                       
            timer.Restart()        
            <@ VectorAddWithStaticDef(a, b, c, worksize) @>.Run() |> ignore
            timer.Stop()
            Console.WriteLine("  " + test + " execution time (kernel is not compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")                       
        // ***************************************************************************************************
         
        // ***************************************************************************************************
        // Vector add with dynamic define ****************************************************************************
        Console.WriteLine("")
        let test = "[ Vector Add With Dynamic Define ]"
        Console.WriteLine("# Testing " + test + " with OpenCL")
        // Execute vector add in OpenCL mode
        let worksize = new WorkSize(lsize, 64L)
        timer.Start()        
        <@ VectorAddWithDynamicDef(a, b, c, worksize) @>.Run() |> ignore
        timer.Stop()
        // Check result
        let mutable isResultCorrect = true
        for i = 0 to correctMapResult.Length - 1 do
            if correctMapResult.[i] * dynamicMultiplier <> c.[i]  then
                isResultCorrect <- false
        if not isResultCorrect then
            Console.WriteLine("  " + test + " returned a wrong result!")
        else
            Console.WriteLine("  " + test + " execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")                       
            // Now I change the mutable variable value
            dynamicMultiplier <- 5.0f
            timer.Restart()        
            <@ VectorAddWithDynamicDef(a, b, c, worksize) @>.Run() |> ignore
            timer.Stop()
            isResultCorrect <- true
            for i = 0 to correctMapResult.Length - 1 do
                if correctMapResult.[i] * dynamicMultiplier <> c.[i] then
                    isResultCorrect <- false
            if not isResultCorrect then
                Console.WriteLine("  " + test + " returned a wrong result (after changing the dynamic define value)!")
            else
                Console.WriteLine("  " + test + " execution time (kernel is opencl-to-executable compiled cause the dynamic define has changed value): " + timer.ElapsedMilliseconds.ToString() + "ms")                       
        // ***************************************************************************************************
       