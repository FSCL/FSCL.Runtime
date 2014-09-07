module AcceleratedCollectionDataTypeSample

    open FSCL
    open FSCL.Language
    open FSCL.Runtime
    open System
    open System.Diagnostics
    open OpenCL
    open System.Runtime.InteropServices
    
    [<StructLayout(LayoutKind.Sequential)>]
    type MyRecord = {
        mutable x: float32;
        mutable y: float32    
    }

    [<ReflectedDefinition>]
    let sumCurried (a: MyRecord) (b: MyRecord)  =
        let v = { x = a.x + b.x; y = a.y + b.y }
        v

    let Run() =    
        let timer = new Stopwatch()

        // Record vectors
        let size = 1 <<< 10
        let a = Array.create size ({ x = 1.0f; y = 2.0f })
        let b = Array.create size ({ x = 4.0f; y = 3.0f })
        let mutable c = Array.zeroCreate<MyRecord> size
        let correctMapResult = Array.create size ({ x = 5.0f; y = 5.0f })
        let correctReduceResult = a |> Array.reduce (fun a b -> { x = a.x + b.x; y = a.y + b.y }) 
                
        // ***************************************************************************************************
        // Accelerated collection ****************************************************************************
        Console.WriteLine("")
        let test = "[ Accelerated Records Vector Sum ]"
        Console.WriteLine("# Testing " + test + "")
        timer.Start()
        let c = <@ Array.map2 (fun el1 el2 -> 
                                let v = { x = el1.x + el2.x; y = el1.y + el2.y }
                                v) a b @>.Run()
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
            let c = <@ Array.map2 (fun el1 el2 -> 
                                    let v = { x = el1.x + el2.x; y = el1.y + el2.y }
                                    v) a b @>.Run()
            timer.Stop()
            Console.WriteLine("  " + test + " execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")
        // ***************************************************************************************************
        
        // ***************************************************************************************************
        // Accelerated collection reduce ****************************************************************************
        Console.WriteLine("")
        let test = "[ Accelerated Record Vector Reduce With Utility Function ]"
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
        let test = "[ Accelerated Record Vector Reduce With Lambda ]"
        Console.WriteLine("# Testing " + test + "")
        timer.Start()
        let reduce_sum = <@ Array.reduce (fun el1 el2 -> 
                                            let ret = { x = el1.x + el2.x; y = el1.y + el2.y }
                                            ret) a @>.Run()
        timer.Stop()
        // Check result
        let isResultCorrect = (reduce_sum = correctReduceResult)
        if not isResultCorrect then
            Console.WriteLine("  " + test + " returned a wrong result!")
        else
            Console.WriteLine("  " + test + " execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")
            timer.Restart()
            let reduce_sum = <@ Array.reduce (fun el1 el2 -> 
                                                    let ret = { x = el1.x + el2.x; y = el1.y + el2.y }
                                                    ret) a @>.Run()
            timer.Stop()
            Console.WriteLine("  " + test + " execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")
        // ***************************************************************************************************
 