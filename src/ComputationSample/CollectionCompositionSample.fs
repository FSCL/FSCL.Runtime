module CollectionCompositionSample

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
        let count = 128
        let size = 16
        let lsize = size |> int64
        let arrays = Array.init count (
                        fun i -> Array.init size (
                                    fun j -> i + j))
                                    
        let revArrays = arrays |> Array.map(
                                    fun i -> i |> Array.rev)
        let correctMapResult = (arrays, revArrays) ||> Array.map2(fun i j -> (i,j) ||> Array.map2(fun a b -> a + b))
        let correctMultResult = (arrays, revArrays) ||> Array.map2(fun i j -> (i,j) ||> Array.map2(fun a b -> a * b))
                                        
        // ***************************************************************************************************
        // Map composition ****************************************************************************
        Console.WriteLine("")
        let test = "[ Map composition of Array.map ]"
        Console.WriteLine("# Testing " + test + "")
        timer.Start()
        let c = <@ (arrays, revArrays) ||> 
                   Array.map2(fun i j -> 
                                (i,j) ||> 
                                Array.map2(fun a b -> a + b)) 
                @>.Run()
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
            let c = <@ (arrays, revArrays) ||> 
                       Array.map2(fun i j -> 
                                    (i,j) ||> 
                                    Array.map2(fun a b -> a + b)) 
                    @>.Run()
            timer.Stop()
            Console.WriteLine("  " + test + " execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")
        // ***************************************************************************************************
        