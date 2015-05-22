module CompositionSample

    open FSCL
    open FSCL.Language
    open FSCL.Runtime
    open FSCL.Compiler
    open System
    open System.Diagnostics
    open OpenCL
    
    let MatMulCPU (a: float32[,]) (b: float32[,]) =
        let res = Array2D.zeroCreate<float32> (a.GetLength(0)) (b.GetLength(1))
        for row = 0 to a.GetLength(0) - 1  do
            for col = 0 to b.GetLength(1) - 1 do
                let mutable v = 0.0f
                for k = 0 to a.GetLength(1) - 1 do
                    v <- v + (a.[row, k] * b.[k, col])
                res.[row, col] <- v
        res

    // Matrix multiplication
    [<ReflectedDefinition;Kernel>]
    let MatMul (wi: WorkItemInfo) (matA: float32[,]) (matB: float32[,]) =
        let matC = Array2D.zeroCreate<float32> (matA.GetLength(0)) (matA.GetLength(1))
        let r = wi.GlobalID(1)
        let c = wi.GlobalID(0)
    
        // Unroll 8
        let mutable accum = 0.0f
        if r < matA.GetLength(0) && c < matB.GetLength(1) then
            for i = 0 to matA.GetLength(1) - 1 do
                accum <- accum + matA.[r, i] * matB.[i, c]
            matC.[r, c] <- accum
        matC
        
    // Matrix multiplication
    [<ReflectedDefinition;Kernel>]
    let Map2 (wi: WorkItemInfo) (matA: float32[,]) (matB: float32[,]) =
        let matC = Array2D.zeroCreate<float32> (matA.GetLength(0)) (matA.GetLength(1))
        let r = wi.GlobalID(1)
        let c = wi.GlobalID(0)
    
        // Unroll 8
        let mutable accum = 0.0f
        matC.[r, c] <- matA.[r,c] - matB.[r,c]
        matC
        
    [<ReflectedDefinition;Kernel>]
    let MatTransp (wi: WorkItemInfo) (matA: float32[,]) =
        let matC = Array2D.zeroCreate<float32> (matA.GetLength(1)) (matA.GetLength(0))

        let x = wi.GlobalID(0)
        let y = wi.GlobalID(1)
        matC.[x, y] <- matA.[y, x]
        matC

    let Run() =    
        let FirstDeviceSupportMultiDimensionalWorkItems() =
            let device = OpenCLPlatform.Platforms.[0].Devices.[0]
            if device.MaxWorkItemDimensions > 1L then
                true
            else
                false

        let timer = new Stopwatch()
        
        let rnd = System.Random()
        // Matrices    

        let GetData(inputSize:int) =
            let matA = Array2D.zeroCreate inputSize inputSize 
            let matB = Array2D.zeroCreate inputSize inputSize 
            // Triangular matrix
            for r = 0 to matA.GetLength(0) - 1 do
                for c = r to matA.GetLength(1) - 1 do
                    matA.[r, c] <- 1.0f
                    matB.[r, c] <- 1.0f

            let mutable norm1 = 0.0f
            for r = 0 to matA.GetLength(0) - 1 do
                let mutable s = 0.0f
                for c = 0 to matA.GetLength(1) - 1 do
                    s <- s + matA.[r,c]
                if s > norm1 then
                    norm1 <- s
            let mutable normInf = 0.0f
            for c = 0 to matA.GetLength(1) - 1 do
                let mutable s = 0.0f
                for r = 0 to matA.GetLength(0) - 1 do
                    s <- s + matA.[r,c]
                if s > normInf then
                    normInf <- s
            let normalization = 1.0f / (norm1 * normInf) 
            matA, matB, 1.0f, new WorkSize([| inputSize |> int64; inputSize |> int64 |], [| 16L; 16L |]) 
            
        // Create input
        //let input = Array.create count (
//                        Array2D.init<uchar4> inputSize inputSize (fun r c -> 
//                                                                    uchar4(rnd.Next() % 5 |> byte, rnd.Next() % 5 |> byte, rnd.Next() % 5 |> byte, rnd.Next() % 5 |> byte)))
        //let ws = WorkSize([| (((outputSize - 1) / 16) + 1) * 16 |> int64; (((outputSize - 1) / 16) + 1) * 16 |> int64 |], [| 16L; 16L |])

                        
                             
        // Dump completion times on the various devices
        let minSize, maxSize = 64, 1024
        let iters = 30
        let mutable data = [ [ "Matmul"; "" ] ]
//        let platforms = FSCL.Runtime.GetOpenCLPlatforms()
//        for pidx, pname, ds in platforms do
//            for didx, dname, _ in ds do
//                Console.WriteLine("Running matmul on " + pname)
//                data <- data @ [[ pname; "" ]]
//                for s in minSize .. minSize .. maxSize do
//                    Console.WriteLine(s)
//                    let matA, matB, normalization, ws = GetData(s)
//                    let res = <@ DEVICE(pidx, didx, MatMul ws matA matB) @>.Run()
//                    let timer = new System.Diagnostics.Stopwatch()
//                    let mutable vol = null
//                    timer.Start()
//                    for i = 0 to iters - 1 do
//                        vol <- <@ DEVICE(pidx, didx, MatMul ws matA matB) @>.Run()
//                    timer.Stop()
//                    Console.WriteLine("Result correct? " + (vol <> null).ToString())
//                    data <- data @ [[ s.ToString(); (((double)timer.ElapsedMilliseconds)/((double)iters)).ToString() ]]
//        let text = data |> 
//                    List.map(String.concat ",") |> 
//                    String.concat System.Environment.NewLine
//        System.IO.File.WriteAllText(data.[0].[0] + ".csv", text)
//                    
//        let mutable data = [ [ "Transpose"; "" ] ]
//        for pidx, pname, ds in FSCL.Runtime.GetOpenCLPlatforms() do
//            for didx, name, _ in ds do
//                Console.WriteLine("Running transpose on " + pname)
//                data <- data @ [[ pname; "" ]]
//                for s in minSize .. minSize .. maxSize do
//                    Console.WriteLine(s)
//                    let matA, matB, normalization, ws = GetData(s) 
//                    let res = <@ DEVICE(pidx, didx, MatTransp ws matA) @>.Run()
//                    let timer = new System.Diagnostics.Stopwatch()
//                    let mutable vol = null
//                    timer.Start()
//                    for i = 0 to iters - 1 do
//                        vol <- <@ DEVICE(pidx, didx, MatTransp ws matA) @>.Run() 
//                    timer.Stop()
//                    Console.WriteLine("Result correct? " + (vol <> null).ToString())
//                    data <- data @ [[ s.ToString(); (((double)timer.ElapsedMilliseconds)/((double)iters)).ToString() ]]
//        let text = data |> 
//                    List.map(String.concat ",") |> 
//                    String.concat System.Environment.NewLine
//        System.IO.File.WriteAllText(data.[0].[0] + ".csv", text)
//
//        let mutable data = [ [ "Map2"; "" ] ]
//        for pidx, pname, ds in FSCL.Runtime.GetOpenCLPlatforms() do
//            for didx, name, _ in ds do
//                Console.WriteLine("Running map2 on " + pname)
//                data <- data @ [[ pname; "" ]]
//                for s in minSize .. minSize .. maxSize do
//                    Console.WriteLine(s)
//                    let matA, matB, normalization, ws = GetData(s) 
//                    let res = <@ DEVICE(pidx, didx, Map2 ws matA matA) @>.Run()
//                    let timer = new System.Diagnostics.Stopwatch()
//                    let mutable vol = null
//                    timer.Start()
//                    for i = 0 to iters - 1 do
//                        vol <- <@ DEVICE(pidx, didx, Map2 ws matA matA) @>.Run() 
//                    timer.Stop()
//                    Console.WriteLine("Result correct? " + (vol <> null).ToString())
//                    data <- data @ [[ s.ToString(); (((double)timer.ElapsedMilliseconds)/((double)iters)).ToString() ]]
//        let text = data |> 
//                    List.map(String.concat ",") |> 
//                    String.concat System.Environment.NewLine
//        System.IO.File.WriteAllText(data.[0].[0] + ".csv", text)
//                    
//        let mutable data = [ [ "Map"; "" ] ]
//        for pidx, pname, ds in FSCL.Runtime.GetOpenCLPlatforms() do
//            for didx, name, _ in ds do
//                Console.WriteLine("Running map2 on " + pname)
//                data <- data @ [[ pname; "" ]]
//                for s in minSize .. minSize .. maxSize do
//                    Console.WriteLine(s)
//                    let matA, matB, normalization, ws = GetData(s)
//                    let res = <@ DEVICE(pidx, didx, Array2D.map(fun el -> el * 2.0f) matA) @>.Run()
//                    let timer = new System.Diagnostics.Stopwatch()
//                    let mutable vol = null
//                    timer.Start()
//                    for i = 0 to iters - 1 do
//                        vol <- <@ DEVICE(pidx, didx, Array2D.map(fun el -> el * 2.0f) matA) @>.Run() 
//                    timer.Stop()
//                    Console.WriteLine("Result correct? " + (vol <> null).ToString())
//                    data <- data @ [[ s.ToString(); (((double)timer.ElapsedMilliseconds)/((double)iters)).ToString() ]]
//        let text = data |> 
//                    List.map(String.concat ",") |> 
//                    String.concat System.Environment.NewLine
//        System.IO.File.WriteAllText(data.[0].[0] + ".csv", text)
//                
        let mutable data = [ [ "CompositionBest"; "" ] ]
        for pidx, pname, ds in FSCL.Runtime.GetOpenCLPlatforms() do
            for didx, name, _ in ds do
                if pidx > -1 then
                    Console.WriteLine("Running composition on " + pname)
                    data <- data @ [[ pname; "" ]]
                    for s in minSize .. minSize .. maxSize do
                        let matA, matB, normalization, ws = GetData(s)
                        Console.WriteLine(s)
                        let res = <@    Map2 ws 
                                                                          (DEVICE(1, 0, Array2D.map(fun el -> el * 2.0f) matB)) 
                                                                           (DEVICE(0, 0,  MatMul ws matB matA))
                                   @>.Run()
                        let timer = new System.Diagnostics.Stopwatch()
                        let mutable vol = null
                        timer.Start()
                        for i = 0 to iters - 1 do
                            vol <- <@  [| 0 |] |>
                                       Array.fold(fun X it ->
                                                     DEVICE(1, 0, Map2 ws 
                                                                          (DEVICE(1, 0, Array2D.map(fun el -> el * 2.0f) X)) 
                                                                           (DEVICE(0, 0, MatMul ws (DEVICE(0, 0, MatMul ws X matA)) X))))
                                                  (DEVICE(1, 0, Array2D.map(fun it -> it * normalization) (DEVICE(1, 0, MatTransp ws matA))))
                                   @>.Run()
                        timer.Stop()
                        Console.WriteLine("Result correct? " + (vol <> null).ToString())
                        data <- data @ [[ s.ToString(); (((double)timer.ElapsedMilliseconds)/((double)iters)).ToString() ]]
        let text = data |> 
                    List.map(String.concat ",") |> 
                    String.concat System.Environment.NewLine
        System.IO.File.WriteAllText(data.[0].[0] + ".csv", text)

                       
            
        // ***************************************************************************************************
        // NHS approximation of inverse ****************************************************************************
        Console.WriteLine("")
        let test = "[ NHS approximation of matrix inverse ]"
        Console.WriteLine("# Testing " + test + "")
        timer.Start()
        let threshold = 0.8f
        ()
//
//        // ***************************************************************************************************
//        // Map composition ****************************************************************************
//        Console.WriteLine("")
//        let test = "[ Composition of kernels ]"
//        Console.WriteLine("# Testing " + test + "")
//        timer.Start()
//        let threshold = 0.8f
//        let c = <@ input |>
//                   Array.maxBy(fun image ->
//                                    image |>                                     
//                                    // To black-and-white
//                                    Array2D.map(fun p -> (0.2126f * (float32)p.x + 0.7152f * (float32)p.y + 0.0722f * (float32)p.z)) |> 
//                                    // Sobel
//                                    SobelFilter2D ws |> 
//                                    // Count pixels over the white threshold 
//                                    averageBy (fun it -> 
//                                                    if it > threshold then
//                                                        1.0f
//                                                    else 
//                                                        0.0f)) 
//               @>.Run()
////        timer.Stop()
////        // Check result
////        let mutable isResultCorrect = true
////        for i = 0 to correctMapResult.Length - 1 do
////            if correctMapResult.[i] <> c.[i] then
////                isResultCorrect <- false
////        if not isResultCorrect then
////            Console.WriteLine("  " + test + " returned a wrong result!")
////        else
////            Console.WriteLine("  " + test + " execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")
////            timer.Restart()
////            let c = <@ (arrays, revArrays) ||> 
////                       Array.map2(fun i j -> 
////                                    (i,j) ||> 
////                                    Array.map2(fun a b -> a + b)) 
////                    @>.Run()
////            timer.Stop()
////            Console.WriteLine("  " + test + " execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")
//        // ***************************************************************************************************
//        