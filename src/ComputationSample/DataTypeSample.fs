module DataTypeSample

    open FSCL
    open FSCL.Language
    open FSCL.Runtime
    open FSCL.Compiler
    open System
    open System.Diagnostics
    open OpenCL        
    open System.Runtime
    open System.Runtime.InteropServices
        
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

    // Vector float4 addition
    [<ReflectedDefinition; Kernel>]
    let Vector4Add(a: float4[], b: float4[], c: float4[], wi: WorkItemInfo) =
        let gid = wi.GlobalID(0)
        c.[gid] <- a.[gid] + b.[gid]
        
    // Vector addition with struct
    [<ReflectedDefinition;Kernel>]
    let VectorAddStruct(a: MyStruct[], b: MyStruct[], c: MyStruct[], wi: WorkItemInfo) =
        let gid = wi.GlobalID(0)
        let mutable newStruct = new MyStruct()
        newStruct.x <- a.[gid].x + b.[gid].x
        newStruct.y <- a.[gid].y + b.[gid].y
        c.[gid] <- newStruct
    
    // Vector addition with struct with non-default constructor
    [<ReflectedDefinition;Kernel>]
    let VectorAddStructConstructor(a: MyStruct[], b: MyStruct[], c: MyStruct[], wi: WorkItemInfo) =
        let gid = wi.GlobalID(0)
        let mutable newStruct = new MyStruct(a.[gid].x + b.[gid].x, a.[gid].y + b.[gid].y)
        c.[gid] <- newStruct
    
    // Vector addition with record
    [<ReflectedDefinition;Kernel>]
    let VectorAddRecord(a: MyRecord[], b: MyRecord[], c: MyRecord[], wi: WorkItemInfo) =
        let gid = wi.GlobalID(0)
        let newRecord = { x = a.[gid].x + b.[gid].x; y = a.[gid].y + b.[gid].y }
        c.[gid] <- newRecord

    // Matrix multiplication
    [<ReflectedDefinition;Kernel>]
    let MatrixMult(a: float32[,], b: float32[,], c: float32[,], wi: WorkItemInfo) =
        let x = wi.GlobalID(0)
        let y = wi.GlobalID(1)

        let mutable accum = 0.0f
        for k = 0 to a.GetLength(1) - 1 do
            accum <- accum + (a.[x,k] * b.[k,y])
        c.[x,y] <- accum

    let Run() =    
        let FirstDeviceSupportMultiDimensionalWorkItems() =
            let device = OpenCLPlatform.Platforms.[0].Devices.[0]
            if device.MaxWorkItemDimensions > 1L then
                true
            else
                false

        let timer = new Stopwatch()

        // Struct vectors
        let size = 1 <<< 10
        let lsize = size |> int64
        let aStruct = Array.create size (new MyStruct(1.0f, 2.0f))
        let bStruct = Array.create size (new MyStruct(4.0f, 3.0f))
        let mutable cStruct = Array.zeroCreate<MyStruct> (size)
        let correctMapResultStruct = Array.create size (new MyStruct(5.0f, 5.0f))

        // Record vectors
        let aRecord = Array.create size ({ x = 1.0f; y = 2.0f })
        let bRecord = Array.create size ({ x = 4.0f; y = 3.0f })
        let mutable cRecord = Array.create size ({ x = 0.0f; y = 0.0f })
        let correctMapResultRecord = Array.create size ({ x = 5.0f; y = 5.0f })
        
        // Vectors4
        let a4 = Array.create size (float4(2.0f, 2.0f, 2.0f, 2.0f))
        let b4 = Array.create size (float4(3.0f, 3.0f, 3.0f, 3.0f))
        let mutable c4 = Array.zeroCreate<float4> (size)
        let correctMapResult4 = Array.create size (float4(5.0f, 5.0f, 5.0f, 5.0f))
        
        // ***************************************************************************************************
        // Float4 vector add ****************************************************************************
        Console.WriteLine("")
        let test = "[ Vector Add Float4 ]"
        Console.WriteLine("# Testing " + test + " with OpenCL")
        let worksize = new WorkSize(lsize, 64L)
        // Execute vector add in OpenCL mode
        timer.Start()
        <@ Vector4Add(a4, b4, c4, worksize) @>.Run() |> ignore
        timer.Stop()
        // Check result
        let mutable isResultCorrect = true
        for i = 0 to correctMapResult4.Length - 1 do
            if correctMapResult4.[i] <> c4.[i] then
                isResultCorrect <- false
        if not isResultCorrect then
            Console.WriteLine("  " + test + " returned a wrong result!")
        else
            Console.WriteLine("  " + test + " execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")
            timer.Restart()        
            <@ Vector4Add(a4, b4, c4, worksize) @>.Run() |> ignore
            timer.Stop()
            Console.WriteLine("  " + test + " execution time (kernel is not compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")                            
        // ***************************************************************************************************
           
        // ***************************************************************************************************
        // Vector add with struct ****************************************************************************
        Console.WriteLine("")
        let test = "[ Vector Add Struct ]"
        Console.WriteLine("# Testing " + test + " with OpenCL")
        // Execute vector add in OpenCL mode
        let worksize = new WorkSize(lsize, 64L)
        timer.Start()        
        <@ VectorAddStruct(aStruct, bStruct, cStruct, worksize) @>.Run() |> ignore
        timer.Stop()
        // Check result
        let mutable isResultCorrect = true
        for i = 0 to correctMapResultStruct.Length - 1 do
            if correctMapResultStruct.[i] <> cStruct.[i] then
                isResultCorrect <- false
        if not isResultCorrect then
            Console.WriteLine("  " + test + " returned a wrong result!")
        else
            Console.WriteLine("  " + test + " execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")
            timer.Restart()        
            <@ VectorAddStruct(aStruct, bStruct, cStruct, worksize) @>.Run() |> ignore
            timer.Stop()
            Console.WriteLine("  " + test + " execution time (kernel is not compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")                                
        // ***************************************************************************************************
        
        // ***************************************************************************************************
        // Vector add with struct with contructor ****************************************************************************
        Console.WriteLine("")
        let test = "[ Vector Add Struct Non-Default ]"
        Console.WriteLine("# Testing " + test + " with OpenCL")
        // Execute vector add in OpenCL mode
        cStruct <-  Array.zeroCreate<MyStruct> (size)
        let worksize = new WorkSize(lsize, 64L)
        timer.Start()        
        <@ VectorAddStructConstructor(aStruct, bStruct, cStruct, worksize) @>.Run() |> ignore
        timer.Stop()
        // Check result
        let mutable isResultCorrect = true
        for i = 0 to correctMapResultStruct.Length - 1 do
            if correctMapResultStruct.[i] <> cStruct.[i] then
                isResultCorrect <- false
        if not isResultCorrect then
            Console.WriteLine("  " + test + " returned a wrong result!")
        else
            Console.WriteLine("  " + test + " execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")
            timer.Restart()        
            <@ VectorAddStructConstructor(aStruct, bStruct, cStruct, worksize) @>.Run() |> ignore
            timer.Stop()
            Console.WriteLine("  " + test + " execution time (kernel is not compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")                                
        // ***************************************************************************************************
                          
        // ***************************************************************************************************
        // Vector add with record ****************************************************************************
        Console.WriteLine("")
        let test = "[ Vector Add Record ]"
        Console.WriteLine("# Testing " + test + " with OpenCL")
        // Execute vector add in OpenCL mode
        let worksize = new WorkSize(lsize, 64L)
        timer.Start()        
        <@ VectorAddRecord(aRecord, bRecord, cRecord, worksize) @>.Run() |> ignore
        timer.Stop()
        // Check result
        let mutable isResultCorrect = true
        for i = 0 to correctMapResultRecord.Length - 1 do
            if correctMapResultRecord.[i] <> cRecord.[i] then
                isResultCorrect <- false
        if not isResultCorrect then
            Console.WriteLine("  " + test + " returned a wrong result!")
        else
            Console.WriteLine("  " + test + " execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")
            timer.Restart()        
            <@ VectorAddRecord(aRecord, bRecord, cRecord, worksize) @>.Run() |> ignore
            timer.Stop()
            Console.WriteLine("  " + test + " execution time (kernel is not compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")                                
        // ***************************************************************************************************
        