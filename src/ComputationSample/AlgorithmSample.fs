module AlgorithmSample

    open FSCL
    open FSCL.Language
    open FSCL.Runtime
    open FSCL.Compiler
    open System
    open System.Diagnostics
    open OpenCL
    
    // Vector addition
    [<ReflectedDefinition; Kernel>]
    let VectorAddCurried (wi:WorkItemInfo) (a: float32[]) (b: float32[]) =
        let c = Array.zeroCreate<float32> a.Length
        let gid = wi.GlobalID(0)
        c.[gid] <- a.[gid] + b.[gid]
        c

    // Vector addition
    [<ReflectedDefinition; Kernel>]
    let VectorAdd(a: float32[], b: float32[], wi: WorkItemInfo) =
        let c = Array.zeroCreate<float32> a.Length
        let gid = wi.GlobalID(0)
        c.[gid] <- a.[gid] + b.[gid]
        c
        
    // Vector addition
    [<ReflectedDefinition; Kernel>]
    let VectorAddWrite(a: float32[], b: float32[], c:float32[], wi: WorkItemInfo) =
        let gid = wi.GlobalID(0)
        c.[gid] <- a.[gid] + b.[gid]
        
    // Matrix multiplication
    [<ReflectedDefinition; Kernel>]
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
        // Simple vector add *********************************************************************************
        Console.WriteLine("")
        let test = "[ Vector Add ]"
        Console.WriteLine("# Testing " + test + " with OpenCL")
        // Execute vector add in OpenCL mode
        let worksize = new WorkSize(lsize, 64L)
        let n = <@ VectorAddWrite(a, b, c, worksize) @>.Run()
        
         // ***************************************************************************************************
        // Simple vector add **********************************************************************************
        Console.WriteLine("")
        let test = "[ Array.map2 ]"
        Console.WriteLine("# Testing " + test + " with OpenCL")
        // Execute vector add in OpenCL mode
        let worksize = new WorkSize(lsize, 64L)
        let c = <@ (a,b) ||> 
                   Array.map2 (fun a b -> a + b)
                @>.Run()
        ()
