module FSCL.Runtime.Tests.KernelExecution

open FSCL
open FSCL.Runtime
open FSCL.Compiler
open FSCL.Language

open OpenCL
open NUnit.Framework

// Vector addition
[<ReflectedDefinition>]
let VectorAdd(a: float32[], b: float32[], c: float32[], wi: WorkItemInfo) =
    let gid = wi.GlobalID(0)
    c.[gid] <- a.[gid] + b.[gid]
    
// Vector addition with return type
[<ReflectedDefinition>]
let VectorAddReturn(a: float32[], b: float32[], wi: WorkItemInfo) =
    let c = Array.zeroCreate<float32> (a.GetLength(0))
    let gid = wi.GlobalID(0)
    c.[gid] <- a.[gid] + b.[gid]
    c
 
// Vector addition with utility function   
[<ReflectedDefinition>]
let sum(a, b) =
    a + b
[<ReflectedDefinition>]
let VectorAddWithUtilityFunction(a: float32[], b: float32[], c: float32[], wi:WorkItemInfo) =
    let gid = wi.GlobalID(0)
    c.[gid] <- sum(a.[gid], b.[gid])
    
// Vector addition with utility function inline  
[<ReflectedDefinition>][<Inline>]
let inline sumInline(a, b) =
    a + b
[<ReflectedDefinition>]
let VectorAddWithUtilityFunctionInline(a: float32[], b: float32[], c: float32[], wi:WorkItemInfo) =
    let gid = wi.GlobalID(0)
    c.[gid] <- sum(a.[gid], b.[gid])

// Vector4 addition
[<ReflectedDefinition>]
let Vector4Add(a: float4[], b: float4[], c: float4[], wi: WorkItemInfo) =
    let gid = wi.GlobalID(0)
    c.[gid] <- a.[gid] + b.[gid]

// Matrix addition
[<ReflectedDefinition>]
let MatrixAdd(a: float32[,], b: float32[,], c: float32[,], wi: WorkItemInfo) =
    let x = wi.GlobalID(0)

    for k = 0 to a.GetLength(0) - 1 do
        c.[x,k] <- a.[x,k] + b.[x,k]

// Matrix multiplication
[<ReflectedDefinition>]
let MatrixMult(a: float32[,], b: float32[,], result: float32[,], wi: WorkItemInfo) =
    let col = wi.GlobalID(0)
    let row = wi.GlobalID(1)

    let mutable accum = 0.0f
    for k = 0 to a.GetLength(1) - 1 do
        accum <- accum + (a.[row, k] * b.[k, col])
    result.[row, col] <- accum
    
// Matrix multiplication with local and reference to global var (BLOCK_SIZE)
[<ReflectedDefinition>]
let BLOCK_SIZE = 16
[<ReflectedDefinition>]
let MatrixMultAdvanced(matA: float32[,], matB: float32[,], matC: float32[,], wi: WorkItemInfo) =
    let bx = wi.GroupID(0)
    let by = wi.GroupID(1) 
    let tx = wi.LocalID(0)
    let ty = wi.LocalID(1)
    let wa = matA.GetLength(0)
    let wb = matB.GetLength(0)

    let bCol = bx * BLOCK_SIZE
    let bBeginRow = 0
    let bStep  = BLOCK_SIZE
    let mutable bRow = bBeginRow
    let mutable Csub = 0.0f
 
    let As = local(Array2D.zeroCreate<float32> BLOCK_SIZE BLOCK_SIZE)
    let Bs = local(Array2D.zeroCreate<float32> BLOCK_SIZE BLOCK_SIZE)

    for aCol in 0 .. BLOCK_SIZE .. (wa - 1) do
        As.[ty, tx] <- matA.[by * BLOCK_SIZE, aCol]
        Bs.[ty, tx] <- matB.[bRow, bCol]
        wi.Barrier(CLK_LOCAL_MEM_FENCE)
 
        for k = 0 to BLOCK_SIZE - 1 do
            Csub <- Csub + (As.[ty,k] * Bs.[k,tx])
        wi.Barrier(CLK_LOCAL_MEM_FENCE)

        bRow <- bRow + bStep
    matC.[by * BLOCK_SIZE + ty, bx * BLOCK_SIZE + tx] <- Csub
 
// Sequential computation
let SimpleSequentialComp(a:float32[], b:float32[]) =
    let c = Array.zeroCreate<float32> (a.Length/2)
    for i = 0 to a.Length / 2 - 1 do
        c.[i] <- a.[i] + b.[i]
    c

// Utility functions
let FirstDeviceSupportMultiDimensionalWorkItems() =
    let device = OpenCLPlatform.Platforms.[0].Devices.[0]
    if device.MaxWorkItemDimensions > 1L then
        true
    else
        false
             
let CreateVectors size =
    let a = Array.create size 2.5f
    let b = Array.create size 3.5f
    let c = Array.zeroCreate<float32> size
    a, b, c
    
let CreateMatrices rows cols =
    let a = Array2D.create rows cols 2.5f
    let b = Array2D.create cols rows 3.5f
    let c = Array2D.zeroCreate<float32> rows rows
    a, b, c

let Create4Vectors size =
    let a = Array.create size (float4(2.5f))
    let b = Array.create size (float4(3.5f))
    let c = Array.zeroCreate<float4> size
    a, b, c
  
// Tests            
[<Test>]
let ``Can run simple vector addition``() =
    if OpenCL.OpenCLPlatform.Platforms.Count > 0 then
        let a, b, c = CreateVectors 1024
        let worksize = new WorkSize(1024L, 64L)
        <@ VectorAdd(a, b, c, worksize) @>.Run() 
        let correctResult = Array.map2 (+) a b
        Assert.AreEqual(correctResult, c)
    else
        System.Console.WriteLine("Skipping test cause no OpenCL device has been found")
                       
[<Test>]
let ``Can run vector addition with return type``() =
    if OpenCL.OpenCLPlatform.Platforms.Count > 0 then
        let a, b, _ = CreateVectors 1024
        let worksize = new WorkSize(1024L, 64L)
        let c = <@ VectorAddReturn(a, b, worksize) @>.Run() 
        let correctResult = Array.map2 (+) a b
        Assert.AreEqual(correctResult, c)
    else
        System.Console.WriteLine("Skipping test cause no OpenCL device has been found")
                    
[<Test>]
let ``Can run vector addition with utility function``() =
    if OpenCL.OpenCLPlatform.Platforms.Count > 0 then
        let a, b, c = CreateVectors 1024
        let worksize = new WorkSize(1024L, 64L)
        <@ VectorAddWithUtilityFunction(a, b, c, worksize) @>.Run() 
        let correctResult = Array.map2 (+) a b
        Assert.AreEqual(correctResult, c)
    else
        System.Console.WriteLine("Skipping test cause no OpenCL device has been found")
                
[<Test>]
let ``Can run vector addition with inline utility function``() =
    if OpenCL.OpenCLPlatform.Platforms.Count > 0 then
        let a, b, c = CreateVectors 1024
        let worksize = new WorkSize(1024L, 64L)
        <@ VectorAddWithUtilityFunctionInline(a, b, c, worksize) @>.Run() 
        let correctResult = Array.map2 (+) a b
        Assert.AreEqual(correctResult, c)
    else
        System.Console.WriteLine("Skipping test cause no OpenCL device has been found")
        
[<Test>]
let ``Can run matrix multiplication``() =
    if OpenCL.OpenCLPlatform.Platforms.Count > 0 && 
       OpenCL.OpenCLPlatform.Platforms.[0].Devices.[0].MaxWorkItemSizes.Count > 1 &&
       OpenCL.OpenCLPlatform.Platforms.[0].Devices.[0].MaxWorkItemSizes.[1] > 1L then
        let a, b, c = CreateMatrices 256 128
        let worksize = new WorkSize([| 128L; 128L |], [| 16L; 16L |])
        <@ MatrixMult(a, b, c, worksize) @>.Run() 
        let correctResult = 
            let r = Array2D.zeroCreate<float32> 256 256
            for r = 0 to a.GetLength(0) - 1 do
                for c = 0 to b.GetLength(1) - 1 do
                    let mutable accum = 0.0f
                    for k = 0 to a.GetLength(1) - 1 do
                        accum <- accum + a.[r, k] * b.[k, c]
            r
        Assert.AreEqual(correctResult, c)
    else
        System.Console.WriteLine("Skipping test cause no OpenCL device has been found")

    