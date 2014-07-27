module FSCL.Runtime.Tests.MutlithreadKernelExecutionTest

open FSCL
open FSCL.Runtime
open FSCL.Compiler
open FSCL.Language
open FSCL.Runtime.Tests.Kernels

open OpenCL
open NUnit.Framework

// Tests            
[<Test>]
let ``Can run simple vector addition multithreading``() =
    let a, b, c = CreateVectors 1024
    let worksize = new WorkSize(1024L, 64L)
    <@ RUNNING_MODE(RunningMode.Multithread, VectorAdd(a, b, c, worksize)) @>.Run() 
    let correctResult = Array.map2 (+) a b
    Assert.AreEqual(correctResult, c)    

[<Test>]
let ``Can run simple kernel with local memory multithreading``() =
    let a, b, c = CreateMatrices 4
    let worksize = new WorkSize([| 4L; 4L |], [| 4L; 4L |])
    let As = Array2D.zeroCreate<float32> 4 4
    let Bs = Array2D.zeroCreate<float32> 4 4
    <@ RUNNING_MODE(RunningMode.Multithread, MatrixMultAdvancedLocalParams(a, b, c, As, Bs, worksize)) @>.Run() 
    let correctResult = Array2D.zeroCreate<float32> 4 4
    for r = 0 to 3 do
        for c = 0 to 3 do
            let mutable s = 0.0f
            for k = 0 to 3 do
                s <- s + (a.[r, k] * b.[k, c])
            correctResult.[r, c] <- s
    Assert.AreEqual(correctResult, c)    
                       
    