module FSCL.Runtime.Tests.OpenCLKernelExecutionTests

open FSCL
open FSCL.Runtime
open FSCL.Compiler
open FSCL.Language
open FSCL.Runtime.Tests.Kernels

open OpenCL
open NUnit.Framework
  
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
    