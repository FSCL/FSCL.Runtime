module FSCL.Runtime.Tests.StructsAndRecords

open FSCL
open FSCL.Runtime
open FSCL.Compiler
open FSCL.Language
open OpenCL
open NUnit.Framework
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

// Simple vector addition with struct
[<ReflectedDefinition>]
let VectorAddStruct(a: MyStruct[], b:MyStruct[], c:MyStruct[], wi:WorkItemInfo) =
    let gid = wi.GlobalID(0)
    // A little verbose just to test correct codegen of different constructs
    let mutable newStruct = new MyStruct()
    newStruct.x <- a.[gid].x + b.[gid].x
    newStruct.y <- a.[gid].y + b.[gid].y
    c.[gid] <- newStruct
    
// Simple vector addition with struct constructor
[<ReflectedDefinition>]
let VectorAddStructWithConstructor(a: MyStruct[], b:MyStruct[], c:MyStruct[], wi:WorkItemInfo) =
    let gid = wi.GlobalID(0)
    // A little verbose just to test correct codegen of different constructs
    let mutable newStruct = new MyStruct(a.[gid].x + b.[gid].x, a.[gid].y + b.[gid].y)
    c.[gid] <- newStruct
    
// Simple vector addition with record
[<ReflectedDefinition>]
let VectorAddRecord(a: MyRecord[], b:MyRecord[], c:MyRecord[], wi:WorkItemInfo) =
    let gid = wi.GlobalID(0)
    let newRecord = { x = a.[gid].x + b.[gid].x;  y = a.[gid].y + b.[gid].y }
    c.[gid] <- newRecord
             
let CreateStructVectors size =
    let a = Array.create size (new MyStruct(1.0f, 2.0f))
    let b = Array.create size (new MyStruct(4.0f, 3.0f))
    let c = Array.zeroCreate<MyStruct> size
    a, b, c

let CreateRecordVectors size =
    let a = Array.create size ({ x = 1.0f; y = 2.0f })
    let b = Array.create size ({ x = 4.0f; y = 3.0f })
    let c = Array.create size ({ x = 0.0f; y = 0.0f })
    a, b, c
  
// Tests            
[<Test>]
let ``Can run vector addition with structs``() =
    if OpenCL.OpenCLPlatform.Platforms.Count > 0 then
        let a, b, c = CreateStructVectors 1024
        let worksize = new WorkSize(1024L, 64L)
        <@ VectorAddStruct(a, b, c, worksize) @>.Run() 
        let correctResult = (a, b) ||> Array.map2 (fun s1 s2 -> new MyStruct(s1.x + s2.x, s1.y + s2.y))
        Assert.AreEqual(correctResult, c)
    else
        System.Console.WriteLine("Skipping test cause no OpenCL device has been found")
                       
[<Test>]
let ``Can run vector addition with structs created with non-default constructor``() =
    if OpenCL.OpenCLPlatform.Platforms.Count > 0 then
        let a, b, c = CreateStructVectors 1024
        let worksize = new WorkSize(1024L, 64L)
        <@ VectorAddStructWithConstructor(a, b, c, worksize) @>.Run() 
        let correctResult = (a, b) ||> Array.map2 (fun s1 s2 -> new MyStruct(s1.x + s2.x, s1.y + s2.y))
        Assert.AreEqual(correctResult, c)
    else
        System.Console.WriteLine("Skipping test cause no OpenCL device has been found")
                    
[<Test>]
let ``Can run vector addition with records``() =
    if OpenCL.OpenCLPlatform.Platforms.Count > 0 then
        let a, b, c = CreateRecordVectors 1024
        let worksize = new WorkSize(1024L, 64L)
        <@ VectorAddRecord(a, b, c, worksize) @>.Run() 
        let correctResult = (a, b) ||> Array.map2 (fun s1 s2 -> { x = s1.x + s2.x; y = s1.y + s2.y })
        Assert.AreEqual(correctResult, c)        
    else
        System.Console.WriteLine("Skipping test cause no OpenCL device has been found")
    