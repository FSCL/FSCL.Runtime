open FSCL
open FSCL.Language
open FSCL.Runtime
open System.Reflection
open System.Reflection.Emit
open System
open System.Collections.Generic
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Linq.RuntimeHelpers
open OpenCL
open System.Diagnostics

let FirstDeviceSupportMultiDimensionalWorkItems() =
    let device = OpenCLPlatform.Platforms.[0].Devices.[0]
    if device.MaxWorkItemDimensions > 1L then
        true
    else
        false

[<ReflectedDefinition>]
let VectorAddWithOptions(wi: WorkItemInfo, a:float32 option[], b:float32 option[], c:float32 option[]) =
    let gid = wi.GlobalID(0)
    if a.[gid].IsSome && b.[gid].IsSome then
        c.[gid] <- Some(a.[gid].Value + b.[gid].Value)


[<EntryPoint>]
let main argv =
    // Check opencl devices
    let plats = GetOpenCLPlatforms()
    if plats.Count = 0 then
        Console.WriteLine("No OpenCL-enabled device found on this platform")
    else
        // Show OpenCL devices
        Console.WriteLine("Your OpenCL-enabled devices are listed below")

        


        (*
        let size = new WorkSize(64L, 64L)
        let a = Array.zeroCreate<float32> 64
        let comp = <@ ZeroOp(size, a) @>
        let mutable r = null
        let iter = 10000
        let watch = new Stopwatch()
        watch.Start()
        for i = 1 to iter do
            r <- comp.Run()
        watch.Stop()
        Console.WriteLine("Elapsed time: " + (((double)watch.ElapsedMilliseconds)/((double)iter)).ToString() + " ms")
        *)
        //MemoryFlagsSample.Run()
        Console.WriteLine("::::::::::: Algorithms Sample :::::::::::")
        AlgorithmSample.Run()
//        Console.WriteLine(":::::::::::::::::::::::::::::::::::::::::")
//        Console.WriteLine("\n::::::::::: Utility Functions Sample :::::::::::")
//        UtilityFunctionSample.Run() 
//        Console.WriteLine("::::::::::::::::::::::::::::::::::::::::::::::::")
//        Console.WriteLine("\n::::::::::: Static/Dynamic Defines Sample :::::::::::")
//        DynamicDefineSample.Run() 
//        Console.WriteLine(":::::::::::::::::::::::::::::::::::::::::::::::::::::")
//        Console.WriteLine("\n::::::::::: Data Types Sample :::::::::::")
//        DataTypeSample.Run()
//        Console.WriteLine(":::::::::::::::::::::::::::::::::::::::::")
//        Console.WriteLine("\n::::::::::: Return Types Sample :::::::::::")
//        ReturnTypeSample.Run()
//        Console.WriteLine(":::::::::::::::::::::::::::::::::::::::::::")
//        Console.WriteLine("\n::::::::::: Accelerated Collections Sample :::::::::::")
//        AcceleratedCollectionSample.Run()
//        Console.WriteLine("::::::::::::::::::::::::::::::::::::::::::::::::::::::")
//        Console.WriteLine("\n::::::::::: Collection Data Types Sample :::::::::::")
//        AcceleratedCollectionDataTypeSample.Run()
//        Console.WriteLine("::::::::::::::::::::::::::::::::::::::::::::::::::::")
//        Console.WriteLine("\n::::::::::: Sequential and Multithread Sample :::::::::::")
//        SequentialAndMultithreadSample.Run()
//        Console.WriteLine(":::::::::::::::::::::::::::::::::::::::::::::::::::::::::")
//        
            (*
        // ***************************************************************************************************
        // Expression of multiple kernels ****************************************************************************
        if FirstDeviceSupportMultiDimensionalWorkItems() then
            Console.WriteLine("")
            Console.WriteLine("# Testing expression made of multiple kernels (matrix multiplication followed by a sum) on the first device")
            let multWorkSize = new WorkSize([| matSizel; matSizel |], [| 8L; 8L |])
            let addWorkSize = new WorkSize(matSizel, 8L)
            timer.Start()
            let r = <@ MatrixAdd(
                            MatrixMult(am, bm, multWorkSize),
                            cm, dm, addWorkSize)
                     @>.Run(opts)
            timer.Stop()
            // Check result
            let mutable isResultCorrect = true
            for i = 0 to correctMatMulAdd.GetLength(0) - 1 do
                for j = 0 to correctMatMulAdd.GetLength(1) - 1 do
                    if correctMatMulAdd.[i, j] <> dm.[i, j] then
                        isResultCorrect <- false
            if not isResultCorrect then
                Console.WriteLine("  Multikernel returned a wrong result!")
            else
                Console.WriteLine("  First multikernel execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")
        // ***************************************************************************************************
        

        // ***************************************************************************************************
        // Composition of collection functions and custom kernel
        let workSize = new WorkSize(a.LongLength, 128L)
        Console.WriteLine("")
        Console.WriteLine("# Testing expressions containing a collection functions, custom kernels and sequential computations")
        timer.Restart()
        let r = <@ Array.reduce sumCurried 
                                (SimpleSequentialComp ((Array.map2 sumCurried a b), 
                                                        VectorAddReturn(a, b, worksize))) @>.Run()
        timer.Stop()
        Console.WriteLine("  Expression execution time: " + timer.ElapsedMilliseconds.ToString() + "ms")
        // ***************************************************************************************************

        // ***************************************************************************************************
        // Composition of collection functions and custom kernel
        Console.WriteLine("")
        Console.WriteLine("# Testing expression with sub-executor")
        // This is a wrapper for a multi-kernel computation
        let MapReduce (a:float32[]) (b:float32[]) =
            <@ Array.reduce sumCurried (Array.map2 sumCurried a b) @>.Run()

        timer.Restart()
        let r = <@ MapReduce (VectorAddReturn(a, b, workSize)) b @>.Run()
        timer.Stop()
        Console.WriteLine("  Expression with sub-executor execution time: " + timer.ElapsedMilliseconds.ToString() + "ms")
        // ***************************************************************************************************
        *)
    Console.WriteLine("Press Enter to exit...")
    Console.Read() |> ignore
    0


    