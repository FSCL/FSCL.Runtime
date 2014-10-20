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

// Matrix multiplication with local and reference to global var (BLOCK_SIZE)
[<ReflectedDefinition>]
let ZeroOp (wi: WorkItemInfo, a:float32[]) =
    a.[0] <- a.[0] * a.[1]
    a

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
 

let FirstDeviceSupportMultiDimensionalWorkItems() =
    let device = OpenCLPlatform.Platforms.[0].Devices.[0]
    if device.MaxWorkItemDimensions > 1L then
        true
    else
        false

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
        MemoryFlagsSample.Run()
        AlgorithmSample.Run()
        UtilityFunctionSample.Run() 
        DataTypeSample.Run()
        ReturnTypeSample.Run()
        AcceleratedCollectionSample.Run()
        AcceleratedCollectionDataTypeSample.Run()
        SequentialAndMultithreadSample.Run()
        
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


    