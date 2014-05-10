module MatrixMultTest

open Utils
open FSCL.Compiler
open FSCL.Compiler.Language
open FSCL.Runtime
open FSCL.Runtime.Language
open System
open System.IO
open System.Diagnostics

[<ReflectedDefinition>]
let BLOCK_SIZE = 16

[<ReflectedDefinition; DynamicConstantDefine>]
let mutable FILTER_WIDTH = 3

// Matrix multiplication
[<ReflectedDefinition>]
let MatMul(matA: float32[,], matB: float32[,], matC: float32[,]) =
    // Block index
    let bx = get_group_id(0)
    let by = get_group_id(1) 
    // Thread index
    let tx = get_local_id(0)
    let ty = get_local_id(1)
    // Matrix width
    let wa = matA.GetLength(1)
    let wb = matB.GetLength(1)

    // Index of the first sub-matrix of B processed 
    // by the block
    let bCol = bx * BLOCK_SIZE
    let bBeginRow = 0
    // Step size used to iterate through the 
    // sub-matrices of B
    let bStep  = BLOCK_SIZE
 
    // Loop over all the sub-matrices of A and B
    // required to compute the block sub-matrix
    let mutable bRow = bBeginRow
    let mutable Csub = 0.0f
    
    // Declaration of the local memory array As 
    // used to store the sub-matrix of A
    let As = local(Array2D.zeroCreate<float32> BLOCK_SIZE BLOCK_SIZE)
 
    // Declaration of the local memory array Bs 
    // used to store the sub-matrix of B
    let Bs = local(Array2D.zeroCreate<float32> BLOCK_SIZE BLOCK_SIZE)

    for aCol in 0 .. BLOCK_SIZE .. (wa - 1) do
        // Load the matrices from global memory
        // to local memory; each thread loads
        // one element of each matrix
        As.[ty, tx] <- matA.[by * BLOCK_SIZE, aCol]
        Bs.[ty, tx] <- matB.[bRow, bCol]
 
        // Synchronize to make sure the matrices 
        // are loaded
        barrier(CLK_LOCAL_MEM_FENCE)
 
        // Multiply the two matrices together;
        // each thread computes one element
        // of the block sub-matrix
        for k = 0 to BLOCK_SIZE - 1 do
            Csub <- Csub + (As.[ty,k] * Bs.[k,tx])
 
        // Synchronize to make sure that the preceding
        // computation is done before loading two new
        // sub-matrices of A and B in the next iteration
        barrier(CLK_LOCAL_MEM_FENCE)

        bRow <- bRow + bStep
         
    // Write the block sub-matrix to device memory;
    // each thread writes one element
    matC.[by * BLOCK_SIZE + ty, bx * BLOCK_SIZE + tx] <- Csub
        
        
// Matrix multiplication
[<ReflectedDefinition>]
let MatMulCPU(matA: float32[,], matB: float32[,], matC: float32[,]) =
    let r = get_global_id(0)
    let c = get_global_id(1)
   
    let mutable accum = 0.0f
    for i = 0 to (matA.GetLength(1) >>> 3) - 1 do
        accum <- accum + matA.[r, i] * matB.[i, c]
        accum <- accum + matA.[r, i + 1] * matB.[i + 1, c]
        accum <- accum + matA.[r, i + 2] * matB.[i + 2, c]
        accum <- accum + matA.[r, i + 3] * matB.[i + 3, c]
        accum <- accum + matA.[r, i + 4] * matB.[i + 4, c]
        accum <- accum + matA.[r, i + 5] * matB.[i + 5, c]
        accum <- accum + matA.[r, i + 6] * matB.[i + 6, c]
        accum <- accum + matA.[r, i + 7] * matB.[i + 7, c]
    matC.[r, c] <- accum

let Verify(r: float32[,], cols: long) =
    let mutable found = false
    let mutable i = 0
    let mutable j = 0
    while not found && i < r.GetLength(0) do
        j <- 0
        while not found && j < r.GetLength(1) do
            if r.[i, j] <> 2.0f * 5.0f * (float32)(cols) then
                found <- true
            j <- j + 1
        i <- i + 1
    not found
        
let VerifyCPU(r: float4[,], cols: long) =
    let mutable found = false
    let mutable i = 0
    let mutable j = 0
    while not found && i < r.GetLength(0) do
        j <- 0
        while not found && j < r.GetLength(1) do
            if r.[i, j] <> float4(2.0f * 5.0f * (float32)cols) then
                found <- true
            j <- j + 1
        i <- i + 1
    not found

let DoTest(minSize: long, maxSize: long, iters: int) =
    let inputReadModes = [ BufferReadMode.EnqueueReadBuffer; BufferReadMode.MapBuffer ]
    let outputWriteModes = [ BufferWriteMode.EnqueueWriteBuffer; BufferWriteMode.MapBuffer ]
    let inputFlags = [ MemoryFlags.HostWriteOnly ||| MemoryFlags.ReadOnly;
                       MemoryFlags.HostWriteOnly ||| MemoryFlags.UseHostPointer ||| MemoryFlags.ReadOnly;
                       MemoryFlags.HostWriteOnly ||| MemoryFlags.UsePersistentMemAMD ||| MemoryFlags.ReadOnly; ]
    let outputFlags = [ MemoryFlags.HostReadOnly ||| MemoryFlags.WriteOnly;
                        MemoryFlags.HostReadOnly ||| MemoryFlags.UseHostPointer ||| MemoryFlags.WriteOnly;
                        MemoryFlags.HostReadOnly ||| MemoryFlags.UsePersistentMemAMD ||| MemoryFlags.WriteOnly; ]

    let wr = new StreamWriter("MatrixMult.csv", false)
    wr.WriteLine("PLATFORM;DEVICE;READ MODE;WRITE MODE;INPUT FLAGS;OUTPUT FLAGS;SIZE;TIME;ITERATIONS")

    let sizes = (seq {
                        let s = ref minSize
                        while !s < maxSize do
                            yield (!s, !s)
                            yield (!s + (!s / 2L), !s + (!s / 2L))
                            s := !s * 2L
                        yield (!s, !s)
                    }) |> Array.ofSeq

    for pIndex, pName, pDevs in GetOpenCLPlatforms() do        
        Console.WriteLine("Platform: " + pName.ToString())
        for dIndex, dName, dType in pDevs do
            if dIndex >= 2 then
                Console.WriteLine(" Device " + ": " + dName.ToString() + "(" + dType.ToString() + ")")

                for rm in inputReadModes do
                    Console.WriteLine("  ReadMode: " + rm.ToString())
                    for wm in outputWriteModes do
                        Console.WriteLine("   WriteMode: " + wm.ToString())
                        for ifl in inputFlags do
                            Console.WriteLine("    Input flags: " + ifl.ToString())
                            for ofl in outputFlags do
                                Console.WriteLine("     Output flags: " + ofl.ToString())
                                for rows, cols in sizes do
                                    Console.WriteLine("      Size: " + String.Format("{0,5:#####}", rows) + "x" + String.Format("{0,5:#####}", cols))
                                                                        
                                    if dType = DeviceType.Cpu then
                                        let a = Array2D.create (rows |> int) (cols |> int) 2.0f 
                                        let b = Array2D.create (cols |> int) (rows |> int) 5.0f 
                                        let c = Array2D.zeroCreate (rows |> int) (rows |> int)

                                        let comp = <@ DEVICE(pIndex, dIndex,
                                                        MatMulCPU(
                                                            BUFFER_READ_MODE(rm, 
                                                             MEMORY_FLAGS(ifl, 
                                                                a)),
                                                            BUFFER_READ_MODE(rm, 
                                                             MEMORY_FLAGS(ifl, 
                                                                b)),
                                                            BUFFER_WRITE_MODE(wm, 
                                                             MEMORY_FLAGS(ofl, 
                                                                c)))) @>
                                        // Run once to skip compilation time
                                        comp.Run([| cols; rows |], [| 16L; 16L |])
                                        if not (Verify(c, cols)) then
                                            Console.WriteLine("---------------- COMPUTATION RESULT ERROR")
                                        else
                                            // Run
                                            let watch = new Stopwatch()
                                            watch.Start()
                                            for i = 0 to iters - 1 do
                                                comp.Run([| cols; rows |], [| 16L; 16L |])
                                            watch.Stop()
                                            let ttime, iters = ((double)watch.ElapsedMilliseconds) /((double)iters), iters
                                            //let ttime, iters = Utils.ExcuteFor 1000.0 (fun () -> comp.Run(!size, 64L))
                                
                                            Console.WriteLine("---------------- " + String.Format("{0,11:######0.0000}", ttime) + "ms (" + String.Format("{0,10:#########0}", iters) + " iterations)")
                                            let s = String.Format("{0};{1};{2};{3};{4};{5};{6};{7};{8};", 
                                                            pIndex, 
                                                            (dIndex.ToString() +  " " + dName.ToString()), 
                                                            rm, wm, ifl, ofl, 
                                                            rows * cols,
                                                            ttime,
                                                            iters)
                                            wr.WriteLine(s)
                                    else
                                        let a = Array2D.create (rows |> int) (cols |> int) 2.0f 
                                        let b = Array2D.create (cols |> int) (rows |> int) 5.0f 
                                        let c = Array2D.zeroCreate (rows |> int) (rows |> int)
                                        
                                        let comp = <@ DEVICE(pIndex, dIndex,
                                                        MatMul(
                                                            BUFFER_READ_MODE(rm, 
                                                             MEMORY_FLAGS(ifl, 
                                                                a)),
                                                            BUFFER_READ_MODE(rm, 
                                                             MEMORY_FLAGS(ifl, 
                                                                b)),
                                                            BUFFER_WRITE_MODE(wm, 
                                                             MEMORY_FLAGS(ofl, 
                                                                c)))) @>
                                        // Run once to skip compilation time
                                        comp.Run([| cols; rows |], [| 16L; 16L |])
                                        if not (Verify(c, cols)) then
                                            Console.WriteLine("---------------- COMPUTATION RESULT ERROR")
                                        else
                                            // Run
                                            let watch = new Stopwatch()
                                            watch.Start()
                                            for i = 0 to iters - 1 do
                                                comp.Run([| cols; rows |], [| 16L; 16L |])
                                            watch.Stop()
                                            let ttime, iters = ((double)watch.ElapsedMilliseconds) /((double)iters), iters
                                            //let ttime, iters = Utils.ExcuteFor 1000.0 (fun () -> comp.Run(!size, 64L))
                                
                                            Console.WriteLine("---------------- " + String.Format("{0,11:######0.0000}", ttime) + "ms (" + String.Format("{0,10:#########0}", iters) + " iterations)")
                                            let s = String.Format("{0};{1};{2};{3};{4};{5};{6};{7};{8}", 
                                                            pIndex, 
                                                            (dIndex.ToString() +  " " + dName.ToString()), 
                                                            rm, wm, ifl, ofl, 
                                                            rows * cols,
                                                            ttime,
                                                            iters)
                                            wr.WriteLine(s)
                                    
    wr.Close()
                    