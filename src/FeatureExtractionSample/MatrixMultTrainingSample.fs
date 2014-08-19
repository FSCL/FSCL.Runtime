module MatrixMultTrainingSample

open FSCL
open FSCL.Compiler
open FSCL.Runtime.Scheduling
open FSCL.Runtime.Scheduling.FeatureExtraction
open FSCL.Language
open System.Collections.Generic
open System
open System.IO
open FSCL.Runtime
open FSCL.Language
open System.Diagnostics

[<ReflectedDefinition>]
let BLOCK_SIZE = 16

// Matrix multiplication
[<ReflectedDefinition>]
let MatMul(matA: float32[], matB: float32[], matC: float32[], wi: WorkItemInfo) =
    // Block index
    let bx = wi.GroupID(0);
    let by = wi.GroupID(1);
 
    // Thread index
    let tx = wi.LocalID(0);
    let ty = wi.LocalID(1);
 
    // Index of the first sub-matrix of A processed 
    // by the block
    let aBegin = matA.GetLength(1) * BLOCK_SIZE * by;
 
    // Index of the last sub-matrix of A processed 
    // by the block
    let aEnd   = aBegin + matA.GetLength(1) - 1;
 
    // Step size used to iterate through the 
    // sub-matrices of A
    let aStep  = BLOCK_SIZE;
 
    // Index of the first sub-matrix of B processed 
    // by the block
    let bBegin = BLOCK_SIZE * bx;
 
    // Step size used to iterate through the 
    // sub-matrices of B
    let bStep  = BLOCK_SIZE * matB.GetLength(1);
 
    let mutable b = bBegin
    let mutable Csub = 0.0f

    // Loop over all the sub-matrices of A and B
    // required to compute the block sub-matrix
    for a in aBegin .. BLOCK_SIZE .. aEnd do
        // Declaration of the local memory array As 
        // used to store the sub-matrix of A
        let As = local(Array2D.zeroCreate<float32> BLOCK_SIZE BLOCK_SIZE)
        let Bs = local(Array2D.zeroCreate<float32> BLOCK_SIZE BLOCK_SIZE)
 
        // Load the matrices from global memory
        // to local memory; each thread loads
        // one element of each matrix
        As.[ty, tx] <- matA.[a + (matA.GetLength(1) * ty) + tx]
        Bs.[ty, tx] <- matB.[b + (matB.GetLength(1) * ty) + tx]
 
        // Synchronize to make sure the matrices 
        // are loaded
        wi.Barrier(CLK_LOCAL_MEM_FENCE)
 
        // Multiply the two matrices together;
        // each thread computes one element
        // of the block sub-matrix
        for k = 0 to BLOCK_SIZE - 1 do
            Csub <- Csub + (As.[ty, k] * Bs.[k, tx])
 
        // Synchronize to make sure that the preceding
        // computation is done before loading two new
        // sub-matrices of A and B in the next iteration
        wi.Barrier(CLK_LOCAL_MEM_FENCE)

        b <- b + bStep
 
    // Write the block sub-matrix to device memory;
    // each thread writes one element
    let c = (matB.GetLength(1) * BLOCK_SIZE * by) + (BLOCK_SIZE * bx)
    matC.[c + (matB.GetLength(1) * ty) + tx] <- Csub
    
// Matrix multiplication
[<ReflectedDefinition>]
let MatMulCPU(matA: float32[,], matB: float32[,], matC: float32[,], wi: WorkItemInfo) =
    let r = wi.GlobalID(0)
    let c = wi.GlobalID(1)
    if(r < matA.GetLength(0) && c < matA.GetLength(1)) then
        let t = (matA.GetLength(1) / 8)
        let mutable accum = 0.0f
        for i = 0 to t - 1 do
            accum <- accum + matA.[r, i] * matB.[i, c]
            accum <- accum + matA.[r, i + 1] * matB.[i + 1, c]
            accum <- accum + matA.[r, i + 2] * matB.[i + 2, c]
            accum <- accum + matA.[r, i + 3] * matB.[i + 3, c]
            accum <- accum + matA.[r, i + 4] * matB.[i + 4, c]
            accum <- accum + matA.[r, i + 5] * matB.[i + 5, c]
            accum <- accum + matA.[r, i + 6] * matB.[i + 6, c]
            accum <- accum + matA.[r, i + 7] * matB.[i + 7, c]

        for i = t * 8 to matA.GetLength(1) - 1 do
            accum <- accum + matA.[r, i] * matB.[i, c]
        matC.[r, c] <- accum
      
let Verify(output: float32[,], expected: float32[,]) =
    let mutable found = false
    let mutable i = 0
    let mutable j = 0
    while not found && i < output.GetLength(0) do
        j <- 0
        while not found && j < output.GetLength(1) do
            if output.[i,j] <> expected.[i,j] then
                found <- true
            j <- j + 1
        i <- i + 1
    not found

type MatrixMultSimpleTrainingSample() =    
    inherit IDefaultFeatureExtractionTrainingSample()

    override this.DefaultConfigurationDictionary() =
        let dict = new Dictionary<string, obj>()
        dict.Add("MinMatrixSize", 64L)
        dict.Add("MaxMatrixSize", 2048L)
        dict.Add("Iterations", 100)
        dict
        
    override this.Verify(output: obj, reference: obj) =
        (output :?> float32[,]) = (reference :?> float32[,])
    
    override this.CreateVerifiedOutput(o: obj) =
        let a, b = o :?> float32[,] * float32[,]
        let res = Array2D.zeroCreate<float32> (a.GetLength(0)) (b.GetLength(1))
        for row = 0 to a.GetLength(0) - 1  do
            for col = 0 to b.GetLength(1) - 1 do
                let mutable v = 0.0f
                for k = 0 to a.GetLength(1) - 1 do
                    v <- v + (a.[row, k] * b.[k, col])
                res.[row, col] <- v
        box res

    override this.ResultColumnIDs 
        with get() =   
            let ids = new List<String>()         
            for pIndex, pName, pDevs in GetOpenCLPlatforms() do  
                for dIndex, dName, dType in pDevs do  
                    ids.Add(dName + " Completion Time (ms)")
            ids.Add("Matrix Width (elements)")
            ids.Add("Matrix Height (elements)")
            ids |> List.ofSeq
    
    override this.RunInternal(chain, conf) = 
        let configuration = IDefaultFeatureExtractionTrainingSample.ConfigurationToDictionary(conf)
        let minSize = Int64.Parse(configuration.["MinMatrixSize"])
        let maxSize = Int64.Parse(configuration.["MaxMatrixSize"])
        let iterations = Int32.Parse(configuration.["Iterations"])

        let compiler = new Compiler()
        let opts = new Dictionary<string, obj>()        
        let rnd = System.Random()

        let rm = BufferReadMode.EnqueueReadBuffer
        let wm = BufferWriteMode.EnqueueWriteBuffer
        let ifl = MemoryFlags.UseHostPointer ||| MemoryFlags.ReadOnly
        let ofl = MemoryFlags.UseHostPointer ||| MemoryFlags.WriteOnly

        let mutable execResults: obj list list = []
                
        let sizes = (seq {
                            let s = ref minSize
                            while !s <= maxSize do
                                yield (!s + 1L, !s + 1L)
                                //yield (!s, !s * 2L)
                                s := !s + 64L
                        }) |> Array.ofSeq

        for rows, cols in sizes do
            Console.WriteLine("      Size: " + String.Format("{0,5:#####}", rows) + "x" + String.Format("{0,5:#####}", cols))
                                          
            let a = Array2D.init (rows |> int) (cols |> int) (fun r c -> rnd.Next() % 10 |> float32)
            let b = Array2D.init (cols |> int) (rows |> int) (fun r c -> rnd.Next() % 10 |> float32)
            let reference = this.CreateVerifiedOutput((a, b)) :?> float32[,]

            let mutable features: obj list = []
            let mutable instanceResult: obj list = []

            for pIndex, pName, pDevs in GetOpenCLPlatforms() do   
                for dIndex, dName, dType in pDevs do
                    if dIndex > 0 then
                        let c = Array2D.zeroCreate (rows |> int) (cols |> int)
                        let ws = new WorkSize([| ((cols / (BLOCK_SIZE |> int64)) + 1L) * (BLOCK_SIZE |> int64); ((rows / (BLOCK_SIZE |> int64)) + 1L) * (BLOCK_SIZE |> int64) |], [| BLOCK_SIZE |> int64; BLOCK_SIZE |> int64 |])                                    
                        Console.WriteLine(" Device " + ": " + dName.ToString() + "(" + dType.ToString() + ")")  
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
                                                    c)),
                                            ws)) @>
                        // Extract features
                        let km = compiler.Compile(comp, opts) :?> IKernelModule
                        let precomputedFeatures = chain.Precompute(km)
                        features <- chain.Evaluate(km, precomputedFeatures, [ a; b; c; ws ], opts)

                        // Run once to skip compilation time
                        comp.Run()
                        if not (this.Verify(c, reference)) then
                            Console.WriteLine("---------------- COMPUTATION RESULT ERROR")
                        else                        
                            // Run
                            let watch = new Stopwatch()
                            watch.Start()
                            for i = 0 to iterations - 1 do
                                comp.Run()
                            watch.Stop()
                            let ttime, iters = ((double)watch.ElapsedMilliseconds) /((double)iterations), iterations
                                        
                            Console.WriteLine("---------------- " + String.Format("{0,11:######0.0000}", ttime) + "ms (" + String.Format("{0,10:#########0}", iters) + " iterations)")
                            instanceResult <- instanceResult @ [ ttime ]
                            System.Threading.Thread.Sleep(500)
                                
            execResults <- execResults @ [ instanceResult @ [rows; cols] @ features ]       
        execResults

type MatrixMultAdvancedTrainingSample() =    
    inherit MatrixMultSimpleTrainingSample()

    override this.RunInternal(chain, conf) = 
        let configuration = IDefaultFeatureExtractionTrainingSample.ConfigurationToDictionary(conf)
        let minSize = Int64.Parse(configuration.["MinMatrixSize"])
        let maxSize = Int64.Parse(configuration.["MaxMatrixSize"])
        let iterations = Int32.Parse(configuration.["Iterations"])

        let compiler = new Compiler()
        let opts = new Dictionary<string, obj>()        
        let rnd = System.Random()
        
        let rm = BufferReadMode.EnqueueReadBuffer
        let wm = BufferWriteMode.EnqueueWriteBuffer
        let ifl = MemoryFlags.UseHostPointer ||| MemoryFlags.ReadOnly
        let ofl = MemoryFlags.UseHostPointer ||| MemoryFlags.WriteOnly

        let mutable execResults: obj list list = []
                
        let sizes = (seq {
                            let s = ref minSize
                            while !s <= maxSize do
                                yield (!s + 0L, !s + 0L)
                                //yield (!s, !s * 2L)
                                s := !s + 64L
                        }) |> Array.ofSeq

        for rows, cols in sizes do
            Console.WriteLine("      Size: " + String.Format("{0,5:#####}", rows) + "x" + String.Format("{0,5:#####}", cols))
                                          
            let a = Array.init (rows * cols |> int) (fun i -> rnd.Next() % 10 |> float32)
            let b = Array.init (cols * cols |> int) (fun i -> rnd.Next() % 10 |> float32)
            let reference = this.CreateVerifiedOutput((a, b)) :?> float32[,]

            let mutable features: obj list = []
            let mutable instanceResult: obj list = []

            for pIndex, pName, pDevs in GetOpenCLPlatforms() do   
                for dIndex, dName, dType in pDevs do
                    if dIndex > 0 then
                        let c = Array.zeroCreate (rows * cols |> int)
                        let ws = new WorkSize([| ((cols / (BLOCK_SIZE |> int64)) + 0L) * (BLOCK_SIZE |> int64); ((rows / (BLOCK_SIZE |> int64)) + 0L) * (BLOCK_SIZE |> int64) |], [| BLOCK_SIZE |> int64; BLOCK_SIZE |> int64 |])    
                                        
                        Console.WriteLine(" Device " + ": " + dName.ToString() + "(" + dType.ToString() + ")")  
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
                                                    c)),
                                            ws)) @>

                        let km = compiler.Compile(comp, opts) :?> IKernelModule
                        let precomputedFeatures = chain.Precompute(km)
                        features <- chain.Evaluate(km, precomputedFeatures, [ a; b; c; ws ], opts)

                        // Run once to skip compilation time
                        comp.Run()
                        if not (this.Verify(c, reference)) then
                            Console.WriteLine("---------------- COMPUTATION RESULT ERROR")
                        else                        
                            // Run
                            let watch = new Stopwatch()
                            watch.Start()
                            for i = 0 to iterations - 1 do
                                comp.Run()
                            watch.Stop()
                            let ttime, iters = ((double)watch.ElapsedMilliseconds) /((double)iterations), iterations
                                        
                            Console.WriteLine("---------------- " + String.Format("{0,11:######0.0000}", ttime) + "ms (" + String.Format("{0,10:#########0}", iters) + " iterations)")
                            instanceResult <- instanceResult @ [ ttime ]
                            System.Threading.Thread.Sleep(500)
                                
            execResults <- execResults @ [ instanceResult @ [rows; cols] @ features ]       
        execResults
         