namespace FSCL.Runtime.Scheduling.FRTSchedulingEngine.TrainingSamples

open FSCL
open FSCL.Compiler
open FSCL.Runtime.Scheduling
open FSCL.Runtime.Scheduling.FRTSchedulingEngine
open FSCL.Language
open System.Collections.Generic
open System
open System.IO
open FSCL.Runtime
open FSCL.Language
open System.Diagnostics
open System.Linq
open Microsoft.FSharp.Quotations

[<FRTFeatureExtractionTrainingSample("MatrixMultNaive")>]
type MatrixMultSimpleTrainingSample() =    
    inherit FRTFeatureExtractionTrainingSample()
    
    // Matrix multiplication
    [<ReflectedDefinition>]
    let MatMulCPU(matA: float32[,], matB: float32[,], matC: float32[,], wi: WorkItemInfo) =
        let r = wi.GlobalID(1)
        let c = wi.GlobalID(0)
    
        // Unroll 8
        let mutable accum = 0.0f
        if r < matA.GetLength(0) && c < matB.GetLength(1) then
            for i = 0 to matA.GetLength(1) - 1 do
                accum <- accum + matA.[r, i] * matB.[i, c]
            matC.[r, c] <- accum
                  
    [<ConfigurationItem>]
    member val MinMatrixSize = 64L with get, set    
    [<ConfigurationItem>]
    member val MaxMatrixSize = 2048L with get, set    
    [<ConfigurationItem>]
    member val Iterations = 30 with get, set
        
    member this.Verify(output: float32[,], reference: float32[,]) =
        output = reference
    
    member this.CreateVerifiedOutput(a: float32[,], b:float32[,]) =
        let res = Array2D.zeroCreate<float32> (a.GetLength(0)) (b.GetLength(1))
        for row = 0 to a.GetLength(0) - 1  do
            for col = 0 to b.GetLength(1) - 1 do
                let mutable v = 0.0f
                for k = 0 to a.GetLength(1) - 1 do
                    v <- v + (a.[row, k] * b.[k, col])
                res.[row, col] <- v
        res
                
    override this.Run(features, devices, opts) =
        let rm = opts.["RunningMode"] :?> TrainingSampleRunningMode
        let runtimeRun = opts.["RuntimeRun"] :?> obj-> obj

        let featureOnly = rm = TrainingSampleRunningMode.OnlyFeatures
        let etOnly = rm = TrainingSampleRunningMode.OnlyExecutionTime
        
        let compiler = new Compiler()
        let opts = new Dictionary<string, obj>()        
        let rnd = System.Random()

        let rm = BufferReadMode.EnqueueReadBuffer
        let wm = BufferWriteMode.EnqueueWriteBuffer
        let ifl = MemoryFlags.ReadOnly ||| MemoryFlags.UseHostPointer
        let ofl = MemoryFlags.WriteOnly ||| MemoryFlags.UseHostPointer
        
        let executionResults = new List<float32 list>()
        let featureValues = new List<float32 list>()
                
        let sizes = (seq {
                            let s = ref this.MinMatrixSize
                            while !s <= this.MaxMatrixSize do
                                yield (!s, !s)
                                yield (!s + 1L, !s + 1L)
                                s := !s + this.MinMatrixSize
                        }) |> Array.ofSeq
                            
        // Get complation times
        for rows, cols in sizes do
            let times = List<float32>()

            let a = Array2D.init (rows |> int) (cols |> int) (fun r c -> rnd.Next() % 5 |> float32)
            let b = Array2D.init (cols |> int) (rows |> int) (fun r c -> rnd.Next() % 5 |> float32)
            let reference = 
                if not featureOnly then
                    Array2D.zeroCreate<float32> 1 1
                    //this.CreateVerifiedOutput((a, b)) :?> float32[,]
                else
                    Array2D.zeroCreate<float32> 1 1
            let c = Array2D.zeroCreate (rows |> int) (cols |> int)
            let ws = new WorkSize([| ((((cols |> int) - 1) / 16) + 1) * 16 |> int64; 
                                        ((((rows |> int) - 1) / 16) + 1) * 16 |> int64 |], [| 16L; 16L |])                                    
                                                
            // Extract features
            let comp = <@              
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
                                ws) @>
            if not etOnly then
                let km = compiler.Compile(comp, opts) :?> IKernelModule
                let precomputedFeatures = features.BuildFinalizers(km)
                featureValues.Add(features.EvaluateFinalizers(km, precomputedFeatures, [ this; a; b; c; ws ]))

            for pid, _, platform in devices do   
                for did, _, _ in platform do    
                    let c = Array2D.zeroCreate (rows |> int) (cols |> int)
                        
                    let comp = <@ DEVICE(pid, did, 
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
                    // Run once to skip compilation time
                    if not featureOnly then    
                        runtimeRun(comp) |> ignore
                        if not (this.Verify(c, reference)) then
                            Console.WriteLine("---------------- COMPUTATION RESULT ERROR")
                        else          
                            // Run
                            let avg, stddev = FRTUtil.GetAvgAndStdDevCompletionTime(this.Iterations, 
                                                                                    fun () -> runtimeRun(comp) |> ignore)   
                            times.Add(avg |> float32)
                            times.Add(stddev |> float32)
                            System.Threading.Thread.Sleep(500)
                               
            executionResults.Add(times |> List.ofSeq)
                    
        (featureValues, executionResults) ||> Seq.zip |> List.ofSeq
        
[<FRTFeatureExtractionTrainingSample("MatrixMultTiled")>]
type MatrixMultAdvancedTrainingSample() =    
    inherit MatrixMultSimpleTrainingSample()
            
    [<ReflectedDefinition>]
    let BLOCK_SIZE = 16

    // Matrix multiplication
    [<ReflectedDefinition>]
    let MatMul(matA: float32[], matB: float32[], matC: float32[], matAWidth: int, matBWidth: int, wi: WorkItemInfo) =
        // Block index
        let bx = wi.GroupID(0);
        let by = wi.GroupID(1);
 
        // Thread index
        let tx = wi.LocalID(0);
        let ty = wi.LocalID(1);
 
        // Index of the first sub-matrix of A processed 
        // by the block
        let aBegin = matAWidth * BLOCK_SIZE * by;
 
        // Index of the last sub-matrix of A processed 
        // by the block
        let aEnd   = aBegin + matAWidth - 1;
 
        // Step size used to iterate through the 
        // sub-matrices of A
        let aStep  = BLOCK_SIZE;
 
        // Index of the first sub-matrix of B processed 
        // by the block
        let bBegin = BLOCK_SIZE * bx;
 
        // Step size used to iterate through the 
        // sub-matrices of B
        let bStep  = BLOCK_SIZE * matBWidth;
 
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
            As.[ty, tx] <- matA.[a + (matAWidth * ty) + tx]
            Bs.[ty, tx] <- matB.[b + (matBWidth * ty) + tx]
 
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
        let c = (matBWidth * BLOCK_SIZE * by) + (BLOCK_SIZE * bx)
        matC.[c + (matBWidth * ty) + tx] <- Csub
    
    override this.Run(features, devices, opts) =
        let rm = opts.["RunningMode"] :?> TrainingSampleRunningMode
        let runtimeRun = opts.["RuntimeRun"] :?> obj-> obj

        let featureOnly = rm = TrainingSampleRunningMode.OnlyFeatures
        let etOnly = rm = TrainingSampleRunningMode.OnlyExecutionTime
         
        let compiler = new Compiler()
        let opts = new Dictionary<string, obj>()        
        let rnd = System.Random()
        
        let rm = BufferReadMode.EnqueueReadBuffer
        let wm = BufferWriteMode.EnqueueWriteBuffer
        let ifl = MemoryFlags.ReadOnly ||| MemoryFlags.UseHostPointer
        let ofl = MemoryFlags.WriteOnly ||| MemoryFlags.UseHostPointer
        
        let executionResults = new List<float32 list>()
        let featureValues = new List<float32 list>()
                
        let sizes = (seq {
                            let s = ref this.MinMatrixSize
                            while !s <= this.MaxMatrixSize do
                                yield (!s, !s)
                                //yield (!s, !s * 2L)
                                s := !s * 2L
                        }) |> Array.ofSeq

        for rows, cols in sizes do
            let times = List<float32>()
            Console.WriteLine("      Size: " + String.Format("{0,10:##########}", rows))
                                          
            let a = Array.init (rows * cols |> int) (fun i -> rnd.Next() % 10 |> float32)
            let b = Array.init (cols * cols |> int) (fun i -> rnd.Next() % 10 |> float32)
            let reference = 
                if not featureOnly then
                    Array.zeroCreate<float32> 1 
                    ///this.CreateVerifiedOutput((a, b, rows |> int)) :?> float32[]
                else
                    Array.zeroCreate<float32> 1 
                   
            // Extract features                
            let c = Array.zeroCreate (rows * cols |> int)
            let ws = new WorkSize([| (((cols - 1L) / (BLOCK_SIZE |> int64)) + 1L) * (BLOCK_SIZE |> int64);
                                        (((rows - 1L) / (BLOCK_SIZE |> int64)) + 1L) * (BLOCK_SIZE |> int64) |], [| BLOCK_SIZE |> int64; BLOCK_SIZE |> int64 |])    
                                        
            let comp = <@   MatMul(
                                BUFFER_READ_MODE(rm, 
                                    MEMORY_FLAGS(ifl, 
                                        a)),
                                BUFFER_READ_MODE(rm, 
                                    MEMORY_FLAGS(ifl, 
                                        b)),
                                BUFFER_WRITE_MODE(wm, 
                                    MEMORY_FLAGS(ofl, 
                                        c)),
                                cols |> int,
                                rows |> int,
                                ws) @>
                                    
            if not etOnly then
                let km = compiler.Compile(comp, opts) :?> IKernelModule
                let precomputedFeatures = features.BuildFinalizers(km)
                featureValues.Add(features.EvaluateFinalizers(km, precomputedFeatures, [ this; a; b; c; cols|> int; rows |> int; ws ]))
                
            // Get completion times 
            for pid, _, platform in devices do   
                for did, _, _ in platform do 
                    let c = Array.zeroCreate (rows * cols |> int)
                        
                    let comp = <@ DEVICE(pid, did, 
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
                                            cols |> int,
                                            rows |> int,
                                            ws)) @>

                    // Run once to skip compilation time
                    if not featureOnly then    
                        runtimeRun(comp) |> ignore
                        if not true then //(((this.Verify(c, reference)) then
                            Console.WriteLine("---------------- COMPUTATION RESULT ERROR")
                        else                        
                            // Run
                            let avg, stddev = FRTUtil.GetAvgAndStdDevCompletionTime(this.Iterations, 
                                                                                    fun () -> runtimeRun(comp) |> ignore)   
                            times.Add(avg |> float32)
                            times.Add(stddev |> float32)
                            System.Threading.Thread.Sleep(500)

            executionResults.Add(times |> List.ofSeq)
   
        (featureValues, executionResults) ||> Seq.zip |> List.ofSeq
         