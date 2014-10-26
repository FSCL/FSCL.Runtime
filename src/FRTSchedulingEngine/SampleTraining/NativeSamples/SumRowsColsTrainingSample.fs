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

[<FRTFeatureExtractionTrainingSample("SumCols")>]
type SumColsTrainingSample() =    
    inherit FRTFeatureExtractionTrainingSample()
    
    // Matrix rows sum
    [<ReflectedDefinition>]
    let SumCols(matA: float32[,], c: float32[], wi: WorkItemInfo) =
        let r = wi.GlobalID(0)
        let mutable accum = 0.0f
        if r < matA.GetLength(0) then
            for i = 0 to matA.GetLength(1) - 1 do
                accum <- accum + matA.[r, i]
            c.[r] <- accum
            
    [<ConfigurationItem>]
    member val MinMatrixSize = 64L with get, set    
    [<ConfigurationItem>]
    member val MaxMatrixSize = 2048L with get, set   
    [<ConfigurationItem>]
    member val Iterations = 100 with get, set
        
    member this.Verify(output: float32[], reference: float32[]) =
        output = reference
    
    member this.CreateVerifiedOutput(a: float32[,]) =
        let res = Array.zeroCreate<float32> (a.GetLength(0))
        for row = 0 to a.GetLength(0) - 1  do
            let mutable v = 0.0f
            for col = 0 to a.GetLength(1) - 1 do
                v <- v + a.[row, col]
            res.[row] <- v
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

        let sizes = (seq {
                            let s = ref this.MinMatrixSize
                            while !s <= this.MaxMatrixSize do
                                yield (!s, !s)
                                yield (!s + 1L, !s + 1L)
                                //yield (!s, !s * 2L)
                                s := !s + this.MinMatrixSize
                        }) |> Array.ofSeq

        let executionResults = new List<float32[]>()
        let featureValues = new List<float32[]>()

        for rows, cols in sizes do
            let times = List<float32>()
                                          
            let a = Array2D.init (rows |> int) (cols |> int) (fun r c -> r |> float32)
            let reference = 
                if not featureOnly then
                    this.CreateVerifiedOutput(a) 
                else
                    [||]
            let c = Array.zeroCreate (rows |> int)
            let ws = new WorkSize((((rows - 1L) / 64L) + 1L) * 64L, 64L)  
                    
            let comp = <@   SumCols(
                                BUFFER_READ_MODE(rm, 
                                    MEMORY_FLAGS(ifl, 
                                        a)),
                                BUFFER_WRITE_MODE(wm, 
                                    MEMORY_FLAGS(ofl, 
                                        c)),
                                ws) @>
            // Extract features
            if not etOnly then
                let km = compiler.Compile(comp, opts) :?> IKernelModule
                let precomputedFeatures = features.BuildFinalizers(km)
                featureValues.Add(features.EvaluateFinalizers(km, precomputedFeatures, [ a; c; ws ]))

            for pid, _, platform in devices do   
                for did, _, _ in platform do 
                    let c = Array.zeroCreate (rows |> int)                          

                    let comp = <@ DEVICE(pid, did, 
                                         SumCols(
                                            BUFFER_READ_MODE(rm, 
                                                MEMORY_FLAGS(ifl, 
                                                    a)),
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

            executionResults.Add(times |> Array.ofSeq)
                                  
        (featureValues, executionResults) ||> Seq.zip |> Array.ofSeq
                
[<FRTFeatureExtractionTrainingSample("SumRows")>]
type SumRowsTrainingSample() =    
    inherit SumColsTrainingSample()
    
    // Matrix cols sum
    [<ReflectedDefinition>]
    let SumRows(matA: float32[,], c: float32[], wi: WorkItemInfo) =
        let col = wi.GlobalID(0)
        let mutable accum = 0.0f
        if col < matA.GetLength(1) then
            for i = 0 to matA.GetLength(0) - 1 do
                accum <- accum + matA.[i, col]
            c.[col] <- accum

    member this.CreateVerifiedOutput(a: float32[,]) =
        let res = Array.zeroCreate<float32> (a.GetLength(1))
        for col = 0 to a.GetLength(1) - 1 do
            let mutable v = 0.0f
            for row = 0 to a.GetLength(0) - 1  do
                v <- v + a.[row, col]
            res.[col] <- v
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

        let mutable execResults: obj list list = []
                
        let executionResults = new List<float32[]>()
        let featureValues = new List<float32[]>()

        let sizes = (seq {
                            let s = ref this.MinMatrixSize
                            while !s <= this.MaxMatrixSize do
                                yield (!s, !s)
                                yield (!s + 1L, !s + 1L)
                                s := !s + this.MinMatrixSize
                        }) |> Array.ofSeq

        for rows, cols in sizes do
            let times = List<float32>()
            Console.WriteLine("      Size: " + String.Format("{0,5:#####}", rows) + "x" + String.Format("{0,5:#####}", cols))
                                          
            let a = Array2D.init (rows |> int) (cols |> int) (fun r c -> r |> float32)
            let reference = 
                if not featureOnly then
                    this.CreateVerifiedOutput(a) 
                else
                    [||]
                    
            // Extract features
            let c = Array.zeroCreate (rows |> int)
            let ws = new WorkSize((((cols - 1L) / 64L) + 1L) * 64L, 64L)                            

            let comp = <@   SumRows(
                                BUFFER_READ_MODE(rm, 
                                    MEMORY_FLAGS(ifl, 
                                        a)),
                                BUFFER_WRITE_MODE(wm, 
                                    MEMORY_FLAGS(ofl, 
                                        c)),
                                ws) @>
            // Extract features
            if not etOnly then
                let km = compiler.Compile(comp, opts) :?> IKernelModule
                let precomputedFeatures = features.BuildFinalizers(km)
                featureValues.Add(features.EvaluateFinalizers(km, precomputedFeatures, [ a; c; ws ]))

            // Get completion times
            for pip, _, platform in devices do   
                for did, _, _ in platform do 
                    let c = Array.zeroCreate (rows |> int)                          

                    let comp = <@ DEVICE(pip, did, 
                                         SumRows(
                                            BUFFER_READ_MODE(rm, 
                                                MEMORY_FLAGS(ifl, 
                                                    a)),
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
                         
            executionResults.Add(times |> Array.ofSeq)
        
        (featureValues, executionResults) ||> Seq.zip |> Array.ofSeq
