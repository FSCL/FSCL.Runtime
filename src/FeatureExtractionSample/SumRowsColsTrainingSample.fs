module SumRowsColsTrainingSample

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
open System.Linq

// Matrix rows sum
[<ReflectedDefinition>]
let SumCols(matA: float32[,], c: float32[], wi: WorkItemInfo) =
    let r = wi.GlobalID(0)
    let mutable accum = 0.0f
    if r < matA.GetLength(0) then
        for i = 0 to matA.GetLength(1) - 1 do
            accum <- accum + matA.[r, i]
        c.[r] <- accum

// Matrix cols sum
[<ReflectedDefinition>]
let SumRows(matA: float32[,], c: float32[], wi: WorkItemInfo) =
    let col = wi.GlobalID(0)
    let mutable accum = 0.0f
    if col < matA.GetLength(1) then
        for i = 0 to matA.GetLength(0) - 1 do
            accum <- accum + matA.[i, col]
        c.[col] <- accum

type SumColsTrainingSample() =    
    inherit IDefaultFeatureExtractionTrainingSample()

    override this.DefaultConfigurationDictionary() =
        let dict = new Dictionary<string, obj>()
        dict.Add("MinMatrixSize", 64L)
        dict.Add("MaxMatrixSize", 2048L)
        dict.Add("Iterations", 100)
        dict
        
    override this.Verify(output: obj, reference: obj) =
        (output :?> float32[]) = (reference :?> float32[])
    
    override this.CreateVerifiedOutput(o: obj) =
        let a = o :?> float32[,]
        let res = Array.zeroCreate<float32> (a.GetLength(0))
        for row = 0 to a.GetLength(0) - 1  do
            let mutable v = 0.0f
            for col = 0 to a.GetLength(1) - 1 do
                v <- v + a.[row, col]
            res.[row] <- v
        box res

    override this.ResultColumnIDs 
        with get() =   
            let ids = new List<String>()         
            for pIndex, pName, pDevs in GetOpenCLPlatforms() do  
                for dIndex, dName, dType in pDevs do  
                    ids.Add(dName + " Completion Time (ms)")
            //ids.Add("Matrix Rows (elements)")
            //ids.Add("Matrix Cols (elements)")
            ids |> List.ofSeq
    
    override this.RunInternal(chain, conf, rm: TrainingSampleRunningMode) = 
        let featureOnly = rm = TrainingSampleRunningMode.OnlyFeatures
        let etOnly = rm = TrainingSampleRunningMode.OnlyExecutionTime

        let configuration = IDefaultFeatureExtractionTrainingSample.ConfigurationToDictionary(conf)
        let minSize = Int64.Parse(configuration.["MinMatrixSize"])
        let maxSize = Int64.Parse(configuration.["MaxMatrixSize"])
        let iterations = Int32.Parse(configuration.["Iterations"])

        let compiler = new Compiler()
        let opts = new Dictionary<string, obj>()        
        let rnd = System.Random()

        let rm = BufferReadMode.EnqueueReadBuffer
        let wm = BufferWriteMode.EnqueueWriteBuffer
        let ifl = MemoryFlags.ReadOnly //  ||| MemoryFlags.UseHostPointer
        let ofl = MemoryFlags.WriteOnly //  ||| MemoryFlags.UseHostPointer

        let sizes = (seq {
                            let s = ref minSize
                            while !s <= maxSize do
                                yield (!s, !s)
                                yield (!s + 1L, !s + 1L)
                                //yield (!s, !s * 2L)
                                s := !s + minSize
                        }) |> Array.ofSeq

        let executionResults = new List<List<obj>>()
        let featureValues = new List<List<obj>>()

        for rows, cols in sizes do
            executionResults.Add(new List<obj>())
            Console.WriteLine("      Size: " + String.Format("{0,5:#####}", rows) + "x" + String.Format("{0,5:#####}", cols))
                                          
            let a = Array2D.init (rows |> int) (cols |> int) (fun r c -> r |> float32)
            let reference = 
                if not featureOnly then
                    this.CreateVerifiedOutput(a) :?> float32[]
                else
                    [||]
                    
            for pIndex, pName, pDevs in GetOpenCLPlatforms() do   
                for dIndex, dName, dType in pDevs do
                    let c = Array.zeroCreate (rows |> int)
                    let ws = new WorkSize((((rows - 1L) / 64L) + 1L) * 64L, 64L)                                    
                    Console.WriteLine(" Device " + ": " + dName.ToString() + "(" + dType.ToString() + ")")  
                    let comp = <@ DEVICE(pIndex, dIndex,
                                    SumCols(
                                        BUFFER_READ_MODE(rm, 
                                            MEMORY_FLAGS(ifl, 
                                                a)),
                                        BUFFER_WRITE_MODE(wm, 
                                            MEMORY_FLAGS(ofl, 
                                                c)),
                                        ws)) @>
                    // Extract features
                    if (pIndex = 0 && dIndex = 0 && not etOnly) then
                        let km = compiler.Compile(comp, opts) :?> IKernelModule
                        let precomputedFeatures = chain.Precompute(km)
                        featureValues.Add(new List<obj>(chain.Evaluate(km, precomputedFeatures, [ a; c; ws ], opts)))
                        
                    // Run once to skip compilation time
                    if not featureOnly then    
                        comp.Run()
                        if not (this.Verify(c, reference)) then
                            Console.WriteLine("---------------- COMPUTATION RESULT ERROR")
                        else                        
                            // Run
                            let watch = new Stopwatch()
                            let data = Array.zeroCreate<double> iterations
                            for i = 0 to iterations - 1 do   
                                watch.Restart()                   
                                comp.Run()
                                watch.Stop()
                                data.[i] <- (double)watch.ElapsedMilliseconds 
                            let avg = data |> Array.average
                            let stddev  = Math.Sqrt(data |> Array.map(fun d -> Math.Pow(d - avg, 2.0)) |> Array.reduce(+) |> (fun a -> a/(double)iterations))  
                                                                                          
                            // Dump
                            Console.WriteLine("---------------- " + String.Format("{0,11:######0.0000}", avg) + "ms (" + String.Format("{0,10:#########0}", iterations) + " iterations)")
                            executionResults.Last().Add(avg)
                            executionResults.Last().Add(stddev)
                            System.Threading.Thread.Sleep(500)
                               
            //executionResults.Last().AddRange([rows; cols])       
        executionResults, featureValues


type SumRowsTrainingSample() =    
    inherit SumColsTrainingSample()

    override this.CreateVerifiedOutput(o: obj) =
        let a = o :?> float32[,]
        let res = Array.zeroCreate<float32> (a.GetLength(1))
        for col = 0 to a.GetLength(1) - 1 do
            let mutable v = 0.0f
            for row = 0 to a.GetLength(0) - 1  do
                v <- v + a.[row, col]
            res.[col] <- v
        box res

    override this.RunInternal(chain, conf, rm: TrainingSampleRunningMode) = 
        let featureOnly = rm = TrainingSampleRunningMode.OnlyFeatures
        let etOnly = rm = TrainingSampleRunningMode.OnlyExecutionTime

        let configuration = IDefaultFeatureExtractionTrainingSample.ConfigurationToDictionary(conf)
        let minSize = Int64.Parse(configuration.["MinMatrixSize"])
        let maxSize = Int64.Parse(configuration.["MaxMatrixSize"])
        let iterations = Int32.Parse(configuration.["Iterations"])

        let compiler = new Compiler()
        let opts = new Dictionary<string, obj>()        
        let rnd = System.Random()

        let rm = BufferReadMode.EnqueueReadBuffer
        let wm = BufferWriteMode.EnqueueWriteBuffer
        let ifl = MemoryFlags.ReadOnly //  ||| MemoryFlags.UseHostPointer
        let ofl = MemoryFlags.WriteOnly //  ||| MemoryFlags.UseHostPointer

        let mutable execResults: obj list list = []
                
        let executionResults = new List<List<obj>>()
        let featureValues = new List<List<obj>>()

        let sizes = (seq {
                            let s = ref minSize
                            while !s <= maxSize do
                                yield (!s, !s)
                                yield (!s + 1L, !s + 1L)
                                s := !s + minSize
                        }) |> Array.ofSeq

        for rows, cols in sizes do
            executionResults.Add(new List<obj>())
            Console.WriteLine("      Size: " + String.Format("{0,5:#####}", rows) + "x" + String.Format("{0,5:#####}", cols))
                                          
            let a = Array2D.init (rows |> int) (cols |> int) (fun r c -> r |> float32)
            let reference = 
                if not featureOnly then
                    this.CreateVerifiedOutput(a) :?> float32[]
                else
                    [||]
                    
            for pIndex, pName, pDevs in GetOpenCLPlatforms() do   
                for dIndex, dName, dType in pDevs do
                    let c = Array.zeroCreate (rows |> int)
                    let ws = new WorkSize((((cols - 1L) / 64L) + 1L) * 64L, 64L)                                   
                    Console.WriteLine(" Device " + ": " + dName.ToString() + "(" + dType.ToString() + ")")  
                    let comp = <@ DEVICE(pIndex, dIndex,
                                    SumRows(
                                        BUFFER_READ_MODE(rm, 
                                            MEMORY_FLAGS(ifl, 
                                                a)),
                                        BUFFER_WRITE_MODE(wm, 
                                            MEMORY_FLAGS(ofl, 
                                                c)),
                                        ws)) @>
                    // Extract features
                    if (pIndex = 0 && dIndex = 0 && not etOnly) then
                        let km = compiler.Compile(comp, opts) :?> IKernelModule
                        let precomputedFeatures = chain.Precompute(km)
                        featureValues.Add(new List<obj>(chain.Evaluate(km, precomputedFeatures, [ a; c; ws ], opts)))

                    // Run once to skip compilation time
                    if not featureOnly then    
                        comp.Run()
                        if not (this.Verify(c, reference)) then
                            Console.WriteLine("---------------- COMPUTATION RESULT ERROR")
                        else      
                            // Run
                            let watch = new Stopwatch()
                            let data = Array.zeroCreate<double> iterations
                            for i = 0 to iterations - 1 do   
                                watch.Restart()                   
                                comp.Run()
                                watch.Stop()
                                data.[i] <- (double)watch.ElapsedMilliseconds 
                            let avg = data |> Array.average
                            let stddev  = Math.Sqrt(data |> Array.map(fun d -> Math.Pow(d - avg, 2.0)) |> Array.reduce(+) |> (fun a -> a/(double)iterations))  
                                                                                          
                            // Dump
                            Console.WriteLine("---------------- " + String.Format("{0,11:######0.0000}", avg) + "ms (" + String.Format("{0,10:#########0}", iterations) + " iterations)")
                            executionResults.Last().Add(avg)
                            executionResults.Last().Add(stddev)

                            System.Threading.Thread.Sleep(500)
                         
            //executionResults.Last().AddRange([rows; cols])       
        executionResults, featureValues