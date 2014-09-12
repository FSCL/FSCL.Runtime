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

// Matrix rows sum
[<ReflectedDefinition>]
let SumCols(matA: float32[,], c: float32[], wi: WorkItemInfo) =
    let r = wi.GlobalID(0)
    let mutable accum = 0.0f
    for i = 0 to matA.GetLength(1) - 1 do
        accum <- accum + matA.[r, i]
    c.[r] <- accum

// Matrix cols sum
[<ReflectedDefinition>]
let SumRows(matA: float32[,], c: float32[], wi: WorkItemInfo) =
    let col = wi.GlobalID(0)
    let mutable accum = 0.0f
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
            ids.Add("Matrix Rows (elements)")
            ids.Add("Matrix Cols (elements)")
            ids |> List.ofSeq
    
    override this.RunInternal(chain, conf, featureOnly: bool) = 
        let configuration = IDefaultFeatureExtractionTrainingSample.ConfigurationToDictionary(conf)
        let minSize = Int64.Parse(configuration.["MinMatrixSize"])
        let maxSize = Int64.Parse(configuration.["MaxMatrixSize"])
        let iterations = Int32.Parse(configuration.["Iterations"])

        let compiler = new Compiler()
        let opts = new Dictionary<string, obj>()        
        let rnd = System.Random()

        let rm = BufferReadMode.EnqueueReadBuffer
        let wm = BufferWriteMode.EnqueueWriteBuffer
        let ifl = MemoryFlags.ReadOnly
        let ofl = MemoryFlags.WriteOnly

        let mutable execResults: obj list list = []
                
        let sizes = (seq {
                            let s = ref minSize
                            while !s <= maxSize do
                                yield (!s, !s)
                                //yield (!s, !s * 2L)
                                s := !s + 64L
                        }) |> Array.ofSeq

        for rows, cols in sizes do
            Console.WriteLine("      Size: " + String.Format("{0,5:#####}", rows) + "x" + String.Format("{0,5:#####}", cols))
                                          
            let a = Array2D.init (rows |> int) (cols |> int) (fun r c -> r |> float32)
            let reference = 
                if not featureOnly then
                    this.CreateVerifiedOutput(a) :?> float32[]
                else
                    [||]

            let mutable features: obj list = []
            let mutable instanceResult: obj list = []

            for pIndex, pName, pDevs in GetOpenCLPlatforms() do   
                for dIndex, dName, dType in pDevs do
                    if dIndex > -1 then
                        let c = Array.zeroCreate (rows |> int)
                        let ws = new WorkSize(rows, Math.Min(rows, 64L))                                    
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
                        let km = compiler.Compile(comp, opts) :?> IKernelModule
                        let precomputedFeatures = chain.Precompute(km)
                        features <- chain.Evaluate(km, precomputedFeatures, [ a; c; ws ], opts)

                        // Run once to skip compilation time
                        if not featureOnly then    
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
                        else
                            instanceResult <- instanceResult @ [ 0.0f ] 
                               
            execResults <- execResults @ [ instanceResult @ [rows; cols] @ features ]       
        execResults


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

    override this.RunInternal(chain, conf, featureOnly: bool) = 
        let configuration = IDefaultFeatureExtractionTrainingSample.ConfigurationToDictionary(conf)
        let minSize = Int64.Parse(configuration.["MinMatrixSize"])
        let maxSize = Int64.Parse(configuration.["MaxMatrixSize"])
        let iterations = Int32.Parse(configuration.["Iterations"])

        let compiler = new Compiler()
        let opts = new Dictionary<string, obj>()        
        let rnd = System.Random()

        let rm = BufferReadMode.EnqueueReadBuffer
        let wm = BufferWriteMode.EnqueueWriteBuffer
        let ifl = MemoryFlags.ReadOnly
        let ofl = MemoryFlags.WriteOnly

        let mutable execResults: obj list list = []
                
        let sizes = (seq {
                            let s = ref minSize
                            while !s <= maxSize do
                                yield (!s, !s)
                                //yield (!s, !s * 2L)
                                s := !s + 64L
                        }) |> Array.ofSeq

        for rows, cols in sizes do
            Console.WriteLine("      Size: " + String.Format("{0,5:#####}", rows) + "x" + String.Format("{0,5:#####}", cols))
                                          
            let a = Array2D.init (rows |> int) (cols |> int) (fun r c -> r |> float32)
            let reference = 
                if not featureOnly then
                    this.CreateVerifiedOutput(a) :?> float32[]
                else
                    [||]

            let mutable features: obj list = []
            let mutable instanceResult: obj list = []

            for pIndex, pName, pDevs in GetOpenCLPlatforms() do   
                for dIndex, dName, dType in pDevs do
                    if dIndex > 0 then
                        let c = Array.zeroCreate (rows |> int)
                        let ws = new WorkSize(cols, Math.Min(cols, 64L))                                    
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
                        let km = compiler.Compile(comp, opts) :?> IKernelModule
                        let precomputedFeatures = chain.Precompute(km)
                        features <- chain.Evaluate(km, precomputedFeatures, [ a; c; ws ], opts)

                        // Run once to skip compilation time
                        if not featureOnly then    
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
                        else
                            instanceResult <- instanceResult @ [ 0.0f ] 
                               
            execResults <- execResults @ [ instanceResult @ [rows; cols] @ features ]       
        execResults