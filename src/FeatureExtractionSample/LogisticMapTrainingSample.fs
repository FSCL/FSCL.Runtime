module LogisticMapTrainingSample

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

[<ReflectedDefinition>]
let LogisticMap(a: float32[], c: float32[], r: float32, it: int, wi: WorkItemInfo) =
    let gid = wi.GlobalID(0)
    let mutable result = a.[gid]
    for iteration = 1 to it do
        result <- r * result * (1.0f - result)
    c.[gid] <- result

type LogisticMapTrainingSample() =    
    inherit IDefaultFeatureExtractionTrainingSample()

    override this.DefaultConfigurationDictionary() =
        let dict = new Dictionary<string, obj>()
        dict.Add("MinVectorSize", 256L <<< 10)
        dict.Add("MaxVectorSize", 8L <<< 20)
        dict.Add("MinIterations", 10)
        dict.Add("MaxIterations", 100)
        dict.Add("Iterations", 100)
        dict
        
    override this.Verify(output: obj, reference: obj) =
        true
    
    override this.CreateVerifiedOutput(o: obj) =
        let a, it, r = o :?> float32[] * int * float32
        a |> Array.map(fun item ->
                            let t = ref item
                            for i = 1 to it do
                                t := r * !t * (1.0f - !t)
                            !t) |> box

    override this.ResultColumnIDs 
        with get() =   
            let ids = new List<String>()         
            for pIndex, pName, pDevs in GetOpenCLPlatforms() do  
                for dIndex, dName, dType in pDevs do  
                    ids.Add(dName + " Completion Time (ms)")
            //ids.Add("Vector Size (elements)")
            ids |> List.ofSeq
                
    override this.RunInternal(chain, conf, rm: TrainingSampleRunningMode) = 
        let featureOnly = rm = TrainingSampleRunningMode.OnlyFeatures
        let etOnly = rm = TrainingSampleRunningMode.OnlyExecutionTime

        let configuration = IDefaultFeatureExtractionTrainingSample.ConfigurationToDictionary(conf)
        let minSize = Int64.Parse(configuration.["MinVectorSize"])
        let maxSize = Int64.Parse(configuration.["MaxVectorSize"])
        let minLogisticIterations = Int32.Parse(configuration.["MinIterations"])
        let maxLogisticIterations = Int32.Parse(configuration.["MaxIterations"])
        let iterations = Int32.Parse(configuration.["Iterations"])

        let compiler = new Compiler()
        let opts = new Dictionary<string, obj>()        
        let rnd = System.Random()
        
        let rm = BufferReadMode.EnqueueReadBuffer
        let wm = BufferWriteMode.EnqueueWriteBuffer
        let ifl = MemoryFlags.ReadOnly
        let ofl = MemoryFlags.WriteOnly
        
        let executionResults = new List<List<obj>>()
        let featureValues = new List<List<obj>>()
                
        let r = 0.422f;

        let size = ref minSize
        while !size <= maxSize do
            executionResults.Add(new List<obj>())
            Console.WriteLine("      Size: " + String.Format("{0,10:##########}", !size))
            for logIter in minLogisticIterations .. 10 .. maxLogisticIterations do
                Console.WriteLine("      Iterations: " + String.Format("{0,10:##########}", logIter))
                
                let a = Array.zeroCreate<float32> (!size |> int)
                let c = Array.zeroCreate<float32> (!size |> int)
                for i = 0 to (!size |> int) - 1 do
                    a.[i] <- rnd.NextDouble() |> float32
                let reference = 
                    if not featureOnly then
                        this.CreateVerifiedOutput((a, logIter, r)) :?> float32[]
                    else
                        Array.zeroCreate<float32> 1

                for pIndex, pName, pDevs in GetOpenCLPlatforms() do   
                    for dIndex, dName, dType in pDevs do                                
                        Console.WriteLine(" Device " + ": " + dName.ToString() + "(" + dType.ToString() + ")")                                    
                        let c = Array.zeroCreate<float32> (!size |> int)
                        let ws = WorkSize(!size, 128L)
                        let comp = <@ DEVICE(pIndex, dIndex,
                                        LogisticMap(
                                            BUFFER_READ_MODE(rm, 
                                                MEMORY_FLAGS(ifl, 
                                                    a)),
                                            BUFFER_WRITE_MODE(wm, 
                                                MEMORY_FLAGS(ofl, 
                                                    c)),
                                            r,
                                            logIter,
                                            ws)) @>   
                        
                        // Extract features
                        if (dIndex = 0 && pIndex = 0 && not etOnly) then
                            let km = compiler.Compile(comp, opts) :?> IKernelModule
                            let precomputedFeatures = chain.Precompute(km)
                            featureValues.Add(new List<obj>(chain.Evaluate(km, precomputedFeatures, [ a; c; r; logIter; ws ], opts)))

                        if not featureOnly then                                 
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
                                    
                                // Dump
                                Console.WriteLine("---------------- " + String.Format("{0,11:######0.0000}", ttime) + "ms (" + String.Format("{0,10:#########0}", iters) + " iterations)")
                                executionResults.Last().Add(ttime)
                                System.Threading.Thread.Sleep(500)   
                                
            //executionResults.Last().Add(!size)
            size := !size + minSize   
        executionResults, featureValues