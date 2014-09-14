module VectorAddTrainingSample

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
let sum(a, b) =
    a + b
[<ReflectedDefinition>]
let VectorAdd(a: float32[], b: float32[], c: float32[], wi: WorkItemInfo) =
    let gid = wi.GlobalID(0)
    c.[gid] <- a.[gid] + b.[gid]

type VectorAddTrainingSample() =    
    inherit IDefaultFeatureExtractionTrainingSample()

    override this.DefaultConfigurationDictionary() =
        let dict = new Dictionary<string, obj>()
        dict.Add("MinVectorSize", 2048L)
        dict.Add("MaxVectorSize", 8L <<< 20)
        dict.Add("Iterations", 100)
        dict
        
    override this.Verify(output: obj, reference: obj) =
        let o = output :?> float32[]
        let r = reference :?> float32[]
        let mutable i = 0
        let mutable eq = true
        while eq && i < o.Length do
            if o.[i] <> r.[i] then  
                eq <- false
            else
                i <- i + 1
        eq
    
    override this.CreateVerifiedOutput(o: obj) =
        let a, b = o :?> float32[] * float32[]
        Array.map2 (fun a b -> a + b) a b |> box

    override this.ResultColumnIDs 
        with get() =   
            let ids = new List<String>()         
            for pIndex, pName, pDevs in GetOpenCLPlatforms() do  
                for dIndex, dName, dType in pDevs do  
                    ids.Add(dName + " Completion Time (ms)")
            ids.Add("Vector Size (elements)")
            ids |> List.ofSeq
                
    override this.RunInternal(chain, conf, featureOnly: bool) = 
        let configuration = IDefaultFeatureExtractionTrainingSample.ConfigurationToDictionary(conf)
        let minSize = Int64.Parse(configuration.["MinVectorSize"])
        let maxSize = Int64.Parse(configuration.["MaxVectorSize"])
        let iterations = Int32.Parse(configuration.["Iterations"])

        let compiler = new Compiler()
        let opts = new Dictionary<string, obj>()        
        let rnd = System.Random()
        
        let rm = BufferReadMode.EnqueueReadBuffer
        let wm = BufferWriteMode.EnqueueWriteBuffer
        let ifl = MemoryFlags.UseHostPointer ||| MemoryFlags.ReadOnly
        let ofl = MemoryFlags.UseHostPointer ||| MemoryFlags.WriteOnly

        let mutable execResults: obj list list = []
                
        let size = ref minSize
        while !size <= maxSize do
            Console.WriteLine("      Size: " + String.Format("{0,10:##########}", !size))
                        
            let a = Array.zeroCreate<float32> (!size |> int)
            let b = Array.zeroCreate<float32> (!size |> int)
            for i = 0 to (!size |> int) - 1 do
                a.[i] <- (float32)i
                b.[i] <- (float32)((!size |> int) - i)
            let reference = 
                if not featureOnly then
                    this.CreateVerifiedOutput((a, b)) :?> float32[]
                else
                    Array.zeroCreate<float32> 1

            let mutable features: obj list = []
            let mutable instanceResult: obj list = []
            for pIndex, pName, pDevs in GetOpenCLPlatforms() do   
                for dIndex, dName, dType in pDevs do                                
                    Console.WriteLine(" Device " + ": " + dName.ToString() + "(" + dType.ToString() + ")")                                    
                    let c = Array.zeroCreate<float32> (!size |> int)
                    let ws = WorkSize(!size, 128L)
                    let comp = <@ DEVICE(pIndex, dIndex,
                                    VectorAdd(
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
                            instanceResult <- instanceResult @ [ ttime ]
                            System.Threading.Thread.Sleep(500)
                    else
                      instanceResult <- instanceResult @ [ 0.0f ]      
                                
            execResults <- execResults @ [ instanceResult @ [!size] @ features ]                
            size := !size + minSize   
        execResults