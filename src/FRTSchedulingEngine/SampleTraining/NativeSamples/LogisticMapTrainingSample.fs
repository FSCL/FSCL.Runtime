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

[<FRTFeatureExtractionTrainingSample("LogisticMap")>]
type LogisticMapTrainingSample() =    
    inherit FRTFeatureExtractionTrainingSample()
        
    [<ReflectedDefinition>]
    let LogisticMap(a: float32[], c: float32[], r: float32, it: int, wi: WorkItemInfo) =
        let gid = wi.GlobalID(0)
        let mutable result = a.[gid]
        for iteration = 1 to it do
            result <- r * result * (1.0f - result)
        c.[gid] <- result
                
    [<ConfigurationItem>]
    member val MinVectorSize = 64L with get, set    
    [<ConfigurationItem>]
    member val MaxVectorSize = 2048L with get, set    
    [<ConfigurationItem>]
    member val MinIterations = 1000 with get, set    
    [<ConfigurationItem>]
    member val MaxIterations = 10000 with get, set 
    [<ConfigurationItem>]
    member val Iterations = 100 with get, set

    member this.Verify(output: obj, reference: obj) =
        output = reference
    
    member this.CreateVerifiedOutput(o: obj) =
        let a, it, r = o :?> float32[] * int * float32
        a |> Array.map(fun item ->
                            let t = ref item
                            for i = 1 to it do
                                t := r * !t * (1.0f - !t)
                            !t) |> box
                
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
                
        let r = 0.422f;
            
        let sizes = (seq {
                            let s = ref this.MinVectorSize
                            while !s <= this.MaxVectorSize do
                                yield !s
                                s := !s + this.MinVectorSize
                        }) |> Array.ofSeq

        for size in sizes do
            for logIter in this.MinIterations .. this.MinIterations .. this.MaxIterations do
                
                let times = List<float32>()
                
                let a = Array.zeroCreate<float32> (size |> int)
                let c = Array.zeroCreate<float32> (size |> int)
                for i = 0 to (size |> int) - 1 do
                    a.[i] <- rnd.NextDouble() |> float32
                let reference = 
                        Array.zeroCreate<float32> 1                       
                let c = Array.zeroCreate<float32> (size |> int)
                let ws = WorkSize(size, 64L)
                        
                // Extract features
                let comp = <@   LogisticMap(
                                    BUFFER_READ_MODE(rm, 
                                        MEMORY_FLAGS(ifl, 
                                            a)),
                                    BUFFER_WRITE_MODE(wm, 
                                        MEMORY_FLAGS(ofl, 
                                            c)),
                                    r,
                                    logIter,
                                    ws) @> 

                if not etOnly then
                    let km = compiler.Compile(comp) :?> IKernelModule
                    let precomputedFeatures = features.BuildFinalizers(km)
                    featureValues.Add(features.EvaluateFinalizers(km, precomputedFeatures, [ this; a; c; r; logIter; ws ]))
     
                // Get completion times
                for pid, _, platform in devices do   
                    for did, _, _ in platform do                                
                        let c = Array.zeroCreate<float32> (size |> int)
                        let ws = WorkSize(size, 64L)

                        let comp = <@ DEVICE(pid, did, 
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

                        if not featureOnly then                                 
                            // Run once to skip compilation time
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