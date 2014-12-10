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

[<FRTFeatureExtractionTrainingSample("VectorAdd")>]
type VectorAddTrainingSample() =    
    inherit FRTFeatureExtractionTrainingSample()
    
    [<ReflectedDefinition>]
    let VectorAdd(a: float32[], b: float32[], c: float32[], wi: WorkItemInfo) =
        let gid = wi.GlobalID(0)
        c.[gid] <- a.[gid] + b.[gid]
        c
        
    [<ConfigurationItem>]
    member val MinVectorSize = 4096L with get, set    
    [<ConfigurationItem>]
    member val MaxVectorSize = 16L <<< 20 with get, set   
    [<ConfigurationItem>]
    member val Iterations = 30 with get, set
        
    member this.Verify(o: float32[], r: float32[]) =
        o = r
    
    member this.CreateVerifiedOutput(a: float32[], b: float32[]) =
        Array.map2 (fun a b -> a + b) a b
                            
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
                            let s = ref this.MinVectorSize
                            while !s <= this.MaxVectorSize do
                                yield !s
                                s := !s * 2L
                        }) |> Array.ofSeq

        for size in sizes do
            Console.WriteLine("Size: " + size.ToString())
            let times = List<float32>()
                        
            let a = Array.zeroCreate<float32> (size |> int)
            let b = Array.zeroCreate<float32> (size |> int)
            for i = 0 to (size |> int) - 1 do
                a.[i] <- (float32)i
                b.[i] <- (float32)((size |> int) - i)
            let reference = 
                if not featureOnly then
                    this.CreateVerifiedOutput(a, b)
                else
                    Array.zeroCreate<float32> 1
                        
            let c = Array.zeroCreate<float32> (size |> int)
            let ws = WorkSize(size, 128L)

            // Extract features
            let comp = <@   VectorAdd(
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
                        
            // Extract features
            if not etOnly then
                let km = compiler.Compile(comp, opts) :?> IKernelModule
                let precomputedFeatures = features.BuildFinalizers(km)
                featureValues.Add(features.EvaluateFinalizers(km, precomputedFeatures, [ this; a; b; c; ws ]))

            // Get completion times
            for pid, _, platform in devices do   
                for did, _, _ in platform do                                 
                                                 
                    let c = Array.zeroCreate<float32> (size |> int)  
                    let comp = <@  DEVICE(pid, did,
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
