namespace FSCL.Runtime.Scheduling.FeatureExtraction.Benchmarks

open FSCL
open FSCL.Compiler
open FSCL.Runtime.Scheduling
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open System.Reflection
open Microsoft.FSharp.Linq.RuntimeHelpers
open System
open FSCL.Compiler.Util
open FSCL.Compiler.FunctionPostprocessing
open FSCL.Language
open System.Runtime.InteropServices
open FSCL.Runtime.Scheduling.ReflectionUtil
open FSCL.Runtime
open System.Diagnostics

type WavefrontBenchmark() = 
    [<ReflectedDefinition>]
    static member private WavefrontTest(i:float32[], o:float32[], maxIfIndex: int, tripCount: int, wi:WorkItemInfo) =
        let mutable accum = 0.0f
        if wi.LocalID(0) < maxIfIndex then
            for idx = 0 to tripCount - 1 do   
                accum <- accum + i.[0]    
        else
            for idx = 0 to tripCount - 1 do   
                accum <- accum - i.[0]                
        o.[wi.GlobalID(0)] <- accum

    member this.Execute(pIndex: int, dIndex: int) =
        // Create wavefront test
        let i = Array.create (4 <<< 20) 2.0f
        let o = Array.zeroCreate<float32> (4 <<< 20)

        // At first, run non-divergente test
        let ws = new WorkSize(i.LongLength, i.LongLength)
        let comp = <@ DEVICE(pIndex, dIndex, 
                             WavefrontBenchmark.WavefrontTest(i, o, 0, (16 <<< 20), ws)) @>
        comp.R
        let timer = new Stopwatch()
        timer.Start()
        for i = 0 to 99 do
            comp.Run
        

                        
