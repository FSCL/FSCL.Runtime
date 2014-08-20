module WavefrontBenchmark

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
open FSCL.Runtime
open System.Diagnostics

type WavefrontBenchmark() = 
    [<ReflectedDefinition>]
    static member private WavefrontTest(i:float32[], o:float32[], maxIfIndex: int, tripCount: int, wi:WorkItemInfo) =
        let mutable accum = 0.0f
        if wi.LocalID(0) < maxIfIndex then
            for idx = 0 to tripCount - 1 do   
                accum <- accum + i.[wi.GlobalID(0)]
                wi.Barrier(CLK_LOCAL_MEM_FENCE)
        else
            for idx = 0 to tripCount - 1 do   
                accum <- accum - i.[wi.GlobalID(0)] 
                wi.Barrier(CLK_LOCAL_MEM_FENCE)        
        o.[wi.GlobalID(0)] <- accum

    member this.Execute(pIndex: int, dIndex: int) =
        // Create wavefront test
        let i = Array.create (4 <<< 20) 2.0f
        let o = Array.zeroCreate<float32> (4 <<< 20)

        // At first, run non-divergente test
        let ws = new WorkSize(i.LongLength, 512L)
        let comp = <@ DEVICE(pIndex, dIndex, 
                             WavefrontBenchmark.WavefrontTest(i, o, 0, (4 <<< 10), ws)) @>
        comp.Run()

        // Ensured compilation overhead excluded, start test
        let timer = new Stopwatch()
        timer.Start()
        for i = 0 to 1 do
            comp.Run()
        timer.Stop()
        let referenceTime = timer.ElapsedMilliseconds
        Console.WriteLine(referenceTime.ToString() + " ms")

        // Now start from 1 and increase maxIfIndex until the time is close enought to referenceTime
        for j = 1 to 64 do
            let mutable currentWavefrontSize = 1
            let comp = <@ DEVICE(pIndex, dIndex, 
                                 WavefrontBenchmark.WavefrontTest(i, o, j, (4 <<< 10), ws)) @>
            let timer = new Stopwatch()
            timer.Start()
            for i = 0 to 1 do
                comp.Run()
            timer.Stop()
            let currentTime = timer.ElapsedMilliseconds
            Console.WriteLine(j.ToString() + ": " + currentTime.ToString() + " ms")
        

                        
