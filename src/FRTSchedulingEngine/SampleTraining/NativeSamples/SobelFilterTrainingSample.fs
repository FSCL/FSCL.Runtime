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

module Sobel =
    [<ReflectedDefinition>]
    let SobelFilter2DNoBorder(inputImage: uchar4[,],
                                outputImage: uchar4[,],
                                wi: WorkItemInfo) =
        let x = wi.GlobalID(0)
        let y = wi.GlobalID(1)

        let width = outputImage.GetLength(1)
        let height = outputImage.GetLength(0)

        let mutable Gx = float4(0.0f)
        let mutable Gy = Gx

        if x < width && y < height then
            // Read each texel component and calculate the filtered value using neighbouring texel components 
            let i00 = (inputImage.[y, x]).ToFloat4()
            let i10 = (inputImage.[y, x + 1]).ToFloat4()
            let i20 = (inputImage.[y, x + 2]).ToFloat4()
            let i01 = (inputImage.[y + 1, x]).ToFloat4()
            let i11 = (inputImage.[y + 1, x + 1]).ToFloat4()
            let i21 = (inputImage.[y + 1, x + 2]).ToFloat4()
            let i02 = (inputImage.[y + 2, x]).ToFloat4()
            let i12 = (inputImage.[y + 2, x + 1]).ToFloat4()
            let i22 = (inputImage.[y + 2, x + 2]).ToFloat4()

            Gx <- i00 + float4(2.0f) * i10 + i20 - i02  - float4(2.0f) * i12 - i22
            Gy <- i00 - i20  + float4(2.0f) * i01 - float4(2.0f) * i21 + i02 - i22

            outputImage.[y, x] <- (float4.hypot(Gx, Gy)/float4(2.0f)).ToUChar4()

[<FRTFeatureExtractionTrainingSample("SobelFilter")>]
type SobelFilterTrainingSample() = 
    inherit FRTFeatureExtractionTrainingSample()
    
                        
    [<ConfigurationItem>]
    member val MinMatrixSize = 64L with get, set    
    [<ConfigurationItem>]
    member val MaxMatrixSize = 2048L with get, set   
    [<ConfigurationItem>]
    member val Iterations = 100 with get, set
        
    member this.Verify(o: uchar4[,], r: uchar4[,]) =
        let mutable eq = true
        for row = 0 to o.GetLength(0) - 1 do
            for col = 0 to o.GetLength(1) - 1 do
                let f = o.[row, col]
                let s = r.[row, col]
                let f0,f1,f2,f3 = f.x, f.y, f.z, f.w
                let s0,s1,s2,s3 = s.x, s.y, s.z, s.w
                if f0 <> s0 || f1 <> s1 || f2 <> s2 || f3 <> s3 then
                    eq <- false
        eq

    member this.CreateVerifiedOutput(input: uchar4[,]) =
        let output = Array2D.zeroCreate<uchar4> (input.GetLength(0)) (input.GetLength(1))

        for row = 1 to (input.GetLength(0)) - 2 do
            for col = 1 to (input.GetLength(1)) - 2 do
                let gx = input.[row - 1, col - 1].ToFloat4() +
                            ((2 |> byte) * input.[row - 1, col]).ToFloat4() +
                            input.[row - 1, col + 1].ToFloat4() -
                            (input.[row + 1, col - 1].ToFloat4() +
                             ((2 |> byte) * input.[row + 1, col]).ToFloat4() +
                             input.[row + 1, col + 1].ToFloat4())

                let gy = input.[row - 1, col - 1].ToFloat4() -
                            input.[row - 1, col + 1].ToFloat4() +
                            ((2 |> byte) * input.[row, col - 1]).ToFloat4() -
                            ((2 |> byte) * input.[row, col + 1]).ToFloat4() +
                            input.[row + 1, col - 1].ToFloat4() -
                            input.[row + 1, col + 1].ToFloat4()

                let gxc = [| gx.x; gx.y; gx.z; gx.w |]
                let gyc = [| gy.x; gy.y; gy.z; gy.w |]

                output.[row, col] <- ((gx.pown(2) + gy.pown(2)).sqrt() / 2.0f).ToUChar4()
        output
        
    member this.CreateVerifiedOutputNoBorder(input: uchar4[,]) =
        let output = Array2D.zeroCreate<uchar4> (input.GetLength(0) - 2) (input.GetLength(1) - 2)

        for row = 0 to (input.GetLength(0)) - 3 do
            for col = 0 to (input.GetLength(1)) - 3 do
                let gx = input.[row, col].ToFloat4() +
                            ((2 |> byte) * input.[row, col + 1]).ToFloat4() +
                            input.[row, col + 2].ToFloat4() -
                            (input.[row + 2, col].ToFloat4() +
                             ((2 |> byte) * input.[row + 2, col + 1]).ToFloat4() +
                             input.[row + 2, col + 2].ToFloat4())

                let gy = input.[row, col].ToFloat4() -
                            input.[row, col + 2].ToFloat4() +
                            ((2 |> byte) * input.[row + 1, col]).ToFloat4() -
                            ((2 |> byte) * input.[row + 1, col + 2]).ToFloat4() +
                            input.[row + 2, col].ToFloat4() -
                            input.[row + 2, col + 2].ToFloat4()

                let gxc = [| gx.x; gx.y; gx.z; gx.w |]
                let gyc = [| gy.x; gy.y; gy.z; gy.w |]

                output.[row, col] <- ((gx.pown(2) + gy.pown(2)).sqrt() / 2.0f).ToUChar4()
        box output
                
    override this.Run(features, devices, opts) =
        let rm = opts.["RunningMode"] :?> TrainingSampleRunningMode
        let runtimeRun = opts.["RuntimeRun"] :?> obj-> obj

        let featureOnly = rm = TrainingSampleRunningMode.OnlyFeatures
        let etOnly = rm = TrainingSampleRunningMode.OnlyExecutionTime
            
        let compiler = new Compiler()      
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
                                s := !s * 2L
                        }) |> Array.ofSeq

        for size, _ in sizes do
            let times = List<float32>()
            Console.WriteLine("      Size: " + String.Format("{0,10:##########}", size))

            let outputSize = (size |> int)
            let inputSize = outputSize + 2
                        
            // Create input
            let input = Array2D.init<uchar4> inputSize inputSize (fun r c -> uchar4(rnd.Next() % 5 |> byte, rnd.Next() % 5 |> byte, rnd.Next() % 5 |> byte, rnd.Next() % 5 |> byte))

            // Compute reference for verification
            let reference = 
                if not featureOnly then
                    this.CreateVerifiedOutputNoBorder((input))
                else
                    null  

            let output = Array2D.zeroCreate<uchar4> (outputSize) (outputSize)
            let ws = WorkSize([| (((outputSize - 1) / 16) + 1) * 16 |> int64; (((outputSize - 1) / 16) + 1) * 16 |> int64 |], [| 16L; 16L |])

            let comp = <@ 
                            Sobel.SobelFilter2DNoBorder(
                                BUFFER_READ_MODE(rm, 
                                    MEMORY_FLAGS(ifl, 
                                        input)),
                                BUFFER_WRITE_MODE(wm, 
                                    MEMORY_FLAGS(ofl, 
                                        output)),
                                ws) @>   
                            
            // Extract features
            if not etOnly then
                let km = compiler.Compile(comp, opts) :?> IKernelModule
                let precomputedFeatures = features.BuildFinalizers(km)
                featureValues.Add(features.EvaluateFinalizers(km, precomputedFeatures, [ input; output; ws ]))
                           
            // Iterate on devices
            for pid, _, platform in devices do   
                for did, _, _ in platform do   
                                                   
                    let output = Array2D.zeroCreate<uchar4> (outputSize) (outputSize) 
                    let comp = <@ DEVICE(pid, did,
                                         Sobel.SobelFilter2DNoBorder(
                                            BUFFER_READ_MODE(rm, 
                                                MEMORY_FLAGS(ifl, 
                                                    input)),
                                            BUFFER_WRITE_MODE(wm, 
                                                MEMORY_FLAGS(ofl, 
                                                    output)),
                                            ws)) @>   
                          
                    // Run once to skip compilation time
                    if not featureOnly then    
                        runtimeRun(comp) |> ignore
                        if not true then
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
