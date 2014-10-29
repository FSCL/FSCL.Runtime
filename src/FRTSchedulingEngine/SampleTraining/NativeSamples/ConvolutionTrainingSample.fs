namespace FSCL.Runtime.Scheduling.FRTSchedulingEngine.TrainingSamples

open FSCL
open FSCL.Compiler
open FSCL.Runtime.Scheduling
open FSCL.Language
open System.Collections.Generic
open System
open System.IO
open FSCL.Runtime
open FSCL.Language
open System.Diagnostics
open System.Linq
open Microsoft.FSharp.Quotations
open FSCL.Runtime.Scheduling.FRTSchedulingEngine

[<FRTFeatureExtractionTrainingSample("Convolution")>]
type ConvolutionTrainingSample() =    
    inherit FRTFeatureExtractionTrainingSample()
    
    [<ReflectedDefinition>]
    let mutable FILTER_WIDTH = 3

    [<ReflectedDefinition>]
    let Convolution4(pInput: float32[], [<AddressSpace(AddressSpace.Constant)>] pFilter: float32[], pOutput: float32[], nInWidth: int, wi: WorkItemInfo) =
        let nWidth = wi.GlobalSize(0)
        let xOut = wi.GlobalID(0) 
        let yOut = wi.GlobalID(1)
    
        let xInTopLeft = xOut
        let yInTopLeft = yOut
        let mutable sum4 = float4(0.0f) 
        for r = 0 to FILTER_WIDTH - 1 do
            let idxFtmp = r * FILTER_WIDTH
            let yIn = yInTopLeft + r
            let idxIntmp = yIn * nInWidth + xInTopLeft
            let mutable c = 0
            let mutable c4 = 0L
            for cidx in 0 .. 4 .. FILTER_WIDTH - 4 do
                let filter4 = float4.vload(c4, pFilter.pasum(idxFtmp))
                let in4 = float4.vload(c4, pInput.pasum(idxIntmp))
                sum4 <- sum4 + in4 * filter4
                c <- cidx + 4
                c4 <- c4 + 1L
            let cMod = FILTER_WIDTH - c
            let idxF = idxFtmp + c
            let idxIn = idxIntmp + c
            (*
            if true then
                if cMod > 0 then // 0.75
                    sum4.x <- sum4.x + pFilter.[idxF]*pInput.[idxIn]
            if true then
                if cMod > 1 then // 0.50
                    //Use float4 here to further optimize the kernel 
                    sum4.y <- sum4.y + pFilter.[idxF+1]*pInput.[idxIn+1]
            if true then
                if cMod > 2 then // 0.25
                    //Use float4 here to further optimize the kernel 
                    sum4.z <- sum4.z + pFilter.[idxF+2]*pInput.[idxIn+2]
                *)
            if true then
                if cMod = 1 then // 0.25
                    sum4.x <- sum4.x + pFilter.[idxF]*pInput.[idxIn]
            if true then
                if cMod = 2 then // 0.25
                    //Use float4 here to further optimize the kernel 
                    sum4.x <- sum4.x + pFilter.[idxF]* pInput.[idxIn]
                    sum4.y <- sum4.y + pFilter.[idxF+1]*pInput.[idxIn+1]
            if true then
                if cMod = 3 then // 0.25
                    //Use float4 here to further optimize the kernel 
                    sum4.x <- sum4.x + pFilter.[idxF]*pInput.[idxIn]
                    sum4.y <- sum4.y + pFilter.[idxF+1]*pInput.[idxIn+1]
                    sum4.z <- sum4.z + pFilter.[idxF+2]*pInput.[idxIn+2]

        let idxOut = yOut * nWidth + xOut
        pOutput.[idxOut] <- sum4.x + sum4.y + sum4.z + sum4.w
    
    [<ReflectedDefinition>]
    let Convolution(pInput: float32[], [<AddressSpace(AddressSpace.Constant)>] pFilter: float32[], pOutput: float32[], nInWidth: int, nWidth: int, wi: WorkItemInfo) =
        let xOut = wi.GlobalID(0) 
        let yOut = wi.GlobalID(1)
    
        if xOut < nWidth && yOut < nWidth then
            let xInTopLeft = xOut
            let yInTopLeft = yOut
            let mutable sum = 0.0f 
            for r = 0 to FILTER_WIDTH - 1 do
                let idxFtmp = r * FILTER_WIDTH
                let yIn = yInTopLeft + r
                let idxIntmp = yIn * nInWidth + xInTopLeft
                for cidx in 0 .. FILTER_WIDTH - 1 do
                    let filterValue = pFilter.[idxFtmp + cidx]
                    let inValue = pInput.[idxIntmp + cidx]
                    sum <- sum + inValue * filterValue

            let idxOut = yOut * nWidth + xOut
            pOutput.[idxOut] <- sum

    [<ConfigurationItem>]
    member val MinMatrixSize = 64L with get, set    
    [<ConfigurationItem>]
    member val MaxMatrixSize = 2048L with get, set    
    [<ConfigurationItem>]
    member val MinFilterSize = 3L with get, set    
    [<ConfigurationItem>]
    member val MaxFilterSize = 19L with get, set 
    [<ConfigurationItem>]
    member val Iterations = 100 with get, set
            
    member this.Verify(o: float32[], r: float32[]) =
        let mutable eq = true
        for row = 0 to o.GetLength(0) - 1 do
            let f = o.[row]
            let s = r.[row]
            if f <> s then
                eq <- false
        eq
    
    member this.CreateVerifiedOutput(input:float32[], filter:float32[], mWidth:int, rWidth:int) =
        let res = Array.zeroCreate<float32>((mWidth - rWidth + 1) * (mWidth - rWidth + 1))
        for row = 0 to mWidth - rWidth  do
            for col = 0 to mWidth - rWidth do
                let mutable v = 0.0f
                for frow = 0 to rWidth - 1 do
                    for fcol = 0 to rWidth - 1 do
                        v <- v + (filter.[frow * rWidth + fcol] * input.[((row + frow) * mWidth + (col + fcol))])
                res.[row * (mWidth - rWidth + 1) + col] <- v
        res

    
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
                            let s = ref this.MaxMatrixSize
                            while !s <= this.MaxMatrixSize do
                                yield (!s, !s)
                                yield (!s + 1L, !s + 1L)
                                s := !s + this.MinMatrixSize
                        }) |> Array.ofSeq

        for filterSize in this.MinFilterSize .. 2L .. this.MaxFilterSize do
            
            for rows, cols in sizes do
                let times = List<float32>()
                
                let inputSize = cols + (filterSize |> int64) - 1L                          
                let a = Array.init (inputSize * inputSize |> int) (fun it -> ((it |> int64) / inputSize) |> float32)
                let filter = Array.create (filterSize * filterSize |> int) 2.0f  
                let reference = 
                    if not featureOnly then
                        this.CreateVerifiedOutput(a, filter, inputSize |> int, filterSize |> int)
                    else
                        [||]
                let c = Array.zeroCreate (cols * rows |> int)
                let ws = WorkSize([| (((rows - 1L) / 16L) + 1L) * 16L; (((cols - 1L) / 16L) + 1L) * 16L |], [| 16L; 16L |])
               
                // Extract features
                let comp = <@   Convolution(
                                    BUFFER_READ_MODE(rm, 
                                        MEMORY_FLAGS(ifl, 
                                            a)),
                                    BUFFER_READ_MODE(rm, 
                                        MEMORY_FLAGS(ifl, 
                                            filter)),
                                    BUFFER_WRITE_MODE(wm, 
                                        MEMORY_FLAGS(ofl, 
                                            c)),
                                    inputSize |> int,
                                    cols |> int,
                                    ws) @>

                if not etOnly then
                    let km = compiler.Compile(comp) :?> IKernelModule
                    let precomputedFeatures = features.BuildFinalizers(km)
                    featureValues.Add(features.EvaluateFinalizers(km, precomputedFeatures, [ this; a; filter; c; inputSize |> int; cols |> int; ws ]))
     
                // Get completion times
                for pid, _, platform in devices do
                    for did, _, _ in platform do
                        let c = Array.zeroCreate (cols * rows |> int)
                       
                        let comp = <@ DEVICE(pid, did, 
                                             Convolution(
                                                BUFFER_READ_MODE(rm, 
                                                    MEMORY_FLAGS(ifl, 
                                                        a)),
                                                BUFFER_READ_MODE(rm, 
                                                    MEMORY_FLAGS(ifl, 
                                                        filter)),
                                                BUFFER_WRITE_MODE(wm, 
                                                    MEMORY_FLAGS(ofl, 
                                                        c)),
                                                inputSize |> int,
                                                cols |> int,
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
                executionResults.Add(times |> List.ofSeq)

        (featureValues, executionResults) ||> Seq.zip |> List.ofSeq

