module ConvolutionTrainingSample

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

[<ReflectedDefinition; DynamicConstantDefine>]
let FILTER_WIDTH = 3

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

type ConvolutionTrainingSample() =    
    inherit IDefaultFeatureExtractionTrainingSample()

    override this.DefaultConfigurationDictionary() =
        let dict = new Dictionary<string, obj>()
        dict.Add("MinMatrixSize", 64L)
        dict.Add("MaxMatrixSize", 2048L)
        dict.Add("MinFilterSize", 3L)
        dict.Add("MaxFilterSize", 19L)
        dict.Add("Iterations", 100)
        dict
        
    override this.Verify(output: obj, reference: obj) =
        let o = output :?> float32[]
        let r = reference :?> float32[]
        let mutable eq = true
        for row = 0 to o.GetLength(0) - 1 do
            let f = o.[row]
            let s = r.[row]
            if f <> s then
                eq <- false
        eq
    
    override this.CreateVerifiedOutput(o: obj) =
        let input, filter, mWidth, rWidth = o :?> float32[] * float32[] * int * int
        let res = Array.zeroCreate<float32>((mWidth - rWidth + 1) * (mWidth - rWidth + 1))
        for row = 0 to mWidth - rWidth  do
            for col = 0 to mWidth - rWidth do
                let mutable v = 0.0f
                for frow = 0 to rWidth - 1 do
                    for fcol = 0 to rWidth - 1 do
                        v <- v + (filter.[frow * rWidth + fcol] * input.[((row + frow) * mWidth + (col + fcol))])
                res.[row * (mWidth - rWidth + 1) + col] <- v
        box res

    override this.ResultColumnIDs 
        with get() =   
            let ids = new List<String>()         
            for pIndex, pName, pDevs in GetOpenCLPlatforms() do  
                for dIndex, dName, dType in pDevs do  
                    ids.Add(dName + " Completion Time (ms)")
                    ids.Add(dName + " Completion Time STDDEV")
            //ids.Add("Matrix Width (elements)")
            //ids.Add("Matrix Height (elements)")
            //ids.Add("Filter Width (elements)")
            //ids.Add("Filter Height (elements)")
            ids |> List.ofSeq
    
    override this.RunInternal(chain, conf, rm: TrainingSampleRunningMode) = 
        let featureOnly = rm = TrainingSampleRunningMode.OnlyFeatures
        let etOnly = rm = TrainingSampleRunningMode.OnlyExecutionTime

        let configuration = IDefaultFeatureExtractionTrainingSample.ConfigurationToDictionary(conf)
        let minSize = Int64.Parse(configuration.["MinMatrixSize"])
        let maxSize = Int64.Parse(configuration.["MaxMatrixSize"])
        let minFilterSize = Int64.Parse(configuration.["MinFilterSize"])
        let maxFilterSize = Int64.Parse(configuration.["MaxFilterSize"])
        let iterations = Int32.Parse(configuration.["Iterations"])

        let compiler = new Compiler()
        let opts = new Dictionary<string, obj>()   
        opts.Add(RuntimeOptions.ConstantDefines, [])     
        let rnd = System.Random()

        let rm = BufferReadMode.EnqueueReadBuffer
        let wm = BufferWriteMode.EnqueueWriteBuffer
        let ifl = MemoryFlags.ReadOnly //  ||| MemoryFlags.UseHostPointer
        let ofl = MemoryFlags.WriteOnly //  ||| MemoryFlags.UseHostPointer
        
        let executionResults = new List<List<obj>>()
        let featureValues = new List<List<obj>>()
                
        let sizes = (seq {
                            let s = ref minSize
                            while !s <= maxSize do
                                yield (!s, !s)
                                yield (!s + 1L, !s + 1L)
                                s := !s + minSize
                        }) |> Array.ofSeq

        for filterSize in minFilterSize .. 2L .. maxFilterSize do
            opts.[RuntimeOptions.ConstantDefines] <- [ ("FILTER_WIDTH", box (filterSize |> int)) ]
            Console.WriteLine("     Filter Size: " + String.Format("{0,5:#####}", filterSize) + "x" + String.Format("{0,5:#####}", filterSize))
            
            for rows, cols in sizes do
                executionResults.Add(new List<obj>())
                Console.WriteLine("      Size: " + String.Format("{0,5:#####}", cols) + "x" + String.Format("{0,5:#####}", cols))
           
                let inputSize = cols + (filterSize |> int64) - 1L                          
                let a = Array.init (inputSize * inputSize |> int) (fun it -> ((it |> int64) / inputSize) |> float32)
                let filter = Array.create (filterSize * filterSize |> int) 2.0f  
                let reference = 
                    if not featureOnly then
                        this.CreateVerifiedOutput((a, filter, inputSize |> int, filterSize |> int)) :?> float32[]
                    else
                        [||]
                                                
                for pIndex, pName, pDevs in GetOpenCLPlatforms() do        
                    for dIndex, dName, dType in pDevs do
                        let c = Array.zeroCreate (cols * rows |> int)
                        let ws = WorkSize([| (((rows - 1L) / 16L) + 1L) * 16L; (((cols - 1L) / 16L) + 1L) * 16L |], [| 16L; 16L |])

                        Console.WriteLine(" Device " + ": " + dName.ToString() + "(" + dType.ToString() + ")")  
                        let comp = <@ DEVICE(pIndex, dIndex,
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

                        // Extract features
                        if pIndex = 0 && dIndex = 0 && not etOnly then
                            let km = compiler.Compile(comp, opts) :?> IKernelModule
                            let precomputedFeatures = chain.Precompute(km)
                            featureValues.Add(new List<obj>(chain.Evaluate(km, precomputedFeatures, [ a; filter; c; inputSize |> int; cols |> int; ws ],  opts)))

                        // Run once to skip compilation time
                        if not featureOnly then    
                            comp.Run(opts)
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

        executionResults, featureValues

