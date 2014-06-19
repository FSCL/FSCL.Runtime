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

[<ReflectedDefinition; DynamicConstantDefine>]
let mutable FILTER_WIDTH = 3

[<ReflectedDefinition>]
let Convolution(pInput: float32[], [<AddressSpace(AddressSpace.Constant)>] pFilter: float32[], pOutput: float32[], nInWidth: int) =
    let nWidth = get_global_size(0)
    let xOut = get_global_id(0) 
    let yOut = get_global_id(1)
    
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
        if cMod = 1 then 
            let idxF = idxFtmp + c
            let idxIn = idxIntmp + c
            sum4.x <- sum4.x + pFilter.[idxF]*pInput.[idxIn]
        else if cMod = 2 then
            //Use float4 here to further optimize the kernel 
            let idxF = idxFtmp + c
            let idxIn = idxIntmp + c
            sum4.x <- sum4.x + pFilter.[idxF]* pInput.[idxIn]
            sum4.y <- sum4.y + pFilter.[idxF+1]*pInput.[idxIn+1]
        else if cMod = 3 then
            //Use float4 here to further optimize the kernel 
            let idxF = idxFtmp + c
            let idxIn = idxIntmp + c
            sum4.x <- sum4.x + pFilter.[idxF]*pInput.[idxIn]
            sum4.y <- sum4.y + pFilter.[idxF+1]*pInput.[idxIn+1]
            sum4.z <- sum4.z + pFilter.[idxF+2]*pInput.[idxIn+2]

    let idxOut = yOut * nWidth + xOut
    pOutput.[idxOut] <- sum4.x + sum4.y + sum4.z + sum4.w

type ConvolutionTrainingSample() =    
    inherit IDefaultFeatureExtractionTrainingSample()

    override this.DefaultConfigurationDictionary() =
        let dict = new Dictionary<string, obj>()
        dict.Add("MinMatrixSize", 64L)
        dict.Add("MaxMatrixSize", 2048L)
        dict.Add("MinFilterSize", 3L)
        dict.Add("MaxFilterSize", 31L)
        dict.Add("Iterations", 100)
        dict
        
    override this.Verify(output: obj, reference: obj) =
        (output :?> float32[]) = (reference :?> float32[])
    
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
            ids.Add("Matrix Width (elements)")
            ids.Add("Matrix Height (elements)")
            ids.Add("Filter Width (elements)")
            ids.Add("Filter Height (elements)")
            ids |> List.ofSeq
    
    override this.RunInternal(chain, conf) = 
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

        let rm = BufferReadMode.MapBuffer
        let wm = BufferWriteMode.MapBuffer
        let ifl = MemoryFlags.HostWriteOnly ||| MemoryFlags.UseHostPointer ||| MemoryFlags.ReadOnly
        let ofl = MemoryFlags.HostReadOnly ||| MemoryFlags.UseHostPointer ||| MemoryFlags.WriteOnly

        let mutable execResults: obj list list = []
                
        for filterSize in minFilterSize .. 2L .. maxFilterSize do
            opts.[RuntimeOptions.ConstantDefines] <- [ ("FILTER_WIDTH", box filterSize) ]
            Console.WriteLine("     Filter Size: " + String.Format("{0,5:#####}", filterSize) + "x" + String.Format("{0,5:#####}", filterSize))

            let mutable matSize = minSize
            while matSize <= maxSize do
                Console.WriteLine("      Size: " + String.Format("{0,5:#####}", matSize) + "x" + String.Format("{0,5:#####}", matSize))
           
                let inputSize = matSize + (filterSize |> int64) - 1L                          
                let a = Array.init (inputSize * inputSize |> int) (fun it -> ((it |> int64) / inputSize) |> float32)
                let filter = Array.create (filterSize * filterSize |> int) 2.0f  
                let reference = this.CreateVerifiedOutput((a, filter, filterSize |> int, inputSize |> int)) :?> float32[]

                let mutable features: obj list = []
                let mutable instanceResult: obj list = []
                        
                for pIndex, pName, pDevs in GetOpenCLPlatforms() do        
                    for dIndex, dName, dType in pDevs do
                        let c = Array.zeroCreate (matSize * matSize |> int)
                                
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
                                            inputSize |> int)) @>

                        // Extract features
                        let km = compiler.Compile(comp, opts) :?> IKernelModule
                        let precomputedFeatures = chain.Precompute(km)
                        features <- chain.Evaluate(km, precomputedFeatures, [ a; filter; c; matSize |> int ], [| matSize; matSize |], [| 16L; 16L |], opts)

                        // Run once to skip compilation time
                        comp.Run([| matSize; matSize |], [| 16L; 16L |], opts)
                        if not (this.Verify(c, reference)) then
                            Console.WriteLine("---------------- COMPUTATION RESULT ERROR")
                        else                            
                            // Run
                            let watch = new Stopwatch()
                            watch.Start()
                            for i = 0 to iterations - 1 do
                                comp.Run([| matSize; matSize |], [| 16L; 16L |], opts)
                            watch.Stop()
                            let ttime, iters = ((double)watch.ElapsedMilliseconds) /((double)iterations), iterations
                                    
                            Console.WriteLine("---------------- " + String.Format("{0,11:######0.0000}", ttime) + "ms (" + String.Format("{0,10:#########0}", iters) + " iterations)")
                            instanceResult <- instanceResult @ [ ttime ]
                            System.Threading.Thread.Sleep(500)

                    let featureValues = features |> List.map(fun (featV) -> featV.ToString()) |> String.concat ";"

                    execResults <- execResults @ [ instanceResult @ [matSize; matSize; filterSize; filterSize] @ features ]  
                    matSize <- matSize * 2L
        execResults

