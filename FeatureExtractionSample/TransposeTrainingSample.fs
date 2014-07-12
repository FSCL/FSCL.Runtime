module TransposeTrainingSample

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

[<ReflectedDefinition; Device(0,1)>]
let TransposeFloat4(output: float4[], input: float4[], [<AddressSpace(AddressSpace.Local)>] block: float4[]) =
    let wiWidth  = get_global_size(0)
    let gix_t = get_group_id(0)
    let giy_t = get_group_id(1)
    let num_of_blocks_x = get_num_groups(0)

    // break memory banks dependency by "reshuffling" global indeces
    let giy = gix_t
    let gix = giy_t

    let lix = get_local_id(0)
    let liy = get_local_id(1)

    let blockSize = 16//get_local_size(0)

    let mutable ix = (gix*blockSize) + lix
    let mutable iy = (giy*blockSize) + liy
    let index_in = ix + (iy *wiWidth*4)

    // coalesced copy from input global memory into LDS
    let mutable ind = (liy*blockSize*4)+lix
    block.[ind] <- input.[index_in]
    block.[ind+blockSize] <- input.[index_in+wiWidth]
    block.[ind+(blockSize*2)] <- input.[index_in+(wiWidth*2)]
    block.[ind+(blockSize*3)] <- input.[index_in+(wiWidth*3)]
    
    // wait until the whole block is filled
    barrier(CLK_LOCAL_MEM_FENCE)
    
    // calculate the corresponding target 
    // as location inside block of transposed location
    ix <- (giy*blockSize) + lix
    iy <- (gix*blockSize) + liy
    let index_out = ix + (iy*wiWidth*4)

    ind <- (lix*blockSize*4)+liy
    let v0 = block.[ind]
    let v1 = block.[ind+blockSize]
    let v2 = block.[ind+(blockSize*2)]
    let v3 = block.[ind+(blockSize*3)]

    // coalesced copy of transposed data in LDS into output global memory
    output.[index_out] <- float4(v0.x, v1.x, v2.x, v3.x)
    output.[index_out+wiWidth] <- float4(v0.y, v1.y, v2.y, v3.y)
    output.[index_out+(wiWidth*2)] <- float4(v0.z, v1.z, v2.z, v3.z)
    output.[index_out+(wiWidth*3)] <- float4(v0.w, v1.w, v2.w, v3.w)
    
[<ReflectedDefinition>]
let Transpose(output: float32[], input: float32[], [<AddressSpace(AddressSpace.Local)>] block: float32[], width: int, height: int) =
    let mutable xIndex = get_global_id(0)
    let mutable yIndex = get_global_id(1)
    let BLOCK_DIM = 16

    if((xIndex < width) && (yIndex < height)) then
        let index_in = yIndex * width + xIndex
        block.[get_local_id(1)*(BLOCK_DIM+1)+get_local_id(0)] <- input.[index_in]

    barrier(CLK_LOCAL_MEM_FENCE)

    // write the transposed matrix tile to global memory
    xIndex <- get_group_id(1) * BLOCK_DIM + get_local_id(0)
    yIndex <- get_group_id(0) * BLOCK_DIM + get_local_id(1)
    if((xIndex < height) && (yIndex < width)) then
        let index_out = yIndex * height + xIndex;
        output.[index_out] <- block.[get_local_id(0)*(BLOCK_DIM+1)+get_local_id(1)]
        
type TransposeTrainingSample() =    
    inherit IDefaultFeatureExtractionTrainingSample()
        
    override this.DefaultConfigurationDictionary() =
        let dict = new Dictionary<string, obj>()
        dict.Add("MinMatrixSize", 64L)
        dict.Add("MaxMatrixSize", 2048L)
        dict.Add("Iterations", 100)
        dict
        
    override this.Verify(output: obj, reference: obj) =
        let o = output :?> float4[]
        let r = reference :?> float4[]
        let mutable i = 0
        let mutable eq = true
        while eq && i < o.Length do
            if o.[i] <> r.[i] then  
                eq <- false
            else
                i <- i + 1
        eq
    
    override this.CreateVerifiedOutput(o: obj) =
        let a,w = o :?> float32[] * int
        let result = Array.zeroCreate<float32> a.Length
        
        box result

    override this.ResultColumnIDs 
        with get() =   
            let ids = new List<String>()         
            for pIndex, pName, pDevs in GetOpenCLPlatforms() do  
                for dIndex, dName, dType in pDevs do  
                    ids.Add(dName + " Completion Time (ms)")
            ids.Add("Matrix Width (elements)")
            ids.Add("Matrix Height (elements)")
            ids |> List.ofSeq
    
    override this.RunInternal(chain, conf) = 
        let configuration = IDefaultFeatureExtractionTrainingSample.ConfigurationToDictionary(conf)
        let minSize = Int64.Parse(configuration.["MinMatrixSize"])
        let maxSize = Int64.Parse(configuration.["MaxMatrixSize"])
        let iterations = Int32.Parse(configuration.["Iterations"])

        let compiler = new Compiler()
        let opts = new Dictionary<string, obj>()        
        let rnd = System.Random()

        let rm = BufferReadMode.EnqueueReadBuffer
        let wm = BufferWriteMode.EnqueueWriteBuffer
        let ifl = MemoryFlags.UseHostPointer ||| MemoryFlags.ReadOnly
        let ofl = MemoryFlags.UseHostPointer ||| MemoryFlags.ReadOnly

        let mutable execResults: obj list list = []
        let blockSize = 16L
        let elementsPerThread = 4L
                
        let sizes = (seq {
                            let s = ref minSize
                            while !s <= maxSize do
                                yield (!s, !s)
                                //yield (!s, !s * 2L)
                                s := !s + minSize
                        }) |> Array.ofSeq

        for rows, cols in sizes do
            Console.WriteLine("      Size: " + String.Format("{0,5:#####}", rows) + "x" + String.Format("{0,5:#####}", cols * 4L))
                                          
            let a = Array.init (rows * cols |> int) (fun i -> 
                                                        let r = (i |> int64) / cols
                                                        r |> float32)                                    
            let block = Array.zeroCreate<float32> (blockSize * (blockSize + 1L) |> int)
            //let rf = float4tofloat(reference)
            let mutable features: obj list = []
            let mutable instanceResult: obj list = []
            for pIndex, pName, pDevs in GetOpenCLPlatforms() do   
                for dIndex, dName, dType in pDevs do                                
                    Console.WriteLine(" Device " + ": " + dName.ToString() + "(" + dType.ToString() + ")")                                    
                    let c = Array.zeroCreate<float32> (cols * rows |> int)

                    let comp = <@ DEVICE(pIndex, dIndex,
                                    Transpose(
                                        BUFFER_WRITE_MODE(wm, 
                                            MEMORY_FLAGS(ofl, 
                                                c)),
                                        BUFFER_READ_MODE(rm, 
                                            MEMORY_FLAGS(ifl, 
                                                a)),
                                        block,
                                        cols |> int,
                                        rows |> int)) @>   
                        
                    // Extract features
                    let km = compiler.Compile(comp, opts) :?> IKernelModule
                    let precomputedFeatures = chain.Precompute(km)
                    features <- chain.Evaluate(km, precomputedFeatures, [ c; a; block; cols; rows ], [| rows; cols |], [| blockSize; blockSize |], opts)
                                                                                                          
                    // Run once to skip compilation time
                    comp.Run([| rows; cols |], [| blockSize; blockSize |])
                    let reference = this.CreateVerifiedOutput((a, cols |> int)) :?> float4[]
                    if not (this.Verify(c, reference)) then
                        Console.WriteLine("---------------- COMPUTATION RESULT ERROR")
                    else
                        // Run
                        let watch = new Stopwatch()
                        watch.Start()
                        for i = 0 to iterations - 1 do
                            comp.Run([| rows / elementsPerThread; cols |], [| blockSize; blockSize |])
                        watch.Stop()
                        let ttime, iters = ((double)watch.ElapsedMilliseconds) /((double)iterations), iterations
                                
                        // Dump
                        Console.WriteLine("---------------- " + String.Format("{0,11:######0.0000}", ttime) + "ms (" + String.Format("{0,10:#########0}", iters) + " iterations)")
                        instanceResult <- instanceResult @ [ ttime ]
                        System.Threading.Thread.Sleep(500)
                                
            execResults <- execResults @ [ instanceResult @ [ rows; cols * 4L ] @ features ]     
        execResults

type TransposeFloat4TrainingSample() =    
    inherit IDefaultFeatureExtractionTrainingSample()
        
    override this.DefaultConfigurationDictionary() =
        let dict = new Dictionary<string, obj>()
        dict.Add("MinMatrixSize", 64L)
        dict.Add("MaxMatrixSize", 2048L)
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
        let a,w = o :?> float32[] * int
        let result = Array.zeroCreate<float32> a.Length
        for i = 0 to a.Length - 1 do
            let r = i / w
            let c = i % w
            result.[c * w + r] <- a.[r * w + c]
        box result

    override this.ResultColumnIDs 
        with get() =   
            let ids = new List<String>()         
            for pIndex, pName, pDevs in GetOpenCLPlatforms() do  
                for dIndex, dName, dType in pDevs do  
                    ids.Add(dName + " Completion Time (ms)")
            ids.Add("Matrix Width (elements)")
            ids.Add("Matrix Height (elements)")
            ids |> List.ofSeq
    
    override this.RunInternal(chain, conf) = 
        let configuration = IDefaultFeatureExtractionTrainingSample.ConfigurationToDictionary(conf)
        let minSize = Int64.Parse(configuration.["MinMatrixSize"])
        let maxSize = Int64.Parse(configuration.["MaxMatrixSize"])
        let iterations = Int32.Parse(configuration.["Iterations"])

        let compiler = new Compiler()
        let opts = new Dictionary<string, obj>()        
        let rnd = System.Random()

        let rm = BufferReadMode.EnqueueReadBuffer
        let wm = BufferWriteMode.EnqueueWriteBuffer
        let ifl = MemoryFlags.UseHostPointer ||| MemoryFlags.ReadOnly
        let ofl = MemoryFlags.UseHostPointer ||| MemoryFlags.ReadOnly

        let mutable execResults: obj list list = []
        let blockSize = 16L
        let elementsPerThread = 4L
                
        let sizes = (seq {
                            let s = ref minSize
                            while !s <= maxSize do
                                yield (!s, !s)
                                //yield (!s, !s * 2L)
                                s := !s + minSize
                        }) |> Array.ofSeq

        for rows, cols in sizes do
            Console.WriteLine("      Size: " + String.Format("{0,5:#####}", rows) + "x" + String.Format("{0,5:#####}", cols))
                                          
            let a = Array.init (rows * cols |> int) (fun i -> 
                                                        let r = (i |> int64) / cols
                                                        r |> float32)                                    
            let block = Array.zeroCreate<float32> (blockSize * blockSize * elementsPerThread * elementsPerThread  |> int)
            //let rf = float4tofloat(reference)
            let mutable features: obj list = []
            let mutable instanceResult: obj list = []
            for pIndex, pName, pDevs in GetOpenCLPlatforms() do   
                
                for dIndex, dName, dType in pDevs do    
                    if dIndex > 0 then                            
                        Console.WriteLine(" Device " + ": " + dName.ToString() + "(" + dType.ToString() + ")")                                    
                        let c = Array.zeroCreate<float32> (cols * rows |> int)

                        let comp = <@ DEVICE(pIndex, dIndex,
                                        TransposeFloat4(
                                            BUFFER_WRITE_MODE(wm, 
                                                MEMORY_FLAGS(ofl, 
                                                    AsFloat4(c))),
                                            BUFFER_READ_MODE(rm, 
                                                MEMORY_FLAGS(ifl, 
                                                    AsFloat4(a))),
                                            AsFloat4(block))) @>   
                            
                        // Extract features
                        let km = compiler.Compile(comp, opts) :?> IKernelModule
                        let precomputedFeatures = chain.Precompute(km)
                        features <- chain.Evaluate(km, precomputedFeatures, [ AsFloat4(c); AsFloat4(a); AsFloat4(block) ], [| rows / elementsPerThread; cols |], [| blockSize; blockSize |], opts)
                                                                                                              
                        // Run once to skip compilation time
                        comp.Run([| rows / elementsPerThread; cols / elementsPerThread |], [| blockSize; blockSize |])
                        let reference = this.CreateVerifiedOutput((a, cols |> int)) :?> float32[]
                        if not (this.Verify(c, reference)) then
                            Console.WriteLine("---------------- COMPUTATION RESULT ERROR")
                        else
                            // Run
                            let watch = new Stopwatch()
                            watch.Start()
                            for i = 0 to iterations - 1 do
                                comp.Run([| rows / elementsPerThread; cols / elementsPerThread |], [| blockSize; blockSize |])
                            watch.Stop()
                            let ttime, iters = ((double)watch.ElapsedMilliseconds) /((double)iterations), iterations
                                    
                            // Dump
                            Console.WriteLine("---------------- " + String.Format("{0,11:######0.0000}", ttime) + "ms (" + String.Format("{0,10:#########0}", iters) + " iterations)")
                            instanceResult <- instanceResult @ [ ttime ]
                            System.Threading.Thread.Sleep(500)
                                    
            execResults <- execResults @ [ instanceResult @ [ rows; cols ] @ features ]     
        execResults