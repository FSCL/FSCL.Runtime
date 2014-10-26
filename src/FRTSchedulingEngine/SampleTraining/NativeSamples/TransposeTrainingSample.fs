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
open OpenCL
open System.Linq
open Microsoft.FSharp.Quotations
(*
[<ReflectedDefinition>]
let TransposeFloat4(output: float4[], input: float4[], [<AddressSpace(AddressSpace.Local)>] block: float4[], wi: WorkItemInfo) =
    let wiWidth  = wi.GlobalSize(0)
    let gix_t = wi.GroupID(0)
    let giy_t = wi.GroupID(1)
    let num_of_blocks_x = wi.NumGroups(0)

    // break memory banks dependency by "reshuffling" global indeces
    let giy = gix_t
    let gix = giy_t

    let lix = wi.LocalID(0)
    let liy = wi.LocalID(1)

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
    wi.Barrier(CLK_LOCAL_MEM_FENCE)
    
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
    *)
       
[<FRTFeatureExtractionTrainingSample("TransposeTiled")>] 
type TransposeTrainingSample() =    
    inherit FRTFeatureExtractionTrainingSample()
        
    [<ReflectedDefinition>]
    let Transpose(output: float32[], input: float32[], [<AddressSpace(AddressSpace.Local)>] block: float32[], width: int, height: int, wi: WorkItemInfo) =
        let mutable xIndex = wi.GlobalID(0)
        let mutable yIndex = wi.GlobalID(1)
        let BLOCK_DIM = 16

        if((xIndex < width) && (yIndex < height)) then
            let index_in = yIndex * width + xIndex
            block.[(wi.LocalID(1)*(BLOCK_DIM+1))+wi.LocalID(0)] <- input.[index_in]

        wi.Barrier(CLK_LOCAL_MEM_FENCE)

        // write the transposed matrix tile to global memory
        xIndex <- (wi.GroupID(1) * BLOCK_DIM) + wi.LocalID(0)
        yIndex <- (wi.GroupID(0) * BLOCK_DIM) + wi.LocalID(1)
        if((xIndex < height) && (yIndex < width)) then
            let index_out = (yIndex * height) + xIndex;
            output.[index_out] <- block.[(wi.LocalID(0)*(BLOCK_DIM+1))+wi.LocalID(1)]
            
    [<ConfigurationItem>]
    member val MinMatrixSize = 64L with get, set    
    [<ConfigurationItem>]
    member val MaxMatrixSize = 4096L with get, set   
    [<ConfigurationItem>]
    member val Iterations = 100 with get, set
        
    member this.Verify(o: float32[], r: float32[]) =
        let mutable i = 0
        let mutable eq = true
        while eq && i < o.Length do
            if o.[i] <> r.[i] then  
                eq <- false
            else
                i <- i + 1
        eq
    
    member this.CreateVerifiedOutput(a: float32[], w: int) =
        let result = Array.zeroCreate<float32> a.Length
        for i = 0 to a.Length - 1 do
            let r = i / w
            let c = i % w
            result.[c * w + r] <- a.[r * w + c]
        result
                
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
        
        let executionResults = new List<float32[]>()
        let featureValues = new List<float32[]>()

        let blockSize = 16L
        let elementsPerThread = 4L
                
        let sizes = (seq {
                            let s = ref this.MinMatrixSize
                            while !s <= this.MaxMatrixSize do
                                yield (!s, !s)
                                yield (!s + 1L, !s + 1L)
                                s := !s + this.MinMatrixSize
                        }) |> Array.ofSeq

        for rows, cols in sizes do
            let times = List<float32>()
            Console.WriteLine("      Size: " + String.Format("{0,5:#####}", rows) + "x" + String.Format("{0,5:#####}", cols))
                                          
            let a = Array.init (rows * cols |> int) (fun i -> 
                                                        let r = (i |> int64) / cols
                                                        r |> float32)                                    
            let block = Array.zeroCreate<float32> (blockSize * (blockSize + 1L) |> int)
            let reference = 
                if not featureOnly then
                    this.CreateVerifiedOutput(a, cols |> int)
                else
                    [||]
                        
            let c = Array.zeroCreate<float32> (cols * rows |> int)
            let ws = WorkSize([| (((rows - 1L) / blockSize) + 1L) * blockSize; (((cols - 1L) / blockSize) + 1L) * blockSize |], [| blockSize; blockSize |])
                
            // Extract features                                                
            let comp = <@   Transpose(
                                BUFFER_WRITE_MODE(wm, 
                                    MEMORY_FLAGS(ofl, 
                                        c)),
                                BUFFER_READ_MODE(rm, 
                                    MEMORY_FLAGS(ifl, 
                                        a)),
                                block,
                                cols |> int,
                                rows |> int,
                                ws) @>   
                        
            // Extract features
            if not etOnly then
                let km = compiler.Compile(comp, opts) :?> IKernelModule
                let precomputedFeatures = features.BuildFinalizers(km)
                featureValues.Add(features.EvaluateFinalizers(km, precomputedFeatures, [ c; a; block; cols |> int; rows |> int; ws ]))
                                                                              
            // Get completion times
            for pip, _, platform in devices do   
                for did, _, _ in platform do                                                                    
                    let c = Array.zeroCreate<float32> (cols * rows |> int)
                    
                    let comp = <@ DEVICE(pip, did, 
                                         Transpose(
                                            BUFFER_WRITE_MODE(wm, 
                                                MEMORY_FLAGS(ofl, 
                                                    c)),
                                            BUFFER_READ_MODE(rm, 
                                                MEMORY_FLAGS(ifl, 
                                                    a)),
                                            block,
                                            cols |> int,
                                            rows |> int,
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
                               
            executionResults.Add(times |> Array.ofSeq)

        (featureValues, executionResults) ||> Seq.zip |> Array.ofSeq
       
[<FRTFeatureExtractionTrainingSample("TransposeNaive")>]
type TransposeNaiveTrainingSample() =    
    inherit TransposeTrainingSample()        
    
    [<ReflectedDefinition>]
    let TransposeNaive(output: float32[], input: float32[], width: int, height: int, wi: WorkItemInfo) =
        let xIndex = wi.GlobalID(0)
        let yIndex = wi.GlobalID(1)

        if((xIndex < width) && (yIndex < height)) then
            let index_in = yIndex * width + xIndex
            let index_out = xIndex * height + yIndex;
            output.[index_out] <- input.[index_in]

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
        
        let executionResults = new List<float32[]>()
        let featureValues = new List<float32[]>()

        let blockSize = 16L
        let elementsPerThread = 4L
                
        let sizes = (seq {
                            let s = ref this.MinMatrixSize
                            while !s <= this.MaxMatrixSize do
                                yield (!s, !s)
                                yield (!s + 1L, !s + 1L)
                                s := !s + this.MinMatrixSize
                        }) |> Array.ofSeq

        for rows, cols in sizes do
            let times = List<float32>()
                                          
            let a = Array.init (rows * cols |> int) (fun i -> 
                                                        let r = (i |> int64) / cols
                                                        r |> float32)                                    
            let block = Array.zeroCreate<float32> (blockSize * (blockSize + 1L) |> int)
            let reference = 
                if not featureOnly then
                    this.CreateVerifiedOutput(a, cols |> int)
                else
                    [||]
                                                                                         
            let c = Array.zeroCreate<float32> (cols * rows |> int)
            let ws = WorkSize([| (((rows - 1L) / blockSize) + 1L) * blockSize; (((cols - 1L) / blockSize) + 1L) * blockSize |], [| blockSize; blockSize |])
                                                                          
            // Extract features
            let comp = <@   TransposeNaive(
                                BUFFER_WRITE_MODE(wm, 
                                    MEMORY_FLAGS(ofl, 
                                        c)),
                                BUFFER_READ_MODE(rm, 
                                    MEMORY_FLAGS(ifl, 
                                        a)),
                                cols |> int,
                                rows |> int,
                                ws) @>   
                        
            if not etOnly then
                let km = compiler.Compile(comp, opts) :?> IKernelModule
                let precomputedFeatures = features.BuildFinalizers(km)
                featureValues.Add(features.EvaluateFinalizers(km, precomputedFeatures, [ c; a; cols |> int; rows |> int; ws ]))
                
            // Get completion times
            for pid, _, platform in devices do   
                for did, _, _ in platform do                                                                    
                    let c = Array.zeroCreate<float32> (cols * rows |> int)

                    let comp = <@ DEVICE(pid, did, 
                                         TransposeNaive(
                                            BUFFER_WRITE_MODE(wm, 
                                                MEMORY_FLAGS(ofl, 
                                                    c)),
                                            BUFFER_READ_MODE(rm, 
                                                MEMORY_FLAGS(ifl, 
                                                    a)),
                                            cols |> int,
                                            rows |> int,
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

            executionResults.Add(times |> Array.ofSeq)
                               
        (featureValues, executionResults) ||> Seq.zip |> Array.ofSeq
    (*
type TransposeFloat4TrainingSample() =    
    inherit TransposeTrainingSample()
            
    override this.Run(features, devices, rm: TrainingSampleRunningMode) = 
        let featureOnly = rm = TrainingSampleRunningMode.OnlyFeatures
        let etOnly = rm = TrainingSampleRunningMode.OnlyExecutionTime

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
        let ofl = MemoryFlags.UseHostPointer ||| MemoryFlags.WriteOnly

        let mutable execResults: obj list list = []
        let blockSize = 16L
        let elementsPerThread = 4L
        
        let executionResults = new List<float32[]>()
        let featureValues = new List<float32[]>()

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
            for pIndex, _, platform in devices do   
                
                for dIndex, _, _ in platform do       
                    if ((OpenCL.OpenCLPlatform.Platforms.[pIndex].Devices.[dIndex].MaxWorkItemDimensions |> int) >= 2 &&
                            OpenCL.OpenCLPlatform.Platforms.[pIndex].Devices.[dIndex].MaxWorkItemSizes.[1] > 1L) then  
                               
                        Console.WriteLine(" Device " + ": " + dName.ToString() + "(" + dType.ToString() + ")")                                    
                        let c = Array.zeroCreate<float32> (cols * rows |> int)
                        let ws = WorkSize([| rows / elementsPerThread; cols / elementsPerThread |], [| blockSize; blockSize |])

                        let comp = <@ DEVICE(pIndex, dIndex,
                                        TransposeFloat4(
                                            BUFFER_WRITE_MODE(wm, 
                                                MEMORY_FLAGS(ofl, 
                                                    AsFloat4(c))),
                                            BUFFER_READ_MODE(rm, 
                                                MEMORY_FLAGS(ifl, 
                                                    AsFloat4(a))),
                                            AsFloat4(block),
                                            ws)) @>   
                                
                        // Extract features
                        if pIndex = 0 && dIndex = 0 && not etOnly then
                            let km = compiler.Compile(comp, opts) :?> IKernelModule
                            let precomputedFeatures = features.BuildFinalizers(km)
                            featureValues.Add(features.EvaluateFinalizers(km, precomputedFeatures, [ AsFloat4(c); AsFloat4(a); AsFloat4(block); ws ]))
                                                                                                                  
                        // Run once to skip compilation time
                        if not featureOnly then
                            runtimeRun(comp) |> ignore
                            let reference = this.CreateVerifiedOutput((a, cols |> int)) :?> float32[]
                            if not (this.Verify(c, reference)) then
                                Console.WriteLine("---------------- COMPUTATION RESULT ERROR")
                            else
                                // Run
                                let watch = new Stopwatch()
                                watch.Start()
                                for i = 0 to iterations - 1 do
                                    runtimeRun(comp) |> ignore
                                watch.Stop()
                                let ttime, iters = ((double)watch.ElapsedMilliseconds) /((double)iterations), iterations
                                        
                                // Dump
                                Console.WriteLine("---------------- " + String.Format("{0,11:######0.0000}", ttime) + "ms (" + String.Format("{0,10:#########0}", iters) + " iterations)")
                                times.Add(ttime |> float32)
                                System.Threading.Thread.Sleep(500)
                                    
            //executionResults.Last().AddRange([ rows; cols ]) 
        (featureValues, executionResults) ||> Seq.zip |> List.ofSeq
        *)