module PrefixSumTrainingSample

open FSCL
open FSCL.Compiler
open FSCL.Runtime.Scheduling
open FSCL.Runtime.Scheduling.FeatureExtraction
open FSCL.Language
open System.Collections.Generic
open System
open System.IO
open FSCL.Runtime
open FSCL.Runtime.Language
open System.Diagnostics
open VectorAddTrainingSample

(*/------------------------------------------------------------
// Purpose :
// ---------
// Prefix sum or prefix scan is an operation where each output element contains the sum of all input elements preceding it.
//
// Algorithm :
// -----------
// The parallel prefix sum has two principal parts, the reduce phase (also known as the up-sweep phase) and the down-sweep phase.
//
// In the up-sweep reduction phase we traverse the computation tree from bottom to top, computing partial sums.
// After this phase, the last element of the array contains the total sum.
//
// During the down-sweep phase, we traverse the tree from the root and use the partial sums to build the scan in place.
//
// Because the scan pictured is an exclusive sum, a zero is inserted into the last element before the start of the down-sweep phase.
// This zero is then propagated back to the first element.
//
// In our implementation, each compute unit loads and sums up two elements (for the deepest depth). Each subsequent depth during the up-sweep
// phase is processed by half of the compute units from the deeper level and the other way around for the down-sweep phase.
//
// In order to be able to scan large arrays, i.e. arrays that have many more elements than the maximum size of a work-group, the prefix sum has to be decomposed.
// Each work-group computes the prefix scan of its sub-range and outputs a single number representing the sum of all elements in its sub-range.
// The workgroup sums are scanned using exactly the same algorithm.
// When the number of work-group results reaches the size of a work-group, the process is reversed and the work-group sums are
// propagated to the sub-ranges, where each work-group adds the incoming sum to all its elements, thus producing the final scanned array.
//
// References :
// ------------
// NVIDIA Mark Harris. Parallel prefix sum (scan) with CUDA. April 2007
// http://developer.download.nvidia.com/compute/cuda/1_1/Website/projects/scan/doc/scan.pdf
// http://graphics.idav.ucdavis.edu/publications/print_pub?pub_id=915
//
// Other references :
// ------------------
// http://developer.nvidia.com/node/57
//------------------------------------------------------------

#pragma OPENCL EXTENSION cl_amd_printf : enable
#define T int
//#define SUPPORT_AVOID_BANK_CONFLICT

//------------------------------------------------------------
// kernel__ExclusivePrefixScanSmall
//
// Purpose : do a fast scan on a small chunck of data.
//------------------------------------------------------------
*)
[<ReflectedDefinition>]
let ExclusivePrefixScanSmall(input: float32[],
                             output: float32[],
                             [<AddressSpace(AddressSpace.Local)>] block: float32[],
                             length: int) =
    let tid = get_local_id(0)    
    let mutable offset = 1

    // Cache the computational window in shared memory
    block.[2*tid]     <- input.[2*tid]
    block.[2*tid + 1] <- input.[2*tid + 1]

    // Build the sum in place up the tree
    let mutable d = length >>> 1
    while(d > 0) do
        barrier(CLK_LOCAL_MEM_FENCE)
        
        if(tid<d) then
            let ai = offset*(2*tid + 1) - 1
            let bi = offset*(2*tid + 2) - 1

            block.[bi] <- block.[bi] + block.[ai]
        offset <- offset * 2
        d <- d >>> 1

    // Clear the last element
    if(tid = 0) then
        block.[length - 1] <- 0.0f

    // traverse down the tree building the scan in the place
    d <- 1
    while d < length do
        offset <- offset >>> 1
        barrier(CLK_LOCAL_MEM_FENCE)
        
        if(tid < d) then
            let ai = offset*(2*tid + 1) - 1
            let bi = offset*(2*tid + 2) - 1
            
            let t = block.[ai]
            block.[ai] <- block.[bi]
            block.[bi] <- block.[bi] + t
        d <- d * 2
    
    barrier(CLK_LOCAL_MEM_FENCE)

    // write the results back to global memory
    output.[2*tid]     <- block.[2*tid]
    output.[2*tid + 1] <- block.[2*tid + 1]


(*
//------------------------------------------------------------
// kernel__ExclusivePrefixScan
//
// Purpose : do a scan on a chunck of data.
//------------------------------------------------------------

// Define this to more rigorously avoid bank conflicts, even at the lower (root) levels of the tree.
// To avoid bank conflicts during the tree traversal, we need to add padding to the shared memory array every NUM_BANKS (16) elements.
// Note that due to the higher addressing overhead, performance is lower with ZERO_BANK_CONFLICTS enabled.
// It is provided as an example.
//#define ZERO_BANK_CONFLICTS 

// 16 banks on G80
#define NUM_BANKS 16
#define LOG_NUM_BANKS 4

#ifdef ZERO_BANK_CONFLICTS
#define CONFLICT_FREE_OFFSET(index) ((index) >> LOG_NUM_BANKS + (index) >> (2*LOG_NUM_BANKS))
#else
#define CONFLICT_FREE_OFFSET(index) ((index) >> LOG_NUM_BANKS)
#endif

//#define CONFLICT_FREE_OFFSET(index) 0
*)
[<ReflectedDefinition>]
let ExclusivePrefixScan(input: float32[],
                        [<AddressSpace(AddressSpace.Local)>] localBuffer: float32[],
                        blockSums: float32[],
                        blockSumsSize: int) =
    let gid = get_global_id(0)
    let tid = get_local_id(0)
    let bid = get_group_id(0)
    let lwz  = get_local_size(0)
    
    // The local buffer has 2x the size of the local-work-size, because we manage 2 scans at a time.
    let localBufferSize = lwz <<< 1
    let mutable offset = 1
    
    let tid2_0 = tid <<< 1
    let tid2_1 = tid2_0 + 1
    
    let gid2_0 = gid <<< 1
    let gid2_1 = gid2_0 + 1

    // Cache the datas in local memory
    if gid2_0 < blockSumsSize then
        localBuffer.[tid2_0] <- input.[gid2_0]
    else
        localBuffer.[tid2_0] <- 0.0f
    if gid2_1 < blockSumsSize then
        localBuffer.[tid2_1] <- input.[gid2_1] 
    else
        localBuffer.[tid2_1] <- 0.0f

    // bottom-up
    let mutable d = lwz
    while d > 0 do
        barrier(CLK_LOCAL_MEM_FENCE)        
        if (tid < d) then
            let ai = mad24(offset, (tid2_1+0), -1)  // offset*(tid2_0+1)-1 = offset*(tid2_1+0)-1
            let bi = mad24(offset, (tid2_1+1), -1);  // offset*(tid2_1+1)-1;
            localBuffer.[bi] <- localBuffer.[bi] + localBuffer.[ai]
        
        offset <- offset <<< 1
        d <- d >>> 1
    barrier(CLK_LOCAL_MEM_FENCE)

    if (tid < 1) then
        // We store the biggest value (the last) to the sum-block for later use.
        blockSums.[bid] <- localBuffer.[localBufferSize-1]        
        //barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);      
        // Clear the last element
        localBuffer.[localBufferSize - 1] <- 0.0f

    // top-down
    d <- 1
    while d < localBufferSize do
        offset <- offset >>> 1
        barrier(CLK_LOCAL_MEM_FENCE)
        
        if (tid < d) then
            let ai = mad24(offset, (tid2_1+0), -1) // offset*(tid2_0+1)-1 = offset*(tid2_1+0)-1
            let bi = mad24(offset, (tid2_1+1), -1) // offset*(tid2_1+1)-1;

            let tmp = localBuffer.[ai]
            localBuffer.[ai] <- localBuffer.[bi]
            localBuffer.[bi] <- localBuffer.[bi] + tmp
        d <- d <<< 1

    barrier(CLK_LOCAL_MEM_FENCE)

    // Copy back from the local buffer to the output array
    if (gid2_0 < blockSumsSize) then
        input.[gid2_0] <- localBuffer.[tid2_0]
    if (gid2_1 < blockSumsSize) then
        input.[gid2_1] <- localBuffer.[tid2_1]

(*
//------------------------------------------------------------
// kernel__ExclusivePrefixScan
//
// Purpose :
// Final step of large-array scan: combine basic inclusive scan with exclusive scan of top elements of input arrays.
//------------------------------------------------------------
*)
[<ReflectedDefinition>]
let UniformAdd(output: float32[],
               blockSums: float32[],
               outputSize: int) =

    let mutable gid = get_global_id(0) * 2
    let tid = get_local_id(0)
    let blockId = get_group_id(0)

    let localBuffer = local(Array.zeroCreate<float32> 1)

    if (tid < 1) then
        localBuffer.[0] <- blockSums.[blockId]
    barrier(CLK_LOCAL_MEM_FENCE)

    if (gid < outputSize) then
        output.[gid] <- output.[gid] + localBuffer.[0]
    gid <- gid + 1
    if (gid < outputSize) then
        output.[gid] <- output.[gid] + localBuffer.[0]

type PrefixSumTrainingSample() =    
    inherit VectorAddTrainingSample()

    override this.DefaultConfigurationDictionary() =
        let dict = new Dictionary<string, obj>()
        dict.Add("MinVectorSize", 2048L)
        dict.Add("MaxVectorSize", 8L <<< 20)
        dict.Add("Iterations", 100)
        dict

    override this.CreateVerifiedOutput(o: obj) =
        let a = o :?> float32[]
        Array.scan (fun sum el -> sum + el) 0.0f a |> box

    override this.RunInternal(chain, conf) = 
        let configuration = IDefaultFeatureExtractionTrainingSample.ConfigurationToDictionary(conf)
        let minSize = Int64.Parse(configuration.["MinVectorSize"])
        let maxSize = Int64.Parse(configuration.["MaxVectorSize"])
        let iterations = Int32.Parse(configuration.["Iterations"])

        let compiler = new Compiler()
        let opts = new Dictionary<string, obj>() 
        // We try to reuse buffer ACROSS different kernel expressions
        opts.Add(RuntimeOptions.BufferPoolPersistency, BufferPoolPersistency.PersistencyAcrossExpressions)   
        // We accept a little change to the opencl memory flags to increase sharing chances 
        opts.Add(RuntimeOptions.BufferSharePriority, BufferSharePriority.PriorityToShare)        
        let rnd = System.Random()

        let rm = BufferReadMode.MapBuffer
        let wm = BufferWriteMode.MapBuffer
        let ifl = MemoryFlags.UseHostPointer
        let ofl = MemoryFlags.UseHostPointer
        let blockFlags = MemoryFlags.ReadWrite

        let mutable execResults: obj list list = []
                
        let size = ref minSize
        while !size <= maxSize do
            Console.WriteLine("      Size: " + String.Format("{0,10:##########}", !size))
                
            let localSize = 128L        
            let input = Array.create (!size |> int) (1.0f)
            let reference = this.CreateVerifiedOutput(input)

            let mutable features: obj list = []
            let mutable instanceResult: obj list = []
            for pIndex, pName, pDevs in GetOpenCLPlatforms() do   
                for dIndex, dName, dType in pDevs do 
                    Console.WriteLine(" Device " + ": " + dName.ToString() + "(" + dType.ToString() + ")")  
                    if dIndex > 0 then
                        let data = Array.init (!size |> int) (fun i -> input.[i])
                        let local = Array.zeroCreate<float32> (localSize |> int)

                        // Compute the number of buffers we need for the scan
                        let mutable pass = 0
                        let mutable n = !size |> int
                        while n > 1 do
                            n <- (n + (localSize |> int) - 1) / (localSize |> int)
                            pass <- pass + 1
                        let clBufferBlockSums = Array.create pass [| 0.0f |]
                        let blockSumsSizes = Array.create (pass + 1) 0
                        n <- !size |> int
                        for i = 0 to pass - 1 do
                            blockSumsSizes.[i] <- n
                            clBufferBlockSums.[i] <- Array.zeroCreate<float32> n
                            n <- (n + (localSize |> int) - 1) / (localSize |> int)
                        blockSumsSizes.[pass] <- n

                        // Execute scan passes
                        let mutable passInput = data 
                        for i = 0 to pass - 1 do
                            let globalSize = Math.Ceiling(((blockSumsSizes.[i] |> float) / 2.0) / ((localSize |> float) / 2.0)) * ((localSize |> float) / 2.0) |> int
                            let lSize = (localSize |> int) / 2
                            // We transfer input the first time, but we never transfer back
                            let inputTransferMode = if i = 0 then 
                                                        TransferMode.NoTransferBack 
                                                    else 
                                                        TransferMode.NoTransfer ||| TransferMode.NoTransfer
                            let inputFlags = if i = 0 then
                                                ifl
                                             else
                                                MemoryFlags.Auto
                            // The block sums should neve be transferred back
                            let blockTransferMode = TransferMode.NoTransfer ||| TransferMode.NoTransferBack 
                            let scan = <@ DEVICE(pIndex, dIndex,
                                            ExclusivePrefixScan(  
                                                TRANSFER_MODE(inputTransferMode,                                                      
                                                    BUFFER_READ_MODE(rm, 
                                                        MEMORY_FLAGS(inputFlags, 
                                                            passInput))),
                                                local,
                                                TRANSFER_MODE(blockTransferMode,
                                                    BUFFER_WRITE_MODE(wm, 
                                                        MEMORY_FLAGS(blockFlags, 
                                                            clBufferBlockSums.[i]))),
                                                blockSumsSizes.[i])) @>
                            // Extract features (compilation will be done only first time)
                            let km = compiler.Compile(scan, opts) :?> IKernelModule
                            let precomputedFeatures = chain.Precompute(km)
                            let additFeatures = chain.Evaluate(km, precomputedFeatures, [ passInput; local; clBufferBlockSums.[i]; blockSumsSizes.[i] ], [| globalSize |> int64 |], [| lSize |> int64 |], opts)
                            if features.IsEmpty then
                                features <- additFeatures
                            else
                                features <- List.map2(fun (a:obj) (b:obj) -> 
                                                        match a with
                                                        | :? int ->
                                                            box((a :?> int) + (b :?> int))
                                                        | :? int64 ->
                                                            box((a :?> int64) + (b :?> int64))
                                                        | :? float32 ->
                                                            box((a :?> float32) + (b :?> float32))
                                                        | _ ->
                                                            box((a :?> float) + (b :?> float))) features additFeatures
                            // Run

                            scan.Run(globalSize |> int64, lSize |> int64, opts)
                            passInput <- clBufferBlockSums.[i]
                        
                        // Uniform addition
                        let mutable i = pass - 2
                        for i = pass - 2 downto 0 do
                            let globalSize = Math.Ceiling(((blockSumsSizes.[i] |> float) / 2.0) / ((localSize |> float) / 2.0)) * ((localSize |> float) / 2.0) |> int
                            let lSize = (localSize |> int) / 2
                            let passOutput = if i > 0 then clBufferBlockSums.[i - 1] else data

                            // We should transfer back the output only the last iteration
                            let outputTransferMode = if i > 0 then 
                                                        TransferMode.NoTransferBack 
                                                     else 
                                                        TransferMode.TransferIfNeeded
                            // The block sums should never be transferred (they are already on device cause the preceding kernel)
                            let blockTransferMode = TransferMode.NoTransfer ||| TransferMode.NoTransferBack 
                            let unifa = <@ DEVICE(pIndex, dIndex,
                                            UniformAdd(
                                                TRANSFER_MODE(outputTransferMode,
                                                    BUFFER_WRITE_MODE(wm, 
                                                        MEMORY_FLAGS(ofl, 
                                                            passOutput))),
                                                TRANSFER_MODE(blockTransferMode,
                                                    BUFFER_READ_MODE(rm, 
                                                        MEMORY_FLAGS(ifl, 
                                                            clBufferBlockSums.[i]))),
                                                blockSumsSizes.[i])) @>
                            // Extract features (compilation will be done only first time)
                            let km = compiler.Compile(unifa, opts) :?> IKernelModule
                            let precomputedFeatures = chain.Precompute(km)
                            let additFeatures = chain.Evaluate(km, precomputedFeatures, [ passOutput; clBufferBlockSums.[i]; blockSumsSizes.[i] ], [| globalSize |> int64 |], [| lSize |> int64 |], opts)
                            features <- List.map2(fun (a:obj) (b:obj) -> 
                                                    match a with
                                                    | :? int ->
                                                        box((a :?> int) + (b :?> int))
                                                    | :? int64 ->
                                                        box((a :?> int64) + (b :?> int64))
                                                    | :? float32 ->
                                                        box((a :?> float32) + (b :?> float32))
                                                    | _ ->
                                                        box((a :?> float) + (b :?> float))) features additFeatures
                            // Run
                            unifa.Run(globalSize |> int64, lSize |> int64, opts)
                              
                        // Run once to skip compilation time
                        if not (this.Verify(data, reference)) then
                            Console.WriteLine("---------------- COMPUTATION RESULT ERROR")
                        else
                            // Run
                            let watch = new Stopwatch()
                            watch.Start()  

                            let data = input 
                            let local = Array.zeroCreate<float32> (localSize |> int)
                            for i = 0 to iterations - 1 do           
                                // Compute the number of buffers we need for the scan
                                let mutable pass = 0
                                let mutable n = !size |> int
                                while n > 1 do
                                    n <- (n + (localSize |> int) - 1) / (localSize |> int)
                                    pass <- pass + 1
                                let clBufferBlockSums = Array.create pass [| 0.0f |]
                                let blockSumsSizes = Array.create (pass + 1) 0
                                n <- !size |> int
                                for i = 0 to pass - 1 do
                                    blockSumsSizes.[i] <- n
                                    clBufferBlockSums.[i] <- Array.zeroCreate<float32> n
                                    n <- (n + (localSize |> int) - 1) / (localSize |> int)
                                blockSumsSizes.[pass] <- n

                                // Execute scan passes
                                let mutable passInput = data 
                                for i = 0 to pass - 1 do
                                    let globalSize = Math.Ceiling(((blockSumsSizes.[i] |> float) / 2.0) / ((localSize |> float) / 2.0)) * ((localSize |> float) / 2.0) |> int
                                    let lSize = (localSize |> int) / 2
                                    // We transfer input the first time, but we never transfer back
                                    let inputTransferMode = if i = 0 then 
                                                                TransferMode.NoTransferBack 
                                                            else 
                                                                TransferMode.NoTransfer ||| TransferMode.NoTransfer
                                    let inputFlags = if i = 0 then
                                                        ifl
                                                     else
                                                        MemoryFlags.Auto
                                    // The block sums should neve be transferred back
                                    let blockTransferMode = TransferMode.NoTransfer ||| TransferMode.NoTransferBack 
                                    let scan = <@ DEVICE(pIndex, dIndex,
                                                    ExclusivePrefixScan(  
                                                        TRANSFER_MODE(inputTransferMode,                                                      
                                                            BUFFER_READ_MODE(rm, 
                                                                MEMORY_FLAGS(inputFlags, 
                                                                    passInput))),
                                                        local,
                                                        TRANSFER_MODE(blockTransferMode,
                                                            BUFFER_WRITE_MODE(wm, 
                                                                MEMORY_FLAGS(blockFlags, 
                                                                    clBufferBlockSums.[i]))),
                                                        blockSumsSizes.[i])) @>
                                    scan.Run(globalSize |> int64, lSize |> int64, opts)
                                    passInput <- clBufferBlockSums.[i]                                
                                // Uniform addition
                                let mutable i = pass - 2
                                for i = pass - 2 downto 0 do
                                    let globalSize = Math.Ceiling(((blockSumsSizes.[i] |> float) / 2.0) / ((localSize |> float) / 2.0)) * ((localSize |> float) / 2.0) |> int
                                    let lSize = (localSize |> int) / 2
                                    let passOutput = if i > 0 then clBufferBlockSums.[i - 1] else data

                                    // We should transfer back the output only the last iteration
                                    let outputTransferMode = if i > 0 then 
                                                                TransferMode.NoTransferBack 
                                                             else 
                                                                TransferMode.TransferIfNeeded
                                    // The block sums should never be transferred (they are already on device cause the preceding kernel)
                                    let blockTransferMode = TransferMode.NoTransfer ||| TransferMode.NoTransferBack 
                                    let unifa = <@ DEVICE(pIndex, dIndex,
                                                    UniformAdd(
                                                        TRANSFER_MODE(outputTransferMode,
                                                            BUFFER_WRITE_MODE(wm, 
                                                                MEMORY_FLAGS(ofl, 
                                                                    passOutput))),
                                                        TRANSFER_MODE(blockTransferMode,
                                                            BUFFER_READ_MODE(rm, 
                                                                MEMORY_FLAGS(ifl, 
                                                                    clBufferBlockSums.[i]))),
                                                        blockSumsSizes.[i])) @>
                                    // Run
                                    unifa.Run(globalSize |> int64, lSize |> int64, opts)

                                // Force clear buffer pool, otherwise successive iterations reuse buffers
                                Runtime.ForceClearPool(false)

                            watch.Stop()
                            let ttime, iters = ((double)watch.ElapsedMilliseconds) /((double)iterations), iterations
                                
                            // Dump
                            Console.WriteLine("---------------- " + String.Format("{0,11:######0.0000}", ttime) + "ms (" + String.Format("{0,10:#########0}", iters) + " iterations)")
                            instanceResult <- instanceResult @ [ ttime ]
                            System.Threading.Thread.Sleep(500)

                        execResults <- execResults @ [ instanceResult @ [!size] @ features ]                
                        size := !size * 2L   
        execResults