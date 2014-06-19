module ScanTrainingSample

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

(*
 * Work-efficient compute implementation of scan, one thread per 2 elements
 * O(log(n)) stepas and O(n) adds using shared memory
 * Uses a balanced tree algorithm. See Belloch, 1990 "Prefix Sums
 * and Their Applications"
 * @param output    output data 
 * @param input     input data
 * @param block     local memory used in the kernel
 * @param length    lenght of the input data
 * @param idxOffset offset between two consecutive index.
 *)
[<ReflectedDefinition>]
let GroupScan(output: float32[],
              input: float32[],
              [<AddressSpace(AddressSpace.Local)>] block: float32[],
              idxOffset: int) =
    let localId = get_local_id(0)
    let localSize = get_local_size(0)
    let length = input.Length

    // Cache the computational window in shared memory
    let globalIdx = (idxOffset * (2 * (get_global_id(0)) + 1)) - 1
    if (globalIdx < length) then            
        block.[2*localId] <- input.[globalIdx]
    if (globalIdx + idxOffset < length) then 
        block.[2*localId + 1] <- input.[globalIdx + idxOffset]

    // Build up tree 
    let mutable offset = 1
    let mutable l = length >>> 1
    while l > 0 do
        barrier(CLK_LOCAL_MEM_FENCE);
        if (localId < l) then
            let ai = offset*(2*localId + 1) - 1
            let bi = offset*(2*localId + 2) - 1
            block.[bi] <- block.[bi] + block.[ai]
        offset <- offset <<< 1
        l <- l >>> 1

    if (offset < length) then 
        offset <- offset <<< 1

    // Build down tree
    let maxThread = offset >>> 1
    let mutable d = 0
    while d < maxThread do
        d <- d + 1;
        offset <- offset >>> 1
        barrier(CLK_LOCAL_MEM_FENCE)

        if(localId < d) then
            let ai = offset*(localId + 1) - 1
            let bi = ai + (offset >>> 1)
            block.[bi] <- block.[bi] + block.[ai]
        d <- d <<< 1
    
    barrier(CLK_LOCAL_MEM_FENCE)

    // write the results back to global memory
    if (globalIdx < length) then      
        output.[globalIdx] <- block.[2*localId]

    if(globalIdx+idxOffset < length) then 
        output.[globalIdx + idxOffset] <- block.[2*localId + 1]

(*
 * Work-efficient compute implementation of scan, one thread per 2 elements
 * O(log(n)) stepas and O(n) adds using shared memory
 * Uses a balanced tree algorithm. See Belloch, 1990 "Prefix Sums
 * and Their Applications"
 * @param buffer    input/output data 
 * @param offset    Multiple of Offset positions are already updated by group_prefixSum kernel
 * @param length    lenght of the input data
 *)
[<ReflectedDefinition>]
let GlobalScan(buffer: float32[],
               offset: int) =
    let localSize = get_local_size(0)
    let groupIdx  = get_group_id(0)
    let length = buffer.Length

    let sortedLocalBlocks = offset / localSize     // sorted groups per block
    // Map the gids to unsorted local blocks.
    let gidToUnsortedBlocks = groupIdx + (groupIdx / ((offset<<<1) - sortedLocalBlocks) + 1) * sortedLocalBlocks

    // Get the corresponding global index
    let globalIdx = (gidToUnsortedBlocks*localSize + get_local_id(0))
    if (((globalIdx+1) % offset <> 0) && (globalIdx < length)) then
        buffer.[globalIdx] <- buffer.[globalIdx] + buffer.[globalIdx - (globalIdx%offset + 1)]

type ScanTrainingSample() =    
    inherit VectorAddTrainingSample()

    override this.DefaultConfigurationDictionary() =
        let dict = new Dictionary<string, obj>()
        dict.Add("MinVectorSize", 2048L)
        dict.Add("MaxVectorSize", 8L <<< 20)
        dict.Add("Iterations", 100)
        dict

    override this.CreateVerifiedOutput(o: obj) =
        let a = o :?> float32[]
        Array.scan (fun sum el -> sum + el) 1.0f a |> box

    override this.RunInternal(chain, conf) = 
        let configuration = IDefaultFeatureExtractionTrainingSample.ConfigurationToDictionary(conf)
        let minSize = Int64.Parse(configuration.["MinVectorSize"])
        let maxSize = Int64.Parse(configuration.["MaxVectorSize"])
        let iterations = Int32.Parse(configuration.["Iterations"])

        let compiler = new Compiler()
        let opts = new Dictionary<string, obj>()        
        let rnd = System.Random()

        let rm = BufferReadMode.MapBuffer
        let wm = BufferWriteMode.MapBuffer
        let ifl = MemoryFlags.HostWriteOnly ||| MemoryFlags.UseHostPointer ||| MemoryFlags.ReadOnly
        let ofl = MemoryFlags.HostReadOnly ||| MemoryFlags.UseHostPointer

        let mutable execResults: obj list list = []
                
        let size = ref minSize
        while !size <= maxSize do
            Console.WriteLine("      Size: " + String.Format("{0,10:##########}", !size))
                
            let localSize = 64L        
            let input = Array.create (!size |> int) (1.0f)
            let reference = this.CreateVerifiedOutput(input)

            let mutable features: obj list = []
            let mutable instanceResult: obj list = []
            for pIndex, pName, pDevs in GetOpenCLPlatforms() do   
                for dIndex, dName, dType in pDevs do 
                    Console.WriteLine(" Device " + ": " + dName.ToString() + "(" + dType.ToString() + ")")  
                    if dIndex > 0 then
                        let output = Array.zeroCreate<float32> (!size |> int)
                        let local = Array.zeroCreate<float32> (localSize * 2L |> int)

                        let mutable offset = 1
                        while offset < input.Length do
                            if input.Length/offset > 1 then  
                                // Set number of threads needed for group kernel
                                let mutable globalSize = (input.Length + 1) / 2
                                // Set global thread size multiple of local thread size.
                                globalSize <- ((globalSize + (localSize |> int) - 1) / (localSize |> int)) * (localSize |> int)
                                                            
                                // Create computation
                                let inp = if offset = 1 then input else output
                                let inpflags = if offset = 1 then ifl else ofl
                                let groupScan = <@ DEVICE(pIndex, dIndex,
                                                    GroupScan(
                                                        TRANSFER_MODE(
                                                            TransferMode.TransferIfNeeded,
                                                            BUFFER_WRITE_MODE(wm, 
                                                                MEMORY_FLAGS(ofl, 
                                                                    output))),
                                                        BUFFER_READ_MODE(rm, 
                                                            MEMORY_FLAGS(inpflags, 
                                                                inp)),
                                                        local,
                                                        offset)) @>
                                // Extract features (compilation will be done only first time)
                                let km = compiler.Compile(groupScan, opts) :?> IKernelModule
                                let precomputedFeatures = chain.Precompute(km)
                                let additFeatures = chain.Evaluate(km, precomputedFeatures, [ output; inp; local; offset ], [| globalSize |> int64 |], [| localSize |], opts)
                                //features <- List.map2(fun (a:obj) (b:obj) -> box((a :?> float32) + (b :?> float32))) features additFeatures
                                // Run
                                groupScan.Run(globalSize |> int64, localSize)

                            if offset > 1 then   
                                // Set number of threads needed for global_kernel
                                let mutable globalSize = input.Length - offset
                                globalSize <- globalSize - (globalSize / (offset * 2 * (localSize |> int))) * offset
                                // Set global thread size multiple of local thread size.
                                globalSize <- ((globalSize + (localSize |> int) - 1) / (localSize |> int)) * (localSize |> int) 
                                                   
                                // Create computation
                                let inp = if offset = 1 then input else output
                                let globalScan = <@ DEVICE(pIndex, dIndex,
                                                        GlobalScan(
                                                            TRANSFER_MODE(
                                                                TransferMode.TransferIfNeeded,
                                                                BUFFER_WRITE_MODE(wm, 
                                                                    MEMORY_FLAGS(ofl, 
                                                                        output))),                                                        
                                                            offset)) @>

                                // Extract features (compilation will be done only first time)
                                let km = compiler.Compile(globalScan, opts) :?> IKernelModule
                                let precomputedFeatures = chain.Precompute(km)
                                let additFeatures = chain.Evaluate(km, precomputedFeatures, [ output; offset ], [| globalSize |> int64 |], [| localSize |], opts)
                                //features <- List.map2(fun (a:obj) (b:obj) -> box((a :?> float32) + (b :?> float32))) features additFeatures
                                // Run
                                globalScan.Run(globalSize |> int64, localSize)

                            offset <- offset * ((localSize |> int) * 2)
                                                     
                        // Run once to skip compilation time
                        if not (this.Verify(output, reference)) then
                            Console.WriteLine("---------------- COMPUTATION RESULT ERROR")
                        else
                            // Run
                            let watch = new Stopwatch()
                            watch.Start()
                            for i = 0 to iterations - 1 do
                                
                                let mutable offset = 1
                                while offset < input.Length do
                                    if input.Length/offset > 1 then  
                                        // Set number of threads needed for group kernel
                                        let mutable globalSize = (input.Length + 1) / 2
                                        // Set global thread size multiple of local thread size.
                                        globalSize <- ((globalSize + (localSize |> int) - 1) / (localSize |> int)) * (localSize |> int)
                                                                    
                                        // Create computation
                                        let inp = if offset = 1 then input else output
                                        let inpflags = if offset = 1 then ifl else ofl
                                        let groupScan = <@ DEVICE(pIndex, dIndex,
                                                            GroupScan(
                                                                BUFFER_WRITE_MODE(wm, 
                                                                    MEMORY_FLAGS(ofl, 
                                                                        output)),
                                                                BUFFER_READ_MODE(rm, 
                                                                    MEMORY_FLAGS(inpflags, 
                                                                        inp)),
                                                                local,
                                                                offset)) @>
                                        groupScan.Run(globalSize |> int64, localSize)

                                    if offset > 1 then   
                                        // Set number of threads needed for global_kernel
                                        let mutable globalSize = input.Length - offset
                                        globalSize <- globalSize - (globalSize / (offset * 2 * (localSize |> int))) * offset
                                        // Set global thread size multiple of local thread size.
                                        globalSize <- ((globalSize + (localSize |> int) - 1) / (localSize |> int)) * (localSize |> int) 
                                                           
                                        // Create computation
                                        let inp = if offset = 1 then input else output
                                        let globalScan = <@ DEVICE(pIndex, dIndex,
                                                                GlobalScan(
                                                                    BUFFER_WRITE_MODE(wm, 
                                                                        MEMORY_FLAGS(ofl, 
                                                                            output)),                                                        
                                                                    offset)) @>
                                        globalScan.Run(globalSize |> int64, localSize)

                                    offset <- offset * ((localSize |> int) * 2)

                            watch.Stop()
                            let ttime, iters = ((double)watch.ElapsedMilliseconds) /((double)iterations), iterations
                                
                            // Dump
                            Console.WriteLine("---------------- " + String.Format("{0,11:######0.0000}", ttime) + "ms (" + String.Format("{0,10:#########0}", iters) + " iterations)")
                            instanceResult <- instanceResult @ [ ttime ]
                            System.Threading.Thread.Sleep(500)
                                
            execResults <- execResults @ [ instanceResult @ [!size] @ features ]                
            size := !size * 2L   
        execResults