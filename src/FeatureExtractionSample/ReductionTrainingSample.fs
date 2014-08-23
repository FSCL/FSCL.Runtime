module ReductionTrainingSample

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
open VectorAddTrainingSample

[<ReflectedDefinition>]
let placeholderComp a b =
    a + b

[<ReflectedDefinition>]
let SimpleReduction(g_idata:int[], g_odata:int[], block: int, inputSize: int, wi: WorkItemInfo) =
    let mutable global_index = wi.GlobalID(0) * block
    let mutable upper_bound = (wi.GlobalID(0) + 1) * block
    if upper_bound > inputSize then
        upper_bound <- inputSize

    // We don't know which is the neutral value for placeholderComp so we need to
    // initialize it with an element of the input array
    let mutable accumulator = 0

    while global_index < upper_bound do
        accumulator <- placeholderComp accumulator g_idata.[global_index]
        global_index <- global_index + 1

    g_odata.[wi.GlobalID(0)] <- accumulator

[<ReflectedDefinition>]
let AdvancedReduction(g_idata:int[], [<AddressSpace(AddressSpace.Local)>]sdata:int[], g_odata:int[], inputSize: int, wi: WorkItemInfo) =
    let global_index = wi.GlobalID(0)
    let global_size = wi.GlobalSize(0)
    let mutable accumulator = g_idata.[global_index]
    for gi in global_index + global_size .. global_size .. inputSize - 1 do
        accumulator <- placeholderComp accumulator g_idata.[gi]
                                        
    let local_index = wi.LocalID(0)
    sdata.[local_index] <- accumulator
    wi.Barrier(CLK_LOCAL_MEM_FENCE)

    let mutable offset = wi.LocalSize(0) / 2
    while(offset > 0) do
        if(local_index < offset) then
            sdata.[local_index] <- placeholderComp (sdata.[local_index]) (sdata.[local_index + offset])
        offset <- offset / 2
        wi.Barrier(CLK_LOCAL_MEM_FENCE)
                
    if local_index = 0 then
        g_odata.[wi.GroupID(0)] <- sdata.[0]

type SimpleReductionTrainingSample() =    
    inherit IDefaultFeatureExtractionTrainingSample()

    override this.DefaultConfigurationDictionary() =
        let dict = new Dictionary<string, obj>()
        dict.Add("MinVectorSize", 2048L)
        dict.Add("MaxVectorSize", 32L <<< 20)
        dict.Add("MinBlockRatio", 32L)
        dict.Add("MaxBlockRatio", 2L)
        dict.Add("Iterations", 100)
        dict
        
    override this.ResultColumnIDs 
        with get() =   
            let ids = new List<String>()         
            for pIndex, pName, pDevs in GetOpenCLPlatforms() do  
                for dIndex, dName, dType in pDevs do  
                    ids.Add(dName + " Completion Time (ms)")
            ids.Add("Vector Size (elements)")
            ids.Add("Block size (elements)")
            ids.Add("Number of kernel calls")
            ids |> List.ofSeq

    override this.Verify(output: obj, reference: obj) =
        (output :?> int) = (reference :?> int)
    
    override this.CreateVerifiedOutput(o: obj) =
        let a = o :?> int[]
        Array.reduce (fun a b -> placeholderComp a b) a |> box
        
    override this.RunInternal(chain, conf) = 
        let configuration = IDefaultFeatureExtractionTrainingSample.ConfigurationToDictionary(conf)
        let minSize = Int64.Parse(configuration.["MinVectorSize"])
        let maxSize = Int64.Parse(configuration.["MaxVectorSize"])
        let minBlockRatio = Int64.Parse(configuration.["MinBlockRatio"])
        let maxBlockRatio = Int64.Parse(configuration.["MaxBlockRatio"])
        let iterations = Int32.Parse(configuration.["Iterations"])

        let compiler = new Compiler()
        let opts = new Dictionary<string, obj>()    
        // We try to reuse buffer ACROSS different kernel expressions
        opts.Add(RuntimeOptions.BufferPoolPersistency, BufferPoolPersistency.PersistencyAcrossExpressions)   
        // We accept a little change to the opencl memory flags to increase sharing chances 
        opts.Add(RuntimeOptions.BufferSharePriority, BufferSharePriority.PriorityToShare)        
        let rnd = System.Random()

        let rm = BufferReadMode.EnqueueReadBuffer
        let wm = BufferWriteMode.EnqueueWriteBuffer
        let fl = MemoryFlags.UseHostPointer ||| MemoryFlags.ReadWrite

        let mutable execResults: obj list list = []
                
        let size = ref minSize
        while !size <= maxSize do
            Console.WriteLine("      Size: " + String.Format("{0,10:##########}", !size))
                        
            let a = Array.init (!size |> int) (fun i -> 1)
            let reference = this.CreateVerifiedOutput(a)
            let mutable blockSize = (!size / minBlockRatio)
            while blockSize <= (!size / maxBlockRatio) do
                Console.WriteLine("      Block Size: " + String.Format("{0,10:##########}", blockSize))
                
                let mutable kernelCallCount = 0
                let mutable features: obj list = []
                let mutable instanceResult: obj list = []

                for pIndex, pName, pDevs in GetOpenCLPlatforms() do   
                    for dIndex, dName, dType in pDevs do                                
                        Console.WriteLine(" Device " + ": " + dName.ToString() + "(" + dType.ToString() + ")")    
                        Runtime.ForceClearPool(false)                                

                        let c = Array.zeroCreate<int> (!size / blockSize |> int)

                        let mutable currentInputSize = !size
                        let mutable currentOutputSize = !size / blockSize
                        let mutable currentBlockSize = blockSize
                        if currentOutputSize = 0L then
                            currentOutputSize <- 1L

                        let mutable firstIteration = true
                        while currentOutputSize > 0L do
                            let inputTransferMode =
                                if firstIteration then
                                    TransferMode.TransferIfNeeded
                                else
                                    TransferMode.NoTransfer     
                            let outputTransferMode =
                                if currentOutputSize = 1L then
                                    TransferMode.TransferIfNeeded
                                else
                                    TransferMode.NoTransfer     
                            let input = 
                                if firstIteration then
                                    a
                                else
                                    c

                            let ws = WorkSize(currentOutputSize, Math.Min(128L, currentOutputSize))
                            let comp = <@ DEVICE(pIndex, dIndex,
                                            SimpleReduction(
                                                TRANSFER_MODE(inputTransferMode, TransferMode.NoTransfer,
                                                    BUFFER_READ_MODE(rm, 
                                                        MEMORY_FLAGS(fl, 
                                                            input))),
                                                TRANSFER_MODE(TransferMode.NoTransfer, outputTransferMode,
                                                    BUFFER_WRITE_MODE(wm, 
                                                        MEMORY_FLAGS(fl, 
                                                            c))),
                                                currentBlockSize |> int,
                                                currentInputSize |> int,
                                                ws)) @>   
                        
                            // Extract features
                            let km = compiler.Compile(comp, opts) :?> IKernelModule
                            let precomputedFeatures = chain.Precompute(km)
                            let additFeatures = chain.Evaluate(km, precomputedFeatures, [ input; c ; currentBlockSize |> int; currentInputSize |> int; ws], opts)
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
                            comp.Run(opts)
                            if pIndex = 0 && dIndex = 0 then
                                kernelCallCount <- kernelCallCount + 1

                            if currentOutputSize = 1L then
                                currentOutputSize <- 0L
                            else
                                let prevOutputSize = currentOutputSize
                                currentInputSize <- prevOutputSize
                                currentOutputSize <- currentOutputSize / currentBlockSize    
                                if currentOutputSize = 0L then
                                    currentBlockSize <- prevOutputSize
                                    currentOutputSize <- 1L
                            firstIteration <- false
                            
                        if not (this.Verify(c.[0], reference)) then
                            Console.WriteLine("---------------- COMPUTATION RESULT ERROR")
                        else
                            // Force clear buffer pool, otherwise successive iterations reuse buffers
                            Runtime.ForceClearPool(false)
                            
                            let c = Array.zeroCreate<int> (!size / blockSize |> int)

                            // Run                                                  
                            let watch = new Stopwatch()
                            watch.Start()
                            for i = 0 to iterations - 1 do
                                let mutable currentInputSize = !size
                                let mutable currentOutputSize = !size / blockSize
                                let mutable currentBlockSize = blockSize
                                if currentOutputSize = 0L then
                                    currentOutputSize <- 1L

                                let mutable firstIteration = true
                                while currentOutputSize > 0L do
                                    let inputTransferMode =
                                        if firstIteration then
                                            TransferMode.TransferIfNeeded
                                        else
                                            TransferMode.NoTransfer     
                                    let outputTransferMode =
                                        if currentOutputSize = 1L then
                                            TransferMode.TransferIfNeeded
                                        else
                                            TransferMode.NoTransfer     
                                    let input = 
                                        if firstIteration then
                                            a
                                        else
                                            c

                                    let ws = WorkSize(currentOutputSize, Math.Min(128L, currentOutputSize))
                                    let comp = <@ DEVICE(pIndex, dIndex,
                                                    SimpleReduction(
                                                        TRANSFER_MODE(inputTransferMode, TransferMode.NoTransfer,
                                                            BUFFER_READ_MODE(rm, 
                                                                MEMORY_FLAGS(fl, 
                                                                    input))),
                                                        TRANSFER_MODE(TransferMode.NoTransfer, outputTransferMode,
                                                            BUFFER_WRITE_MODE(wm, 
                                                                MEMORY_FLAGS(fl, 
                                                                    c))),
                                                        currentBlockSize |> int,
                                                        currentInputSize |> int,
                                                        ws)) @>   
                           
                                    // Run           
                                    comp.Run(opts)

                                    if currentOutputSize = 1L then
                                        currentOutputSize <- 0L
                                    else
                                        let prevOutputSize = currentOutputSize
                                        currentInputSize <- prevOutputSize
                                        currentOutputSize <- currentOutputSize / currentBlockSize    
                                        if currentOutputSize = 0L then
                                            currentBlockSize <- prevOutputSize
                                            currentOutputSize <- 1L
                                    firstIteration <- false
                            watch.Stop()
                            let ttime, iters = ((double)watch.ElapsedMilliseconds) /((double)iterations), iterations
                                
                            // Dump
                            Console.WriteLine("---------------- " + String.Format("{0,11:######0.0000}", ttime) + "ms (" + String.Format("{0,10:#########0}", iters) + " iterations)")
                            instanceResult <- instanceResult @ [ ttime ]
                            System.Threading.Thread.Sleep(500)
                                
                execResults <- execResults @ [ instanceResult @ [!size; blockSize; kernelCallCount] @ features ]  
                blockSize <- blockSize * 2L
                              
            size := !size * 2L   
        execResults

        
type AdvancedReductionTrainingSample() =    
    inherit SimpleReductionTrainingSample()

    override this.DefaultConfigurationDictionary() =
        let dict = new Dictionary<string, obj>()
        dict.Add("MinVectorSize", 2048L)
        dict.Add("MaxVectorSize", 32L <<< 20)
        dict.Add("Iterations", 100)
        dict
        
    override this.ResultColumnIDs 
        with get() =   
            let ids = new List<String>()         
            for pIndex, pName, pDevs in GetOpenCLPlatforms() do  
                for dIndex, dName, dType in pDevs do  
                    ids.Add(dName + " Completion Time (ms)")
            ids.Add("Vector Size (elements)")
            ids.Add("Number of kernel calls")
            ids |> List.ofSeq

    override this.RunInternal(chain, conf) = 
        let configuration = IDefaultFeatureExtractionTrainingSample.ConfigurationToDictionary(conf)
        let minSize = Int64.Parse(configuration.["MinVectorSize"])
        let maxSize = Int64.Parse(configuration.["MaxVectorSize"])
        let iterations = Int32.Parse(configuration.["Iterations"])

        let compiler = new Compiler()
        let opts = new Dictionary<string, obj>()    
        // We try to reuse buffer ACROSS different kernel expressions
        //opts.Add(RuntimeOptions.BufferPoolPersistency, BufferPoolPersistency.PersistencyAcrossExpressions)   
        // We accept a little change to the opencl memory flags to increase sharing chances 
        opts.Add(RuntimeOptions.BufferSharePriority, BufferSharePriority.PriorityToShare)        
        let rnd = System.Random()

        let rm = BufferReadMode.MapBuffer
        let wm = BufferWriteMode.MapBuffer
        let fl = MemoryFlags.UseHostPointer ||| MemoryFlags.ReadWrite

        let mutable execResults: obj list list = []
                
        let size = ref minSize
        while !size <= maxSize do
            Console.WriteLine("      Size: " + String.Format("{0,10:##########}", !size))
                        
            let a = Array.init (!size |> int) (fun i -> 1)
            let reference = this.CreateVerifiedOutput(a)      
            let localSize = 128                  
           
            let mutable kernelCallCount = 0
            let mutable features: obj list = []
            let mutable instanceResult: obj list = []
            for pIndex, pName, pDevs in GetOpenCLPlatforms() do   
                for dIndex, dName, dType in pDevs do                                
                    Console.WriteLine(" Device " + ": " + dName.ToString() + "(" + dType.ToString() + ")")   
                    
                    Runtime.ForceClearPool(false)                                 
                               
                    let c = Array.zeroCreate<int> (a.Length / 2)
                    let mutable currentDataSize = a.Length
                    let mutable currentGlobalSize = currentDataSize / 2
                    let mutable currentLocalSize = if localSize > currentGlobalSize then currentGlobalSize else localSize
                    
                    let mutable firstIteration = true
                    // Execute until the output size is smaller than group size * number of compute units (full utilization of pipeline)
                    while (currentDataSize > 1) do     
                        let mutable localArray = Array.zeroCreate<int> currentLocalSize 
                        
                        let inputTransferMode =
                            if firstIteration then
                                TransferMode.TransferIfNeeded
                            else
                                TransferMode.NoTransfer
                        let outputTransferMode =
                            if currentGlobalSize = currentLocalSize then
                                // Last iteration, read back
                                TransferMode.NoTransfer 
                            else
                               TransferMode.TransferIfNeeded     
                        let input = 
                            if firstIteration then
                                a
                            else
                                c           
                                       
                        let ws = WorkSize(currentGlobalSize |> int64, currentLocalSize |> int64)
                        let comp = <@ DEVICE(pIndex, dIndex,
                                        AdvancedReduction(
                                            TRANSFER_MODE(inputTransferMode, TransferMode.NoTransfer,
                                                BUFFER_READ_MODE(rm, 
                                                    MEMORY_FLAGS(fl, 
                                                        input))),
                                            localArray,
                                            TRANSFER_MODE(TransferMode.NoTransfer, outputTransferMode,
                                                BUFFER_WRITE_MODE(wm, 
                                                    MEMORY_FLAGS(fl, 
                                                        c))),
                                            currentDataSize,
                                            ws)) @>   
                        // Extract features
                        let km = compiler.Compile(comp, opts) :?> IKernelModule
                        let precomputedFeatures = chain.Precompute(km)
                        let additFeatures = chain.Evaluate(km, precomputedFeatures, [ input; localArray; c; currentDataSize; ws], opts)
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
                        comp.Run(opts)
                        if pIndex = 0 && dIndex = 0 then
                            kernelCallCount <- kernelCallCount + 1
                        
                        // If local size become greater than or equal to global size, we set it to be half the global size
                        currentDataSize <- currentGlobalSize / currentLocalSize
                        if currentLocalSize >= (currentDataSize / 2) then
                            currentLocalSize <- currentDataSize / 2
                        currentGlobalSize <- currentDataSize / 2
                        
                        firstIteration <- false
   
                    if not (this.Verify(c.[0], reference)) then
                        Console.WriteLine("---------------- COMPUTATION RESULT ERROR")
                    else
                        // Force clear buffer pool, otherwise successive iterations reuse buffers
                        Runtime.ForceClearPool(false)
                                                
                        // Run                                 
                        let c = Array.zeroCreate<int> (a.Length / 2)
                                                     
                        let watch = new Stopwatch()
                        watch.Start()
                        for i = 0 to iterations - 1 do                
                            let mutable currentDataSize = a.Length
                            let mutable currentGlobalSize = currentDataSize / 2
                            let mutable currentLocalSize = if localSize > currentGlobalSize then currentGlobalSize else localSize
                    
                            let mutable firstIteration = true
                            // Execute until the output size is smaller than group size * number of compute units (full utilization of pipeline)
                            while (currentDataSize > 1) do     
                                let mutable localArray = Array.zeroCreate<int> currentLocalSize 
                        
                                let inputTransferMode =
                                    if firstIteration then
                                        TransferMode.TransferIfNeeded
                                    else
                                        TransferMode.NoTransfer
                                let outputTransferMode =
                                    if currentGlobalSize = currentLocalSize then
                                        // Last iteration, read back
                                        TransferMode.NoTransfer 
                                    else
                                       TransferMode.TransferIfNeeded     
                                let input = 
                                    if firstIteration then
                                        a
                                    else
                                        c           
                                       
                                let ws = WorkSize(currentGlobalSize |> int64, currentLocalSize |> int64)
                                let comp = <@ DEVICE(pIndex, dIndex,
                                                AdvancedReduction(
                                                    TRANSFER_MODE(inputTransferMode, TransferMode.NoTransfer,
                                                        BUFFER_READ_MODE(rm, 
                                                            MEMORY_FLAGS(fl, 
                                                                input))),
                                                    localArray,
                                                    TRANSFER_MODE(TransferMode.NoTransfer, outputTransferMode,
                                                        BUFFER_WRITE_MODE(wm, 
                                                            MEMORY_FLAGS(fl, 
                                                                c))),
                                                    currentDataSize,
                                                    ws)) @>   

                                // Run           
                                comp.Run(opts)
                        
                                // If local size become greater than or equal to global size, we set it to be half the global size
                                currentDataSize <- currentGlobalSize / currentLocalSize
                                if currentLocalSize >= (currentDataSize / 2) then
                                    currentLocalSize <- currentDataSize / 2
                                currentGlobalSize <- currentDataSize / 2
                        
                                firstIteration <- false
                                   
                            Runtime.ForceClearPool(false)
                        watch.Stop()

                        let ttime, iters = ((double)watch.ElapsedMilliseconds) /((double)iterations), iterations
                                
                        // Dump
                        Console.WriteLine("---------------- " + String.Format("{0,11:######0.0000}", ttime) + "ms (" + String.Format("{0,10:#########0}", iters) + " iterations)")
                        instanceResult <- instanceResult @ [ ttime ]
                        System.Threading.Thread.Sleep(500)
                                
            execResults <- execResults @ [ instanceResult @ [!size; kernelCallCount] @ features ]  
            size := !size * 2L   
        execResults

