module BinarySearchTrainingSample

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

[<ReflectedDefinition>]
let BinarySearch(outputArray: int4[],
                 sortedArray: int2[], 
                 findMe: int) =
    let tid = get_global_id(0)
    let element = sortedArray.[tid]

    // If the element to be found does not lie between them, then nothing left to do in this thread 
    if((element.x < findMe) && (element.y > findMe)) then
        (* However, if the element does lie between the lower and upper bounds of this thread's searchspace
         * we need to narrow down the search further in this search space 
         *) 
        // The search space for this thread is marked in the output as being the total search space for the next pass 
        outputArray.[0].x <- tid
        outputArray.[0].w <- 1

[<ReflectedDefinition>]
let BinarySearchMulKeys(keys: int[],
                        input: int[],
                        numKeys: int,
                        output: int[]) =
    let gid = get_global_id(0)
    let lBound = gid * 256
    let uBound = lBound + 255
   
    for i = 0 to numKeys - 1 do 
        if keys.[i] >= input.[lBound] && keys.[i] <= input.[uBound] then
            output.[i] <- lBound

[<ReflectedDefinition>]
let BinarySearchMulKeysConcurrent(keys: int[],
                                  input: int[],
                                  inputSize: int,
                                  numSubdivisions: int,
                                  output: int[]) =
    let mutable lBound = (get_global_id(0) % numSubdivisions) * (inputSize / numSubdivisions)
    let mutable uBound = lBound + inputSize / numSubdivisions
    let myKey = keys.[get_global_id(0) / numSubdivisions]
    let mutable mid = 0
    let mutable canReturn = false

    while not canReturn && uBound >= lBound do
        mid <- (lBound + uBound) / 2
        if input.[mid] = myKey then
            output.[get_global_id(0) / numSubdivisions] <- mid
            canReturn <- true
        else if input.[mid] > myKey then
            uBound <- mid - 1;
        else
            lBound <- mid + 1

type BinarySearchTrainingSample() =    
    inherit IDefaultFeatureExtractionTrainingSample()

    override this.DefaultConfigurationDictionary() =
        let dict = new Dictionary<string, obj>()
        dict.Add("MinVectorSize", 2048L)
        dict.Add("MaxVectorSize", 8L <<< 20)
        dict.Add("Iterations", 100)
        dict
        
    override this.Verify(output: obj, reference: obj) =
        (output :?> float32[]) = (reference :?> float32[])
    
    override this.CreateVerifiedOutput(o: obj) =
        box ()

    override this.ResultColumnIDs 
        with get() =   
            let ids = new List<String>()         
            for pIndex, pName, pDevs in GetOpenCLPlatforms() do  
                for dIndex, dName, dType in pDevs do  
                    ids.Add(dName + " Completion Time (ms)")
            ids.Add("Vector Size (elements)")
            ids |> List.ofSeq
    
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
        let ofl = MemoryFlags.HostReadOnly ||| MemoryFlags.UseHostPointer ||| MemoryFlags.WriteOnly

        let mutable execResults: obj list list = []
                
        let size = ref minSize
        let elementIndex = ref 1
        while !size <= maxSize do
            Console.WriteLine("      Size: " + String.Format("{0,10:##########}", !size))
                        
            // Setup local size and global size
            let localThreads = 128L;
            let mutable numSubdivisions = !size / localThreads;
            if numSubdivisions < localThreads then
                numSubdivisions <- localThreads
            let globalThreads = numSubdivisions
                
            // Create input and output
            let input = Array.zeroCreate<int2> (!size |> int)
            let first = rnd.Next(0, 10)
            input.[0] <- int2(rnd.Next(0, 10), first + rnd.Next(0, 10)) 
            for i = 1 to (!size |> int) - 1 do
                let first = input.[i - 1].y + rnd.Next(0, 10)
                input.[i] <- int2(first, first + rnd.Next(0, 10))

            while !elementIndex <= (!size |> int) do
                Console.WriteLine("      Element index: " + String.Format("{0,10:##########}", !elementIndex))

                let mutable features: obj list = []
                let mutable instanceResult: obj list = []
                for pIndex, pName, pDevs in GetOpenCLPlatforms() do   
                    for dIndex, dName, dType in pDevs do                                
                        Console.WriteLine(" Device " + ": " + dName.ToString() + "(" + dType.ToString() + ")")  
                        if dType <> DeviceType.Cpu || ((ifl &&& MemoryFlags.UsePersistentMemAMD |> int = 0) && (ofl &&& MemoryFlags.UsePersistentMemAMD |> int = 0)) then                                    
                           
                            let output = [| int4(0, 0, 1, 0) |]
                            let mutable globalLowerBound = 0
                            let mutable globalUpperBound = (!size |> int) - 1
                            let mutable subdivSize = (globalUpperBound - globalLowerBound + 1) / (numSubdivisions |> int)
                            let iterInput = Array.zeroCreate<int2> (numSubdivisions |> int)
    
                            while subdivSize > 1 && output.[0].w <> 0 do            

                                // Build kernel input
                                for i = 0 to (numSubdivisions |> int) - 1 do
                                    let indexa = i * subdivSize
                                    let indexb = (i + 1) * subdivSize - 1;
                                    iterInput.[i].x <- input.[indexa].x
                                    iterInput.[i].y <- input.[indexb].y
                                
                                // Create computation
                                let comp = <@ DEVICE(pIndex, dIndex,
                                                BinarySearch(
                                                    BUFFER_WRITE_MODE(wm, 
                                                        MEMORY_FLAGS(ofl, 
                                                            output)),
                                                    BUFFER_READ_MODE(rm, 
                                                        MEMORY_FLAGS(ifl, 
                                                            iterInput)),
                                                    input.[!elementIndex].x)) @>   
                                
                                // Extract features
                                let km = compiler.Compile(comp, opts) :?> IKernelModule
                                let precomputedFeatures = chain.Precompute(km)
                                features <- chain.Evaluate(km, precomputedFeatures, [ output; input; input.[!elementIndex].x ], [| globalThreads |], [| localThreads |], opts)
                                     
                                // Run once to skip compilation time
                                comp.Run(globalThreads, localThreads)

                                // Update boundaries
                                globalLowerBound <- output.[0].x * subdivSize
                                globalUpperBound <- globalLowerBound + subdivSize - 1
                                subdivSize <- (globalUpperBound - globalLowerBound + 1) / (numSubdivisions |> int)

                                if not (this.Verify((output.[0].x, !elementIndex))) then
                                    Console.WriteLine("---------------- COMPUTATION RESULT ERROR")
                                else
                                    // Run
                                    let watch = new Stopwatch()
                                    watch.Start()
                                    for i = 0 to iterations - 1 do
                                        comp.Run(!size, 128L)
                                    watch.Stop()
                                    let ttime, iters = ((double)watch.ElapsedMilliseconds) /((double)iterations), iterations
                                        
                                    // Dump
                                    Console.WriteLine("---------------- " + String.Format("{0,11:######0.0000}", ttime) + "ms (" + String.Format("{0,10:#########0}", iters) + " iterations)")
                                    instanceResult <- instanceResult @ [ ttime ]
                                    System.Threading.Thread.Sleep(500)
                                
                            execResults <- execResults @ [ instanceResult @ [!size] @ features ] 
                elementIndex := !elementIndex * 2
            size := !size * 2L   
        execResults