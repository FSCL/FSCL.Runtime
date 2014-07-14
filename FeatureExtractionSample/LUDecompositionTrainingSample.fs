module LUDecompositionTrainingSample

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
open OpenCL

[<ReflectedDefinition>]
let VECTOR_SIZE = 4

[<ReflectedDefinition>]
let LUDecompose(lMatrix: float4[],
                inplaceMatrix: float4[],
                d: int,
                [<AddressSpace(AddressSpace.Local)>]
                ratio: float32[]) =
    //get the global id of the work item
    let y = get_global_id(1)
    let x = get_global_id(0)
    let lidx = get_local_id(0)
    let lidy = get_local_id(1)
    //the range in x axis is dimension / 4
    let xdimension = get_global_size(0) + d / VECTOR_SIZE
    let D = d % VECTOR_SIZE;
    //printf(" Thread ID %d %d local ID %d  %d\n",x,y,lidx,lidy);
    if (get_local_id(0) = 0) then
        //ratio needs to be calculated only once per workitem
        if (D = 0) then (ratio.[lidy] <- inplaceMatrix.[ y * xdimension + d / VECTOR_SIZE].x / inplaceMatrix.[ d * xdimension + d / VECTOR_SIZE].x) else ratio.[lidy] <- 1.0f
        if (D = 1) then (ratio.[lidy] <- inplaceMatrix.[ y * xdimension + d / VECTOR_SIZE].y / inplaceMatrix.[ d * xdimension + d / VECTOR_SIZE].y) else ratio.[lidy] <- 1.0f
        if (D = 2) then (ratio.[lidy] <- inplaceMatrix.[ y * xdimension + d / VECTOR_SIZE].w / inplaceMatrix.[ d * xdimension + d / VECTOR_SIZE].w) else ratio.[lidy] <- 1.0f
        if (D = 3) then (ratio.[lidy] <- inplaceMatrix.[ y * xdimension + d / VECTOR_SIZE].z / inplaceMatrix.[ d * xdimension + d / VECTOR_SIZE].z) else ratio.[lidy] <- 1.0f
         
    barrier(CLK_LOCAL_MEM_FENCE)
     
    //check which workitems need to be included for computation
    if (y >= d + 1 && ((x + 1) * VECTOR_SIZE) > d) then
        let mutable result = float4(0.0f)
         
        //the vectorized part begins here
        result.x <- inplaceMatrix.[y * xdimension + x].x - ratio.[lidy] * inplaceMatrix.[ d * xdimension + x].x
        result.y <- inplaceMatrix.[y * xdimension + x].y - ratio.[lidy] * inplaceMatrix.[ d * xdimension + x].y
        result.z <- inplaceMatrix.[y * xdimension + x].w - ratio.[lidy] * inplaceMatrix.[ d * xdimension + x].w
        result.w <- inplaceMatrix.[y * xdimension + x].z - ratio.[lidy] * inplaceMatrix.[ d * xdimension + x].z
                  
        if (x = d / VECTOR_SIZE) then
            if (D = 0) then (lMatrix.[y * xdimension + x].x <- ratio.[lidy]) else (inplaceMatrix.[y * xdimension + x].x <- result.x)
            if (D = 1) then (lMatrix.[y * xdimension + x].y <- ratio.[lidy]) else (inplaceMatrix.[y * xdimension + x].y <- result.y)
            if (D = 2) then (lMatrix.[y * xdimension + x].w <- ratio.[lidy]) else (inplaceMatrix.[y * xdimension + x].w <- result.w)
            if (D = 3) then (lMatrix.[y * xdimension + x].z <- ratio.[lidy]) else (inplaceMatrix.[y * xdimension + x].z <- result.z)
        else
            inplaceMatrix.[y * xdimension + x].x <- result.x
            inplaceMatrix.[y * xdimension + x].y <- result.y
            inplaceMatrix.[y * xdimension + x].w <- result.w
            inplaceMatrix.[y * xdimension + x].z <- result.z

[<ReflectedDefinition>]
let LUCombine(lMatrix: float32[],
              inplaceMatrix: float32[]) =
    let i = get_global_id(1)
    let j = get_global_id(0)
    let gidx = get_group_id(0)
    let gidy = get_group_id(1)
    let dimension = get_global_size(0)
    if (i > j) then
        let dimension = get_global_size(0)
        inplaceMatrix.[i * dimension + j] <- lMatrix.[i * dimension + j]
    
type LUDecompositionTrainingSample() =    
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
        let matrix, dimension = o :?> float32[] * int
        for d = 0 to dimension - 2 do
            for i = d + 1 to dimension - 1 do
                let ratio = matrix.[i * dimension + d] / matrix.[d * dimension + d]
                for j = d to dimension - 1 do
                    matrix.[ i * dimension + j] <- matrix.[ i * dimension + j] - matrix.[d * dimension + j] * ratio
                    if j = d then
                        matrix.[i * dimension + j] <- ratio
        box matrix

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
        let ifl = MemoryFlags.UseHostPointer ||| MemoryFlags.ReadWrite
        let ofl = MemoryFlags.UseHostPointer ||| MemoryFlags.ReadWrite

        let mutable execResults: obj list list = []
        let sizes = (seq {
                        let s = ref minSize
                        while !s <= maxSize do
                            yield (!s, !s)
                            //yield (!s, !s * 2L)
                            s := !s + 64L
                    }) |> Array.ofSeq

        for rows, cols in sizes do
            Console.WriteLine("      Size: " + String.Format("{0,5:#####}", rows) + "x" + String.Format("{0,5:#####}", cols))
                                            
            let blockSize = rows / (VECTOR_SIZE |> int64)

            let inplaceMatrix = Array.init (rows * cols |> int) (fun i -> rnd.Next()  % 10 + 1 |> float32)
            let inputMatrix2 = Array.zeroCreate<float32> (rows * cols |> int)

            let reference = this.CreateVerifiedOutput((inplaceMatrix, rows |> int)) :?> float32[]

            let mutable features: obj list = []
            let mutable instanceResult: obj list = []

            for pIndex, pName, pDevs in GetOpenCLPlatforms() do   
                for dIndex, dName, dType in pDevs do
                    if dIndex > -1 then
                        Console.WriteLine(" Device " + ": " + dName.ToString() + "(" + dType.ToString() + ")")  

                        // Retrieve the best work group size for the kernel
                        let comp = <@ 
                                        DEVICE(pIndex, dIndex,
                                            LUDecompose(
                                                BUFFER_READ_MODE(rm, 
                                                    BUFFER_WRITE_MODE(wm,
                                                        MEMORY_FLAGS(ifl, 
                                                            AsFloat4(inputMatrix2)))),
                                                BUFFER_READ_MODE(rm, 
                                                    BUFFER_WRITE_MODE(wm,
                                                        MEMORY_FLAGS(ifl, 
                                                            AsFloat4(inplaceMatrix)))),
                                                0,
                                                [||])) 
                                       @>
                        let kernelWorkGroupSize = comp.GetWorkGroupSize()

                        // Run iterations for execution
                        let globalSize = [| blockSize; rows |]
                        let localSize = [| blockSize; 1L |]
                        let globalOffset = [| 0L; 0L |]

                        for index = 0 to (rows |> int) - 1 do
                            if index % VECTOR_SIZE = 0 then
                                // Setup global size, local size and offset
                                globalOffset.[0] <- (index / VECTOR_SIZE) |> int64
                                globalOffset.[1] <- VECTOR_SIZE * (index / VECTOR_SIZE) |> int64

                                if (index = 0) then
                                    globalSize.[0] <- globalSize.[0] + 1L
                                    globalSize.[1] <- globalSize.[1] + (VECTOR_SIZE |> int64)
                                globalSize.[0] <- globalSize.[0] - 1L
                                globalSize.[1] <- globalSize.[1] - (VECTOR_SIZE |> int64)

                                if globalSize.[0] <= kernelWorkGroupSize then
                                    localSize.[0] <- globalSize.[0]
                                else
                                    let mutable temp = kernelWorkGroupSize
                                    let mutable ok = false
                                    while not ok && temp > 1L do
                                        if globalSize.[0] % temp = 0L then
                                            ok <- true
                                        else
                                            temp <- temp - 1L
                                    localSize.[0] <- temp

                                if globalSize.[1] <= kernelWorkGroupSize / localSize.[0] then
                                    localSize.[1] <- globalSize.[1]
                                else
                                    let mutable temp = kernelWorkGroupSize / localSize.[0]
                                    let mutable ok = false
                                    while not ok && temp > 1L do
                                        if globalSize.[1] % temp = 0L then
                                            ok <- true
                                        else
                                            temp <- temp - 1L
                                    localSize.[1] <- temp
                     
                            // Run computation
                            let localData = Array.zeroCreate<float32> (localSize.[1] |> int)
                            let comp = <@ 
                                        DEVICE(pIndex, dIndex,
                                            LUDecompose(
                                                BUFFER_READ_MODE(rm, 
                                                    BUFFER_WRITE_MODE(wm,
                                                        MEMORY_FLAGS(ifl, 
                                                            AsFloat4(inputMatrix2)))),
                                                BUFFER_READ_MODE(rm, 
                                                    BUFFER_WRITE_MODE(wm,
                                                        MEMORY_FLAGS(ifl, 
                                                            AsFloat4(inplaceMatrix)))),
                                                index,
                                                localData)) 
                                       @>

                            // Extract features
                            let km = compiler.Compile(comp, opts) :?> IKernelModule
                            //let precomputedFeatures = chain.Precompute(km)
                            //features <- chain.Evaluate(km, precomputedFeatures, [ AsFloat4(inputMatrix2); AsFloat4(inplaceMatrix); index; localData ], globalSize, localSize, opts)

                            // Run once to skip compilation time
                            comp.Run(globalSize, localSize, globalOffset)

                        // Compose LU
                        let comp = <@ 
                                    DEVICE(pIndex, dIndex,
                                        LUCombine(
                                            BUFFER_READ_MODE(rm, 
                                                BUFFER_WRITE_MODE(wm,
                                                    MEMORY_FLAGS(ifl, 
                                                        inputMatrix2))),
                                            BUFFER_READ_MODE(rm, 
                                                BUFFER_WRITE_MODE(wm,
                                                    MEMORY_FLAGS(ifl, 
                                                        inplaceMatrix))))) 
                                   @>

                        // Extract features
                        let km = compiler.Compile(comp, opts) :?> IKernelModule
                        //let precomputedFeatures = chain.Precompute(km)
                        //features <- chain.Evaluate(km, precomputedFeatures, [ AsFloat4(inputMatrix2); AsFloat4(inplaceMatrix); ], [| rows; cols |], localSize, opts)

                        // Run once to skip compilation time
                        comp.Run([| rows; cols |], null, [| 0L; 0L |])
     
                        if not (this.Verify(inplaceMatrix, reference)) then
                            Console.WriteLine("---------------- COMPUTATION RESULT ERROR")
                        else                        
                            // Run
                            let watch = new Stopwatch()
                            watch.Start()
                            for i = 0 to iterations - 1 do                                
                                // Run iterations for execution
                                let globalSize = [| blockSize; rows |]
                                let localSize = [| blockSize; 1L |]
                                let globalOffset = [| 0L; 0L |]

                                for index = 0 to (rows |> int) - 1 do
                                    if index % VECTOR_SIZE = 0 then
                                        // Setup global size, local size and offset
                                        globalOffset.[0] <- (index / VECTOR_SIZE) |> int64
                                        globalOffset.[1] <- VECTOR_SIZE * (index / VECTOR_SIZE) |> int64

                                        if (index = 0) then
                                            globalSize.[0] <- globalSize.[0] + 1L
                                            globalSize.[1] <- globalSize.[1] + (VECTOR_SIZE |> int64)
                                        globalSize.[0] <- globalSize.[0] - 1L
                                        globalSize.[1] <- globalSize.[1] - (VECTOR_SIZE |> int64)

                                        if globalSize.[0] <= kernelWorkGroupSize then
                                            localSize.[0] <- globalSize.[0]
                                        else
                                            let mutable temp = kernelWorkGroupSize
                                            let mutable ok = false
                                            while not ok && temp > 1L do
                                                if globalSize.[0] % temp = 0L then
                                                    ok <- true
                                                else
                                                    temp <- temp - 1L
                                            localSize.[0] <- temp

                                        if globalSize.[1] <= kernelWorkGroupSize / localSize.[0] then
                                            localSize.[1] <- globalSize.[1]
                                        else
                                            let mutable temp = kernelWorkGroupSize / localSize.[0]
                                            let mutable ok = false
                                            while not ok && temp > 1L do
                                                if globalSize.[1] % temp = 0L then
                                                    ok <- true
                                                else
                                                    temp <- temp - 1L
                                            localSize.[1] <- temp
                     
                                    // Run computation
                                    let localData = Array.zeroCreate<float32> (localSize.[1] |> int)
                                    let comp = <@ 
                                                DEVICE(pIndex, dIndex,
                                                    LUDecompose(
                                                        BUFFER_READ_MODE(rm, 
                                                            BUFFER_WRITE_MODE(wm,
                                                                MEMORY_FLAGS(ifl, 
                                                                    AsFloat4(inputMatrix2)))),
                                                        BUFFER_READ_MODE(rm, 
                                                            BUFFER_WRITE_MODE(wm,
                                                                MEMORY_FLAGS(ifl, 
                                                                    AsFloat4(inplaceMatrix)))),
                                                        index,
                                                        localData)) 
                                               @>
                                    comp.Run(globalSize, localSize, globalOffset)
                                // Compose LU
                                let comp = <@ 
                                            DEVICE(pIndex, dIndex,
                                                LUCombine(
                                                    BUFFER_READ_MODE(rm, 
                                                        BUFFER_WRITE_MODE(wm,
                                                            MEMORY_FLAGS(ifl, 
                                                                inputMatrix2))),
                                                    BUFFER_READ_MODE(rm, 
                                                        BUFFER_WRITE_MODE(wm,
                                                            MEMORY_FLAGS(ifl, 
                                                                inplaceMatrix))))) 
                                           @>
                                // Run once to skip compilation time
                                comp.Run([| rows; cols |], null, [| 0L; 0L |])
                            watch.Stop()
                            let ttime, iters = ((double)watch.ElapsedMilliseconds) /((double)iterations), iterations
                                        
                            Console.WriteLine("---------------- " + String.Format("{0,11:######0.0000}", ttime) + "ms (" + String.Format("{0,10:#########0}", iters) + " iterations)")
                            instanceResult <- instanceResult @ [ ttime ]
                            System.Threading.Thread.Sleep(500)
                                
            execResults <- execResults @ [ instanceResult @ [rows; cols] @ features ]       
        execResults
         
type LUDecompositionOpenCLDirectTrainingSample() =    
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
        let matrix, dimension = o :?> float32[] * int
        for d = 0 to dimension - 2 do
            for i = d + 1 to dimension - 1 do
                let ratio = matrix.[i * dimension + d] / matrix.[d * dimension + d]
                for j = d to dimension - 1 do
                    matrix.[ i * dimension + j] <- matrix.[ i * dimension + j] - matrix.[d * dimension + j] * ratio
                    if j = d then
                        matrix.[i * dimension + j] <- ratio
        box matrix

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
        let ifl = MemoryFlags.UseHostPointer ||| MemoryFlags.ReadWrite
        let ofl = MemoryFlags.UseHostPointer ||| MemoryFlags.ReadWrite

        let mutable execResults: obj list list = []
        let sizes = (seq {
                        let s = ref minSize
                        while !s <= maxSize do
                            yield (!s, !s)
                            //yield (!s, !s * 2L)
                            s := !s + 64L
                    }) |> Array.ofSeq
                    
        for rows, cols in sizes do
            Console.WriteLine("      Size: " + String.Format("{0,5:#####}", rows) + "x" + String.Format("{0,5:#####}", cols))
                                            
            let blockSize = rows / (VECTOR_SIZE |> int64)

            let inplaceMatrix = Array.init (rows * cols |> int) (fun i -> rnd.Next()  % 10 + 1 |> float32)
            let inputMatrix2 = Array.zeroCreate<float32> (rows * cols |> int)

            let reference = this.CreateVerifiedOutput((inplaceMatrix, rows |> int)) :?> float32[]

            let mutable features: obj list = []
            let mutable instanceResult: obj list = []

            for pIndex, pName, pDevs in GetOpenCLPlatforms() do   
                for dIndex, dName, dType in pDevs do
                    // Setup OpenCL
                    let platform = OpenCL.OpenCLPlatform.Platforms.[pIndex]
                    let device = platform.Devices.[dIndex]
                    let contextProperties = new OpenCLContextPropertyList(platform)
                    let devices = new List<OpenCLDevice>()
                    devices.Add(device)
                    let context = new OpenCLContext(devices, contextProperties, null, System.IntPtr.Zero) 
                    let queue = new OpenCLCommandQueue(context, device, OpenCLCommandQueueProperties.None) 
                   
                    Console.WriteLine(" Device " + ": " + dName.ToString() + "(" + dType.ToString() + ")")  

                    // Retrieve the best work group size for the kernel
                    let decomp = <@ 
                                    DEVICE(pIndex, dIndex,
                                        LUDecompose(
                                            BUFFER_READ_MODE(rm, 
                                                BUFFER_WRITE_MODE(wm,
                                                    MEMORY_FLAGS(ifl, 
                                                        AsFloat4(inputMatrix2)))),
                                            BUFFER_READ_MODE(rm, 
                                                BUFFER_WRITE_MODE(wm,
                                                    MEMORY_FLAGS(ifl, 
                                                        AsFloat4(inplaceMatrix)))),
                                            0,
                                            [||])) 
                                    @>
                    let kmdecomp = compiler.Compile(decomp, opts) :?> IKernelModule
                    let program = new OpenCLProgram(context, [| kmdecomp.Code.Value |])
                    program.Build(devices, "", null, System.IntPtr.Zero)
                    let decomposeKernel = program.CreateKernel("LUDecompose")
                    let kernelWorkGroupSize = decomposeKernel.GetWorkGroupSize(device)
                    
                    let combcomp = <@ 
                                        DEVICE(pIndex, dIndex,
                                            LUCombine(
                                                BUFFER_READ_MODE(rm, 
                                                    BUFFER_WRITE_MODE(wm,
                                                        MEMORY_FLAGS(ifl, 
                                                            inputMatrix2))),
                                                BUFFER_READ_MODE(rm, 
                                                    BUFFER_WRITE_MODE(wm,
                                                        MEMORY_FLAGS(ifl, 
                                                            inplaceMatrix))))) 
                                        @>
                    let kmdecomb = compiler.Compile(combcomp, opts) :?> IKernelModule
                    let combprogram = new OpenCLProgram(context, [| kmdecomb.Code.Value |])
                    combprogram.Build(devices, "", null, System.IntPtr.Zero)
                    let combineKernel = combprogram.CreateKernel("LUCombine")

                    let matrix2Buffer = new OpenCLBuffer(context, OpenCLMemoryFlags.UseHostPointer, inputMatrix2)
                    let inplaceMatrixBuffer = new OpenCLBuffer(context, OpenCLMemoryFlags.UseHostPointer, inplaceMatrix)

                    // Run iterations for execution
                    let globalSize = [| blockSize; rows |]
                    let localSize = [| blockSize; 1L |]
                    let globalOffset = [| 0L; 0L |]

                    for index = 0 to (rows |> int) - 1 do
                        if index % VECTOR_SIZE = 0 then
                            // Setup global size, local size and offset
                            globalOffset.[0] <- (index / VECTOR_SIZE) |> int64
                            globalOffset.[1] <- VECTOR_SIZE * (index / VECTOR_SIZE) |> int64

                            if (index = 0) then
                                globalSize.[0] <- globalSize.[0] + 1L
                                globalSize.[1] <- globalSize.[1] + (VECTOR_SIZE |> int64)
                            globalSize.[0] <- globalSize.[0] - 1L
                            globalSize.[1] <- globalSize.[1] - (VECTOR_SIZE |> int64)

                            if globalSize.[0] <= kernelWorkGroupSize then
                                localSize.[0] <- globalSize.[0]
                            else
                                let mutable temp = kernelWorkGroupSize
                                let mutable ok = false
                                while not ok && temp > 1L do
                                    if globalSize.[0] % temp = 0L then
                                        ok <- true
                                    else
                                        temp <- temp - 1L
                                localSize.[0] <- temp

                            if globalSize.[1] <= kernelWorkGroupSize / localSize.[0] then
                                localSize.[1] <- globalSize.[1]
                            else
                                let mutable temp = kernelWorkGroupSize / localSize.[0]
                                let mutable ok = false
                                while not ok && temp > 1L do
                                    if globalSize.[1] % temp = 0L then
                                        ok <- true
                                    else
                                        temp <- temp - 1L
                                localSize.[1] <- temp
                     
                        // Run computation
                        decomposeKernel.SetMemoryArgument(0, matrix2Buffer)
                        decomposeKernel.SetMemoryArgument(1, inplaceMatrixBuffer)
                        decomposeKernel.SetValueArgument(2, index)
                        decomposeKernel.SetLocalArgument(3, localSize.[1] * (sizeof<float32> |> int64))
                        decomposeKernel.SetValueArgument(4, inputMatrix2.Length)
                        decomposeKernel.SetValueArgument(5, inplaceMatrix.Length)
                        decomposeKernel.SetValueArgument(6, localSize.[1] |> int)
                        queue.Execute(decomposeKernel, globalOffset, globalSize, localSize, null, null)
                        queue.Finish()
                    // Compose LU
                    combineKernel.SetMemoryArgument(0, matrix2Buffer)
                    combineKernel.SetMemoryArgument(1, inplaceMatrixBuffer)
                    combineKernel.SetValueArgument(2, inputMatrix2.Length)
                    combineKernel.SetValueArgument(3, inplaceMatrix.Length)
                    queue.Execute(combineKernel, null, [| rows; cols |], null, null, null)
                    queue.Finish()
                    // Free
                    matrix2Buffer.Dispose()
                    inplaceMatrixBuffer.Dispose()

                    if not (this.Verify(inplaceMatrix, reference)) then
                        Console.WriteLine("---------------- COMPUTATION RESULT ERROR")
                    else                        
                        // Run
                        let watch = new Stopwatch()
                        watch.Start()
                        for i = 0 to iterations - 1 do                             
                            let matrix2Buffer = new OpenCLBuffer(context, OpenCLMemoryFlags.UseHostPointer, inputMatrix2)
                            let inplaceMatrixBuffer = new OpenCLBuffer(context, OpenCLMemoryFlags.UseHostPointer, inplaceMatrix)

                            // Run iterations for execution
                            let globalSize = [| blockSize; rows |]
                            let localSize = [| blockSize; 1L |]
                            let globalOffset = [| 0L; 0L |]

                            for index = 0 to (rows |> int) - 1 do
                                if index % VECTOR_SIZE = 0 then
                                    // Setup global size, local size and offset
                                    globalOffset.[0] <- (index / VECTOR_SIZE) |> int64
                                    globalOffset.[1] <- VECTOR_SIZE * (index / VECTOR_SIZE) |> int64

                                    if (index = 0) then
                                        globalSize.[0] <- globalSize.[0] + 1L
                                        globalSize.[1] <- globalSize.[1] + (VECTOR_SIZE |> int64)
                                    globalSize.[0] <- globalSize.[0] - 1L
                                    globalSize.[1] <- globalSize.[1] - (VECTOR_SIZE |> int64)

                                    if globalSize.[0] <= kernelWorkGroupSize then
                                        localSize.[0] <- globalSize.[0]
                                    else
                                        let mutable temp = kernelWorkGroupSize
                                        let mutable ok = false
                                        while not ok && temp > 1L do
                                            if globalSize.[0] % temp = 0L then
                                                ok <- true
                                            else
                                                temp <- temp - 1L
                                        localSize.[0] <- temp

                                    if globalSize.[1] <= kernelWorkGroupSize / localSize.[0] then
                                        localSize.[1] <- globalSize.[1]
                                    else
                                        let mutable temp = kernelWorkGroupSize / localSize.[0]
                                        let mutable ok = false
                                        while not ok && temp > 1L do
                                            if globalSize.[1] % temp = 0L then
                                                ok <- true
                                            else
                                                temp <- temp - 1L
                                        localSize.[1] <- temp
                     
                                // Run computation
                                decomposeKernel.SetMemoryArgument(0, matrix2Buffer)
                                decomposeKernel.SetMemoryArgument(1, inplaceMatrixBuffer)
                                decomposeKernel.SetValueArgument(2, index)
                                decomposeKernel.SetLocalArgument(3, localSize.[1] * (sizeof<float32> |> int64))
                                queue.Execute(decomposeKernel, globalOffset, globalSize, localSize, null, null)
                                queue.Finish()
                            // Compose LU
                            combineKernel.SetMemoryArgument(0, matrix2Buffer)
                            combineKernel.SetMemoryArgument(1, inplaceMatrixBuffer)
                            queue.Execute(combineKernel, null, [| rows; cols |], null, null, null)
                            queue.Finish()
                            // Free
                            matrix2Buffer.Dispose()
                            inplaceMatrixBuffer.Dispose()
                        watch.Stop()
                        let ttime, iters = ((double)watch.ElapsedMilliseconds) /((double)iterations), iterations
                                        
                        Console.WriteLine("---------------- " + String.Format("{0,11:######0.0000}", ttime) + "ms (" + String.Format("{0,10:#########0}", iters) + " iterations)")
                        instanceResult <- instanceResult @ [ ttime ]
                        System.Threading.Thread.Sleep(500)
                                
            execResults <- execResults @ [ instanceResult @ [rows; cols] @ features ]       
        execResults

 



