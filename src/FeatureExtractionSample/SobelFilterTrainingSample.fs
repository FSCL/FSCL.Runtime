module SobelFilterTrainingSample

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

(*
[<ReflectedDefinition>]
let SobelFilter(inputImage: uchar4[],
                outputImage: uchar4[]) =
    let x = get_global_id(0)
    let y = get_global_id(1)

    let width = get_global_size(0)
    let height = get_global_size(1)

    let mutable Gx = float4(0.0f)
    let mutable Gy = Gx
    
    let c = x + y * width

    // Read each texel component and calculate the filtered value using neighbouring texel components 
    if x >= 1 && x < (width-1) && y >= 1 && y < height - 1 then
        let i00 = (inputImage.[c - 1 - width]).ToFloat4()
        let i10 = (inputImage.[c - width]).ToFloat4()
        let i20 = (inputImage.[c + 1 - width]).ToFloat4()
        let i01 = (inputImage.[c - 1]).ToFloat4()
        let i11 = (inputImage.[c]).ToFloat4()
        let i21 = (inputImage.[c + 1]).ToFloat4()
        let i02 = (inputImage.[c - 1 + width]).ToFloat4()
        let i12 = (inputImage.[c + width]).ToFloat4()
        let i22 = (inputImage.[c + 1 + width]).ToFloat4()

        Gx <- i00 + float4(2.0f) * i10 + i20 - i02  - float4(2.0f) * i12 - i22
        Gy <- i00 - i20  + float4(2.0f) * i01 - float4(2.0f) * i21 + i02 - i22

        outputImage.[c] <- (float4.hypot(Gx, Gy)/float4(2.0f)).ToUChar4()
        *)
[<ReflectedDefinition>]
let SobelFilter2D(inputImage: uchar4[,],
                  outputImage: uchar4[,],
                  wi: WorkItemInfo) =
    let x = wi.GlobalID(0)
    let y = wi.GlobalID(1)

    let width = wi.GlobalSize(0)
    let height = wi.GlobalSize(1)

    let mutable Gx = float4(0.0f)
    let mutable Gy = Gx

    // Read each texel component and calculate the filtered value using neighbouring texel components 
    if x >= 1 && x < width - 1 && y >= 1 && y < height - 1 then
        let i00 = (inputImage.[y - 1, x - 1]).ToFloat4()
        let i10 = (inputImage.[y - 1, x]).ToFloat4()
        let i20 = (inputImage.[y - 1, x + 1]).ToFloat4()
        let i01 = (inputImage.[y, x - 1]).ToFloat4()
        let i11 = (inputImage.[y, x]).ToFloat4()
        let i21 = (inputImage.[y, x + 1]).ToFloat4()
        let i02 = (inputImage.[y + 1, x - 1]).ToFloat4()
        let i12 = (inputImage.[y + 1, x]).ToFloat4()
        let i22 = (inputImage.[y + 1, x + 1]).ToFloat4()

        Gx <- i00 + float4(2.0f) * i10 + i20 - i02  - float4(2.0f) * i12 - i22
        Gy <- i00 - i20  + float4(2.0f) * i01 - float4(2.0f) * i21 + i02 - i22

        outputImage.[y, x] <- (float4.hypot(Gx, Gy)/float4(2.0f)).ToUChar4()
                
[<ReflectedDefinition>]
let SobelFilter2DNoBorder(inputImage: uchar4[,],
                          outputImage: uchar4[,],
                          wi: WorkItemInfo) =
    let x = wi.GlobalID(0)
    let y = wi.GlobalID(1)

    let width = outputImage.GetLength(1)
    let height = outputImage.GetLength(0)

    let mutable Gx = float4(0.0f)
    let mutable Gy = Gx

    if x < width && y < height then
        // Read each texel component and calculate the filtered value using neighbouring texel components 
        let i00 = (inputImage.[y, x]).ToFloat4()
        let i10 = (inputImage.[y, x + 1]).ToFloat4()
        let i20 = (inputImage.[y, x + 2]).ToFloat4()
        let i01 = (inputImage.[y + 1, x]).ToFloat4()
        let i11 = (inputImage.[y + 1, x + 1]).ToFloat4()
        let i21 = (inputImage.[y + 1, x + 2]).ToFloat4()
        let i02 = (inputImage.[y + 2, x]).ToFloat4()
        let i12 = (inputImage.[y + 2, x + 1]).ToFloat4()
        let i22 = (inputImage.[y + 2, x + 2]).ToFloat4()

        Gx <- i00 + float4(2.0f) * i10 + i20 - i02  - float4(2.0f) * i12 - i22
        Gy <- i00 - i20  + float4(2.0f) * i01 - float4(2.0f) * i21 + i02 - i22

        outputImage.[y, x] <- (float4.hypot(Gx, Gy)/float4(2.0f)).ToUChar4()

type SobelFilterTrainingSample() =
   inherit IDefaultFeatureExtractionTrainingSample()

    override this.DefaultConfigurationDictionary() =
        let dict = new Dictionary<string, obj>()
        dict.Add("MinMatrixSize", 64L)
        dict.Add("MaxMatrixSize", 2048L)
        dict.Add("Iterations", 100)
        dict
        
    override this.Verify(output: obj, reference: obj) =
        let o = output :?> uchar4[,]
        let r = reference :?> uchar4[,]
        let mutable eq = true
        for row = 0 to o.GetLength(0) - 1 do
            for col = 0 to o.GetLength(1) - 1 do
                let f = o.[row, col]
                let s = r.[row, col]
                let f0,f1,f2,f3 = f.x, f.y, f.z, f.w
                let s0,s1,s2,s3 = s.x, s.y, s.z, s.w
                if f0 <> s0 || f1 <> s1 || f2 <> s2 || f3 <> s3 then
                    eq <- false
        eq
    (*
    member this.CreateVerifiedOutput2(o: obj) =
        let input, rows, cols = o :?> (uchar4[] * int * int)
        let output = Array.zeroCreate<uchar4>(rows * cols)

        for row = 1 to rows - 2 do
            for col = 1 to cols - 2 do
                let gx = input.[(row - 1) * cols + col - 1] +
                          ((2 |> byte) * input.[(row - 1) * cols + col]) +
                          input.[(row - 1) * cols + col + 1] -
                          input.[(row + 1) * cols + col - 1] -
                          ((2 |> byte) * input.[(row + 1) * cols + col]) -
                          input.[(row + 1) * cols + col + 1]

                let gy = input.[(row - 1) * cols + col - 1] -
                          input.[(row + 1) * cols + col - 1] +
                          ((2 |> byte) * input.[(row - 1) * cols + col]) -
                          ((2 |> byte) * input.[(row + 1) * cols + col]) +
                          input.[(row - 1) * cols + col + 1] -
                          input.[(row + 1) * cols + col + 1]

                let gxc = [| gx.x; gx.y; gx.z; gx.w |]
                let gyc = [| gy.x; gy.y; gy.z; gy.w |]

                output.[row * cols + col] <- (gx.ToFloat4().pown(2) + gy.ToFloat4().pown(2)).sqrt().ToUChar4() / (2 |> byte)
        box output
        *)
    override this.CreateVerifiedOutput(o: obj) =
        let input = o :?> uchar4[,]
        let output = Array2D.zeroCreate<uchar4> (input.GetLength(0)) (input.GetLength(1))

        for row = 1 to (input.GetLength(0)) - 2 do
            for col = 1 to (input.GetLength(1)) - 2 do
                let gx = input.[row - 1, col - 1].ToFloat4() +
                         ((2 |> byte) * input.[row - 1, col]).ToFloat4() +
                         input.[row - 1, col + 1].ToFloat4() -
                         (input.[row + 1, col - 1].ToFloat4() +
                          ((2 |> byte) * input.[row + 1, col]).ToFloat4() +
                          input.[row + 1, col + 1].ToFloat4())

                let gy = input.[row - 1, col - 1].ToFloat4() -
                          input.[row - 1, col + 1].ToFloat4() +
                          ((2 |> byte) * input.[row, col - 1]).ToFloat4() -
                          ((2 |> byte) * input.[row, col + 1]).ToFloat4() +
                          input.[row + 1, col - 1].ToFloat4() -
                          input.[row + 1, col + 1].ToFloat4()

                let gxc = [| gx.x; gx.y; gx.z; gx.w |]
                let gyc = [| gy.x; gy.y; gy.z; gy.w |]

                output.[row, col] <- ((gx.pown(2) + gy.pown(2)).sqrt() / 2.0f).ToUChar4()
        box output
        
    member this.CreateVerifiedOutputNoBorder(o: obj) =
        let input = o :?> uchar4[,]
        let output = Array2D.zeroCreate<uchar4> (input.GetLength(0) - 2) (input.GetLength(1) - 2)

        for row = 0 to (input.GetLength(0)) - 3 do
            for col = 0 to (input.GetLength(1)) - 3 do
                let gx = input.[row, col].ToFloat4() +
                         ((2 |> byte) * input.[row, col + 1]).ToFloat4() +
                         input.[row, col + 2].ToFloat4() -
                         (input.[row + 2, col].ToFloat4() +
                          ((2 |> byte) * input.[row + 2, col + 1]).ToFloat4() +
                          input.[row + 2, col + 2].ToFloat4())

                let gy = input.[row, col].ToFloat4() -
                          input.[row, col + 2].ToFloat4() +
                          ((2 |> byte) * input.[row + 1, col]).ToFloat4() -
                          ((2 |> byte) * input.[row + 1, col + 2]).ToFloat4() +
                          input.[row + 2, col].ToFloat4() -
                          input.[row + 2, col + 2].ToFloat4()

                let gxc = [| gx.x; gx.y; gx.z; gx.w |]
                let gyc = [| gy.x; gy.y; gy.z; gy.w |]

                output.[row, col] <- ((gx.pown(2) + gy.pown(2)).sqrt() / 2.0f).ToUChar4()
        box output

    override this.ResultColumnIDs 
        with get() =   
            let ids = new List<String>()         
            for pIndex, pName, pDevs in GetOpenCLPlatforms() do  
                for dIndex, dName, dType in pDevs do 
                    ids.Add(dName + " Completion Time (ms)")
            //ids.Add("Matrix Width (elements)")
            //ids.Add("Matrix Height (elements)")
            ids |> List.ofSeq
    
    override this.RunInternal(chain, conf, rm: TrainingSampleRunningMode) = 
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
        let ifl = MemoryFlags.ReadOnly
        let ofl = MemoryFlags.WriteOnly
                
        let executionResults = new List<List<obj>>()
        let featureValues = new List<List<obj>>()
                
        let size = ref minSize
        while !size <= maxSize do
            executionResults.Add(new List<obj>())
            Console.WriteLine("      Size: " + String.Format("{0,10:##########}", !size))

            let outputSize = (!size |> int)
            let inputSize = outputSize + 2
                        
            // Create input
            let input = Array2D.init<uchar4> inputSize inputSize (fun r c -> uchar4(rnd.Next() % 5 |> byte, rnd.Next() % 5 |> byte, rnd.Next() % 5 |> byte, rnd.Next() % 5 |> byte))
            for i = 0 to inputSize - 1 do
                input.[0, i] <- uchar4(1 |> byte)

            // Compute reference for verification
            let reference = 
                if not featureOnly then
                    this.CreateVerifiedOutputNoBorder((input))
                else
                    null  

            // Iterate on devices
            for pIndex, pName, pDevs in GetOpenCLPlatforms() do   
                for dIndex, dName, dType in pDevs do  
                                     
                    Console.WriteLine(" Device " + ": " + dName.ToString() + "(" + dType.ToString() + ")")  
                    let output = Array2D.zeroCreate<uchar4> (outputSize) (outputSize)
                    let ws = WorkSize([| (((outputSize - 1) / 16) + 1) * 16 |> int64; (((outputSize - 1) / 16) + 1) * 16 |> int64 |], [| 16L; 16L |])

                    let comp = <@ DEVICE(pIndex, dIndex,
                                    SobelFilter2DNoBorder(
                                        BUFFER_READ_MODE(rm, 
                                            MEMORY_FLAGS(ifl, 
                                                input)),
                                        BUFFER_WRITE_MODE(wm, 
                                            MEMORY_FLAGS(ofl, 
                                                output)),
                                        ws)) @>   
                            
                    // Extract features
                    if pIndex = 0 && dIndex = 0 && not etOnly then
                        let km = compiler.Compile(comp, opts) :?> IKernelModule
                        let precomputedFeatures = chain.Precompute(km)
                        featureValues.Add(new List<obj>(chain.Evaluate(km, precomputedFeatures, [ input; output; ws ], opts)))
                                                         
                    // Run once to skip compilation time
                    if not featureOnly then    
                        comp.Run()
                        if not (this.Verify(output, reference)) then
                            Console.WriteLine("---------------- COMPUTATION RESULT ERROR")
                        else
                            // Run
                            let watch = new Stopwatch()
                            watch.Start()
                            for i = 0 to iterations - 1 do
                                comp.Run()
                            watch.Stop()
                            let ttime, iters = ((double)watch.ElapsedMilliseconds) /((double)iterations), iterations
                                        
                            // Dump
                            Console.WriteLine("---------------- " + String.Format("{0,11:######0.0000}", ttime) + "ms (" + String.Format("{0,10:#########0}", iters) + " iterations)")
                            executionResults.Last().Add(ttime)
                            System.Threading.Thread.Sleep(500)
                                     
                //executionResults.Last().AddRange([inputSize; !size])               
                size := !size + 64L   
        executionResults, featureValues
