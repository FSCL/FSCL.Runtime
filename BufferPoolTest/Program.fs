open FSCL.Compiler
open FSCL.Compiler.Language
open FSCL.Runtime.Language
open FSCL.Runtime
open System.Reflection
open System.Reflection.Emit
open System
open System.Collections.Generic
open System.Diagnostics
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Linq.RuntimeHelpers
        
// Vector addition
[<Device(0,0)>][<ReflectedDefinition>]
let VectorAdd(a: float32[], b: float32[], c: float32[]) =
    let gid = get_global_id(0)
    c.[gid] <- a.[gid] + b.[gid]
        
// Vector addition
[<Device(0,0)>][<ReflectedDefinition>]
let VectorAddReturnPar(a: float32[], b: float32[], returnedPar: float32[]) =
    let gid = get_global_id(0)
    returnedPar.[gid] <- a.[gid] + b.[gid]
    returnedPar

[<Device(0,0)>][<ReflectedDefinition>]
let VectorAddReturn(a: float32[], b: float32[]) =
    let returned = Array.zeroCreate<float32> (a.GetLength(0))
    let gid = get_global_id(0)
    returned.[gid] <- a.[gid] + b.[gid]
    returned

let CheckResult(c: float32[], correctMapResult: float32[]) = 
    let mutable isResultCorrect = true
    for i = 0 to correctMapResult.Length - 1 do
        if correctMapResult.[i] <> c.[i] then
            isResultCorrect <- false
    if not isResultCorrect then
        Console.WriteLine("  Wrong result!")

[<EntryPoint>]
let main argv =
    let size = 1 <<< 20
    let lsize = size |> int64
    // Create buffers
    let a = Array.create size 2.0f
    let b = Array.create size 3.0f
    let aAlias = Array.create size 2.0f
    let bAlias = Array.create size 3.0f
    let mutable c = Array.zeroCreate<float32> (size)
    let correctMapResult = Array.create size 5.0f
    let correctMapResult2 = Array.create size 7.0f

    // Options
    let opts = new Dictionary<string, obj>()
    opts.Add(RuntimeOptions.BufferSharePriority, BufferSharePriority.PriorityToFlags)
    
    // Check opencl devices
    let plats = GetOpenCLPlatforms()
    if plats.Count = 0 then
        Console.WriteLine("No OpenCL-enabled device found on this platform")
    else
        // Show OpenCL devices
        Console.WriteLine("Your OpenCL-enabled devices are listed below")
        
        let mutable cpuDev = None
        let mutable gpuDev = None
        for pIndex, pName, devs in plats do
            Console.WriteLine("- Platform " + pIndex.ToString())
            for dIndex, dName, dType in devs do
                if dType = DeviceType.Cpu then
                    cpuDev <- Some(pIndex, dIndex)
                if dType = DeviceType.Gpu then
                    gpuDev <- Some(pIndex, dIndex)
                Console.WriteLine("  - Device " + ": " + dName + "(" + dType.ToString() + ")")
        
        Console.WriteLine("####### Test buffer sharing in single context")
        
        // Simple vector add
        Console.WriteLine("")
        Console.WriteLine("# Test vector add with no return value")
        // Execute vector add in OpenCL mode
        c <- Array.zeroCreate<float32> (size)
        <@ VectorAdd(a, b, c) @>.Run(lsize, 64L, opts)
        CheckResult(c, correctMapResult)

        // Simple vector add
        Console.WriteLine("")
        Console.WriteLine("# Test vector add with return value")
        // Execute vector add in OpenCL mode
        c <- Array.zeroCreate<float32> (size)
        c <- <@ VectorAddReturn(a, b) @>.Run(lsize, 64L, opts)
        CheckResult(c, correctMapResult)
        
        // Simple vector add
        Console.WriteLine("")
        Console.WriteLine("# Test vector add(addReturn) concatenation (no arg sharing)")
        // Execute vector add in OpenCL mode
        c <- Array.zeroCreate<float32> (size)
        <@ VectorAdd(aAlias, VectorAddReturn(a, b), c) @>.Run(lsize, 64L, opts)
        CheckResult(c, correctMapResult2)
        
        // Simple vector add
        Console.WriteLine("")
        Console.WriteLine("# Test vector add(addReturnPar) concatenation (no arg sharing)")
        // Execute vector add in OpenCL mode
        c <- Array.zeroCreate<float32> (size)
        c <- <@ VectorAddReturnPar(a, VectorAddReturn(a, b), c) @>.Run(lsize, 64L, opts)
        CheckResult(c, correctMapResult2)

        // Simple vector add
        Console.WriteLine("")
        Console.WriteLine("# Test vector addReturn(addReturn) concatenation (no arg sharing)")
        // Execute vector add in OpenCL mode
        c <- Array.zeroCreate<float32> (size)
        c <- <@ VectorAddReturn(aAlias, VectorAddReturn(a, b)) @>.Run(lsize, 64L, opts)
        CheckResult(c, correctMapResult2)
        
        // Simple vector add
        Console.WriteLine("")
        Console.WriteLine("# Test vector add(addReturn) concatenation (arg read-only sharing)")
        // Execute vector add in OpenCL mode
        c <- Array.zeroCreate<float32> (size)
        <@ VectorAdd(a, VectorAddReturn(a, b), c) @>.Run(lsize, 64L, opts)
        CheckResult(c, correctMapResult2)
                
        // Simple vector add
        Console.WriteLine("")
        Console.WriteLine("# Test vector addReturn(addReturnPar) concatenation (no arg sharing)")
        // Execute vector add in OpenCL mode
        c <- Array.zeroCreate<float32> (size)
        c <- <@ VectorAddReturn(a, VectorAddReturnPar(a, b, c)) @>.Run(lsize, 64L, opts)
        CheckResult(c, correctMapResult2)

        // Simple vector add
        Console.WriteLine("")
        Console.WriteLine("# Test vector addReturnPar(addReturn) concatenation (arg write-only sharing)")
        // Execute vector add in OpenCL mode
        c <- Array.zeroCreate<float32> (size)
        c <- <@ VectorAddReturnPar(aAlias, bAlias, VectorAddReturn(a, b)) @>.Run(lsize, 64L, opts)
        CheckResult(c, correctMapResult)
        
        // Simple vector add
        Console.WriteLine("")
        Console.WriteLine("# Test vector addReturnPar(addReturnPar) concatenation (arg write-only sharing)")
        // Execute vector add in OpenCL mode
        c <- Array.zeroCreate<float32> (size)
        c <- <@ VectorAddReturnPar(aAlias, bAlias, VectorAddReturnPar(a, b, c)) @>.Run(lsize, 64L, opts)
        CheckResult(c, correctMapResult)
        
        Console.WriteLine("########################################################")

        Console.WriteLine("####### Test buffer sharing in single context with access flags")
        // Simple vector add
        Console.WriteLine("")
        Console.WriteLine("# Test vector add(addReturn) concatenation (force subkernel return write only)")
        // Execute vector add in OpenCL mode
        c <- Array.zeroCreate<float32> (size)
        <@ VectorAdd(a, 
                     RETURN_MEMORY_FLAGS(MemoryFlags.WriteOnly,
                      VectorAddReturn(a, b)), 
                     c) @>.Run(lsize, 64L, opts)
        CheckResult(c, correctMapResult2)
        
        // Simple vector add
        Console.WriteLine("")
        Console.WriteLine("# Test vector add(addReturn) concatenation (force root kernel par read only)")
        // Execute vector add in OpenCL mode
        c <- Array.zeroCreate<float32> (size)
        <@ VectorAdd(a, 
                     MEMORY_FLAGS(MemoryFlags.ReadOnly, 
                      VectorAddReturn(a, b)), 
                     c) @>.Run(lsize, 64L, opts)
        CheckResult(c, correctMapResult2)
        
        Console.WriteLine("########################################################")

        Console.WriteLine("####### Test buffer sharing in single context with alloc flags")
        // Simple vector add
        Console.WriteLine("")
        Console.WriteLine("# Test vector add(addReturn) concatenation (different flags for return and param)")
        // Execute vector add in OpenCL mode
        c <- Array.zeroCreate<float32> (size)
        <@ VectorAdd(a, 
                     MEMORY_FLAGS(MemoryFlags.None, 
                      RETURN_MEMORY_FLAGS(MemoryFlags.UsePersistentMemAMD, 
                       VectorAddReturn(a, b))), 
                     c) @>.Run(lsize, 64L, opts)
        CheckResult(c, correctMapResult2)
        
        // Simple vector add
        Console.WriteLine("")
        Console.WriteLine("# Test vector add(addReturn) concatenation (different flags for return and param but priority to share)")
        // Execute vector add in OpenCL mode
        opts.Clear()
        opts.Add(RuntimeOptions.BufferSharePriority, BufferSharePriority.PriorityToShare)
        c <- Array.zeroCreate<float32> (size)
        <@ VectorAdd(a, 
                     MEMORY_FLAGS(MemoryFlags.None, 
                      RETURN_MEMORY_FLAGS(MemoryFlags.UsePersistentMemAMD, 
                       VectorAddReturn(a, b))), 
                     c) @>.Run(lsize, 64L, opts)
        CheckResult(c, correctMapResult2)
                
        Console.WriteLine("########################################################")

        if gpuDev.IsSome && cpuDev.IsSome then
            let cpuP, cpuD = cpuDev.Value
            let gpuP, gpuD = gpuDev.Value

            // Simple vector add
            Console.WriteLine("")
            Console.WriteLine("# Test vector add(addReturn) concatenation on GPU")
            // Execute vector add in OpenCL mode
            c <- Array.zeroCreate<float32> (size)
            <@ DEVICE(gpuP, gpuD, 
                VectorAdd(a, 
                          MEMORY_FLAGS(MemoryFlags.None, 
                           RETURN_MEMORY_FLAGS(MemoryFlags.UsePersistentMemAMD, 
                            DEVICE(gpuP, gpuD,
                             VectorAddReturn(a, b)))), 
                          c)) @>.Run(lsize, 64L, opts)
            CheckResult(c, correctMapResult2)
        
            // Simple vector add
            Console.WriteLine("")
            Console.WriteLine("# Test vector add(addReturn) concatenation on CPU (alloc flags and rw mode optimised)")
            // Execute vector add in OpenCL mode
            opts.Clear()
            opts.Add(RuntimeOptions.BufferSharePriority, BufferSharePriority.PriorityToShare)
            c <- Array.zeroCreate<float32> (size)
            <@ DEVICE(cpuP, cpuD, 
                VectorAdd(a, 
                          MEMORY_FLAGS(MemoryFlags.None, 
                           RETURN_MEMORY_FLAGS(MemoryFlags.UsePersistentMemAMD, 
                            DEVICE(cpuP, cpuD,
                             VectorAddReturn(a, b)))), 
                          c)) @>.Run(lsize, 64L, opts)
            CheckResult(c, correctMapResult2)

        Console.WriteLine("########################################################")
                
    Console.WriteLine("Press Enter to exit...")
    Console.Read() |> ignore
    0


    