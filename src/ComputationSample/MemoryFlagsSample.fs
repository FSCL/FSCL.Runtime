module MemoryFlagsSample

    open FSCL
    open FSCL.Language
    open FSCL.Runtime
    open FSCL.Compiler
    open System
    open System.Diagnostics
    open OpenCL
    open  System.Runtime.InteropServices

    // Vector addition
    [<Device(0,0)>][<ReflectedDefinition>]
    let VectorAdd(a: float32[], b: float32[], c: float32[], wi: WorkItemInfo) =
        let gid = wi.GlobalID(0)
        c.[gid] <- a.[gid] + b.[gid]
        
    [<StructLayout(LayoutKind.Sequential)>]
    type MyRecord = {
        mutable x: float32;
        mutable y: float32;
    }
    
    // Vector addition with record
    [<Device(0,0)>][<ReflectedDefinition>]
    let VectorAddRecord(a: MyRecord[], b: MyRecord[], c: MyRecord[], wi: WorkItemInfo) =
        let gid = wi.GlobalID(0)
        let newRecord = { x = a.[gid].x + b.[gid].x; y = a.[gid].y + b.[gid].y }
        c.[gid] <- newRecord

    let Run() =  
        let timer = new Stopwatch()

        // Vectors
        let size = 1 <<< 20
        let lsize = size |> int64
        let a = Array.create size 2.0f
        let b = Array.create size 3.0f
        let c = Array.zeroCreate<float32> (size)
        let correctMapResult = Array.create size 5.0f
        
        // Record vectors
        let aRecord = Array.create size ({ x = 1.0f; y = 2.0f })
        let bRecord = Array.create size ({ x = 4.0f; y = 3.0f })
        let mutable cRecord = Array.create size ({ x = 0.0f; y = 0.0f })
        let correctMapResultRecord = Array.create size ({ x = 5.0f; y = 5.0f })
             
        // ***************************************************************************************************
        // Vector add with value type ****************************************************************************
        let allocFlags = [| MemoryFlags.AllocHostPointer; MemoryFlags.UseHostPointer; MemoryFlags.CopyHostPointer; MemoryFlags.None; MemoryFlags.Auto |]
        let inputAccessFlags = [| MemoryFlags.ReadOnly; MemoryFlags.ReadWrite |]
        let outputAccessFlags = [| MemoryFlags.WriteOnly; MemoryFlags.ReadWrite |]
        
        for inputAllocFlags in allocFlags do
            for outputAllocFlags in allocFlags do
                for inputAccessFlags in outputAccessFlags do
                    for outputAccessFlags in outputAccessFlags do
                        for hostIsWriteOnlyForInput in [| true; false |] do                        
                            for hostIsReadOnlyForOutput in [| true; false |] do
                                for mapInput in [| true; false |] do
                                    for mapOutput in [| true; false |] do                                                            
                        
                                        Console.WriteLine("")
                                        Console.WriteLine("# Testing value type buffers")
                                        Console.WriteLine("Input alloc flags: " + inputAllocFlags.ToString())
                                        Console.WriteLine("Input access flags: " + inputAccessFlags.ToString())
                                        Console.WriteLine("Input host access flags: " + 
                                                            if hostIsWriteOnlyForInput then
                                                                MemoryFlags.HostWriteOnly.ToString()
                                                            else
                                                                "Auto")
                                        Console.WriteLine("Is input map: " + mapInput.ToString())
                                        Console.WriteLine("Output alloc flags: " + outputAllocFlags.ToString())
                                        Console.WriteLine("Output access flags: " + outputAccessFlags.ToString()) 
                                        Console.WriteLine("Output host access flags: " + 
                                                            if hostIsReadOnlyForOutput then
                                                                MemoryFlags.HostReadOnly.ToString()
                                                            else
                                                                "Auto")
                                        Console.WriteLine("Is output map: " + mapOutput.ToString())
                                        
                                        let inputFlags = 
                                            if hostIsWriteOnlyForInput then
                                                inputAllocFlags ||| inputAccessFlags ||| MemoryFlags.HostWriteOnly
                                            else
                                                inputAllocFlags ||| inputAccessFlags
                                        let inputWriteMode =
                                            if mapInput then
                                                BufferWriteMode.MapBuffer
                                            else
                                                BufferWriteMode.EnqueueWriteBuffer
                                        let outputFlags = 
                                            if hostIsReadOnlyForOutput then
                                                outputAllocFlags ||| outputAccessFlags ||| MemoryFlags.HostReadOnly
                                            else
                                                outputAllocFlags ||| outputAccessFlags
                                        let outputReadMode =
                                            if mapOutput then
                                                BufferReadMode.MapBuffer
                                            else
                                                BufferReadMode.EnqueueReadBuffer
                                                
                                        // Execute vector add in OpenCL mode
                                        let worksize = new WorkSize(lsize, 64L)
                                        let comp = 
                                            <@ 
                                                VectorAdd(
                                                    MEMORY_FLAGS(inputFlags, 
                                                        BUFFER_WRITE_MODE(inputWriteMode,
                                                            a)),
                                                    MEMORY_FLAGS(inputFlags, 
                                                        BUFFER_WRITE_MODE(inputWriteMode,
                                                            b)),
                                                    MEMORY_FLAGS(outputFlags,
                                                        BUFFER_READ_MODE(outputReadMode,
                                                            c)),
                                                    worksize) @>
                                        timer.Start()        
                                        comp.Run() 
                                        timer.Stop()
                                        // Check result
                                        let mutable isResultCorrect = true
                                        for i = 0 to correctMapResult.Length - 1 do
                                            if correctMapResult.[i] <> c.[i] then
                                                isResultCorrect <- false
                                        if not isResultCorrect then
                                            failwith "Test returned a wrong result!"
                                        else
                                            Console.WriteLine("Test execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")                       
                                            timer.Restart()        
                                            comp.Run() |> ignore
                                            timer.Stop()
                                            Console.WriteLine("Test execution time (kernel is not compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")                       
                                        
                                        System.Threading.Thread.Sleep(3000)
                                        // ***************************************************************************************************
           
        // ***************************************************************************************************
        // Vector add with ref type ****************************************************************************
        for inputAllocFlags in allocFlags do
            for outputAllocFlags in allocFlags do
                for inputAccessFlags in outputAccessFlags do
                    for outputAccessFlags in outputAccessFlags do
                        for hostIsWriteOnlyForInput in [| true; false |] do                        
                            for hostIsReadOnlyForOutput in [| true; false |] do
                                for mapInput in [| true; false |] do
                                    for mapOutput in [| true; false |] do                                                            
                        
                                        Console.WriteLine("")
                                        Console.WriteLine("# Testing ref type buffers")
                                        Console.WriteLine("Input alloc flags: " + inputAllocFlags.ToString())
                                        Console.WriteLine("Input access flags: " + inputAccessFlags.ToString())
                                        Console.WriteLine("Input host access flags: " + 
                                                            if hostIsWriteOnlyForInput then
                                                                MemoryFlags.HostWriteOnly.ToString()
                                                            else
                                                                "Auto")
                                        Console.WriteLine("Is input map: " + mapInput.ToString())
                                        Console.WriteLine("Output alloc flags: " + outputAllocFlags.ToString())
                                        Console.WriteLine("Output access flags: " + outputAccessFlags.ToString()) 
                                        Console.WriteLine("Output host access flags: " + 
                                                            if hostIsReadOnlyForOutput then
                                                                MemoryFlags.HostReadOnly.ToString()
                                                            else
                                                                "Auto")
                                        Console.WriteLine("Is output map: " + mapOutput.ToString())
                                        
                                        let inputFlags = 
                                            if hostIsWriteOnlyForInput then
                                                inputAllocFlags ||| inputAccessFlags ||| MemoryFlags.HostWriteOnly
                                            else
                                                inputAllocFlags ||| inputAccessFlags
                                        let inputWriteMode =
                                            if mapInput then
                                                BufferWriteMode.MapBuffer
                                            else
                                                BufferWriteMode.EnqueueWriteBuffer
                                        let outputFlags = 
                                            if hostIsReadOnlyForOutput then
                                                outputAllocFlags ||| outputAccessFlags ||| MemoryFlags.HostReadOnly
                                            else
                                                outputAllocFlags ||| outputAccessFlags
                                        let outputReadMode =
                                            if mapOutput then
                                                BufferReadMode.MapBuffer
                                            else
                                                BufferReadMode.EnqueueReadBuffer
                                                
                                        // Execute vector add in OpenCL mode
                                        let worksize = new WorkSize(lsize, 64L)
                                        let comp = 
                                            <@ 
                                                VectorAddRecord(
                                                    MEMORY_FLAGS(inputFlags, 
                                                        BUFFER_WRITE_MODE(inputWriteMode,
                                                            aRecord)),
                                                    MEMORY_FLAGS(inputFlags, 
                                                        BUFFER_WRITE_MODE(inputWriteMode,
                                                            bRecord)),
                                                    MEMORY_FLAGS(outputFlags,
                                                        BUFFER_READ_MODE(outputReadMode,
                                                            cRecord)),
                                                    worksize) @>
                                        timer.Start()        
                                        comp.Run() 
                                        timer.Stop()
                                        // Check result
                                        let mutable isResultCorrect = true
                                        for i = 0 to correctMapResult.Length - 1 do
                                            if correctMapResultRecord.[i] <> cRecord.[i] then
                                                isResultCorrect <- false
                                        if not isResultCorrect then
                                            failwith "Test returned a wrong result!"
                                        else
                                            Console.WriteLine("Test execution time (kernel is compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")                       
                                            timer.Restart()        
                                            comp.Run() |> ignore
                                            timer.Stop()
                                            Console.WriteLine("Test execution time (kernel is not compiled): " + timer.ElapsedMilliseconds.ToString() + "ms")                       
                                        // ***************************************************************************************************
         