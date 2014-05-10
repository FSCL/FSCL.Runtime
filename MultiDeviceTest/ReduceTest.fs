module ReduceTest

open Utils
open FSCL.Compiler
open FSCL.Compiler.Language
open FSCL.Runtime
open FSCL.Runtime.Language
open System
open System.IO
open System.Diagnostics

let Verify(r: float32[], result: float32) =
    (Array.reduce (fun a b -> a + b) r) = result

[<ReflectedDefinition>]
let sum a b =
    a + b
            
let DoTest(minSize: long, maxSize: long, iters: int) =
    let inputReadModes = [ BufferReadMode.EnqueueReadBuffer; BufferReadMode.MapBuffer ]
    let outputWriteModes = [ BufferWriteMode.EnqueueWriteBuffer; BufferWriteMode.MapBuffer ]
    let inputFlags = [ MemoryFlags.HostWriteOnly ||| MemoryFlags.UseHostPointer ||| MemoryFlags.ReadOnly;
                       MemoryFlags.HostWriteOnly ||| MemoryFlags.UsePersistentMemAMD ||| MemoryFlags.ReadOnly; 
                       MemoryFlags.HostWriteOnly ||| MemoryFlags.ReadOnly; ]
    let outputFlags = [ MemoryFlags.HostReadOnly ||| MemoryFlags.UseHostPointer;
                        MemoryFlags.HostReadOnly ||| MemoryFlags.UsePersistentMemAMD;
                        MemoryFlags.HostReadOnly ]

    let wr = new StreamWriter("VectorReduce.csv", false)
    wr.WriteLine("PLATFORM;DEVICE;READ MODE;WRITE MODE;INPUT FLAGS;OUTPUT FLAGS;SIZE;TIME;ITERATIONS")

    for pIndex, pName, pDevs in GetOpenCLPlatforms() do        
        Console.WriteLine("Platform: " + pName.ToString())
        for dIndex, dName, dType in pDevs do
                Console.WriteLine(" Device " + ": " + dName.ToString() + "(" + dType.ToString() + ")")
                
                for rm in inputReadModes do
                    Console.WriteLine("  ReadMode: " + rm.ToString())
                    for wm in outputWriteModes do
                        Console.WriteLine("   WriteMode: " + wm.ToString())
                        for ifl in inputFlags do
                            Console.WriteLine("    Input flags: " + ifl.ToString())
                            for ofl in outputFlags do
                                Console.WriteLine("     Output flags: " + ofl.ToString())
                                let size = ref minSize
                                while !size <= maxSize do
                                    Console.WriteLine("      Size: " + String.Format("{0,10:##########}", !size))
                                    if dType <> DeviceType.Cpu || ((ifl &&& MemoryFlags.UsePersistentMemAMD |> int = 0) && (ofl &&& MemoryFlags.UsePersistentMemAMD |> int = 0)) then
                                        let a = Array.zeroCreate<float32> (!size |> int)
                                        for i = 0 to (!size |> int) - 1 do
                                            a.[i] <- 1.0f

                                        let mutable minSizeForCPU = 1L
                                        //while minSizeForCPU < !size / 2L do
                                        Console.WriteLine("      Fallback on CPU when size is: " + String.Format("{0,10:##########}", minSizeForCPU))
                                        let comp = <@ 
                                                            DEVICE(pIndex, dIndex,
                                                                RETURN_BUFFER_READ_MODE(rm, 
                                                                    RETURN_BUFFER_WRITE_MODE(wm,
                                                                        RETURN_MEMORY_FLAGS(ofl,
                                                                            Array.reduce 
                                                                                sum 
                                                                                (BUFFER_READ_MODE(rm, 
                                                                                    BUFFER_WRITE_MODE(wm,
                                                                                        MEMORY_FLAGS(ifl,
                                                                                            a))))))))
                                                    @>
                                        // Run once to skip compilation time
                                        let c = comp.Run(!size, 128L)
                                        if not (Verify(a, c)) then
                                            Console.WriteLine("---------------- COMPUTATION RESULT ERROR")
                                        else
                                            // Run
                                            let watch = new Stopwatch()
                                            watch.Start()
                                            for i = 0 to iters - 1 do
                                                comp.Run(!size, 128L) |> ignore
                                            watch.Stop()
                                            let ttime, iters = ((double)watch.ElapsedMilliseconds) /((double)iters), iters
                                            //let ttime, iters = Utils.ExcuteFor 1000.0 (fun () -> comp.Run(!size, 64L))
                                
                                            Console.WriteLine("---------------- " + String.Format("{0,11:######0.0000}", ttime) + "ms (" + String.Format("{0,10:#########0}", iters) + " iterations)")
                                            let s = String.Format("{0};{1};{2};{3};{4};{5};{6};{7};{8}", 
                                                            pIndex, 
                                                            (dIndex.ToString() +  " " + dName.ToString()), 
                                                            rm, wm, ifl, ofl, 
                                                            !size,
                                                            ttime,
                                                            iters)
                                            wr.WriteLine(s)
                                            System.Threading.Thread.Sleep(2000)
    
                                            //minSizeForCPU <- minSizeForCPU * 2L
                                    size := !size * 2L                                    
    wr.Close()
                    