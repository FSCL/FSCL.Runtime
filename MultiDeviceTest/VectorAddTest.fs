module VectorAddTest

open Utils
open FSCL.Compiler
open FSCL.Compiler.Language
open FSCL.Runtime
open FSCL.Runtime.Language
open System
open System.IO
open System.Diagnostics

// Vector addition
[<ReflectedDefinition>]
let VectorAdd(a: float32[], b: float32[], c: float32[]) =
    let gid = get_global_id(0)
    c.[gid] <- a.[gid] + b.[gid]

let Verify(r: float32[]) =
    let v = Array.tryFind(fun (it:float32) -> it <> (float32)r.Length) r
    v.IsNone
        
let DoTest(minSize: long, maxSize: long, iters: int) =
    let inputReadModes = [ BufferReadMode.EnqueueReadBuffer; BufferReadMode.MapBuffer ]
    let outputWriteModes = [ BufferWriteMode.EnqueueWriteBuffer; BufferWriteMode.MapBuffer ]
    let inputFlags = [ MemoryFlags.HostWriteOnly ||| MemoryFlags.UseHostPointer ||| MemoryFlags.ReadOnly;
                       MemoryFlags.HostWriteOnly ||| MemoryFlags.UsePersistentMemAMD ||| MemoryFlags.ReadOnly; 
                       MemoryFlags.HostWriteOnly ||| MemoryFlags.ReadOnly; ]
    let outputFlags = [ MemoryFlags.HostReadOnly ||| MemoryFlags.UseHostPointer ||| MemoryFlags.WriteOnly;
                        MemoryFlags.HostReadOnly ||| MemoryFlags.UsePersistentMemAMD ||| MemoryFlags.WriteOnly;
                        MemoryFlags.HostReadOnly ||| MemoryFlags.WriteOnly; ]

    let wr = new StreamWriter("VectorAdd.csv", true)
    //wr.WriteLine("PLATFORM;DEVICE;READ MODE;WRITE MODE;INPUT FLAGS;OUTPUT FLAGS;SIZE;TIME;ITERATIONS")

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
                                        let b = Array.zeroCreate<float32> (!size |> int)
                                        let c = Array.zeroCreate<float32> (!size |> int)
                                        for i = 0 to (!size |> int) - 1 do
                                            a.[i] <- (float32)i
                                            b.[i] <- (float32)((!size |> int) - i)
                                        let comp = <@ DEVICE(pIndex, dIndex,
                                                        VectorAdd(
                                                         BUFFER_READ_MODE(rm, 
                                                            MEMORY_FLAGS(ifl, 
                                                                a)),
                                                         BUFFER_READ_MODE(rm, 
                                                            MEMORY_FLAGS(ifl, 
                                                                b)),
                                                         BUFFER_WRITE_MODE(wm, 
                                                            MEMORY_FLAGS(ofl, 
                                                                c)))) @>
                                        // Run once to skip compilation time
                                        comp.Run(!size, 128L)
                                        if not (Verify(c)) then
                                            Console.WriteLine("---------------- COMPUTATION RESULT ERROR")
                                        else
                                            // Run
                                            let watch = new Stopwatch()
                                            watch.Start()
                                            for i = 0 to iters - 1 do
                                                comp.Run(!size, 128L)
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
                                            System.Threading.Thread.Sleep(500)

                                    size := !size * 2L                                    
    wr.Close()
                    