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
    let v = Array.tryFind(fun (it:float32) -> it <> 50.0f) r
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

    let wr = new StreamWriter("VectorAddition.csv", false)
    wr.WriteLine("PLATFORM;DEVICE;READ MODE;WRITE MODE;INPUT FLAGS;OUTPUT FLAGS;SIZE;TIME;ITERATIONS")
    wr.Close()
    wr.Dispose()

    for pIndex = 0 to 0 do        
        Console.WriteLine("Platform: " + pIndex.ToString())
        for dIndex = 0 to 2 do
            if dIndex >= 0 then
                Console.WriteLine(" Device " + ": " + dIndex.ToString() + "(" + dIndex.ToString() + ")")
                let wr = new StreamWriter("VectorAdd.csv", true)

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
                                    let a = Array.create (!size |> int) 16.0f
                                    let b = Array.create (!size |> int) 34.0f
                                    let c = Array.zeroCreate<float32> (!size |> int)

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
                                    comp.Run(!size, 64L)
                                    if not (Verify(c)) then
                                        Console.WriteLine("---------------- COMPUTATION RESULT ERROR")
                                    
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
                                                    (dIndex.ToString() +  " " + dIndex.ToString()), 
                                                    rm, wm, ifl, ofl, 
                                                    !size,
                                                    ttime,
                                                    iters)
                                    wr.WriteLine(s)

                                    size := !size * 2L
                                    
                wr.Close()
                    