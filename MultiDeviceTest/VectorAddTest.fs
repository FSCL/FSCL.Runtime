module VectorAddTest

open Utils
open FSCL.Compiler
open FSCL.Compiler.Language
open FSCL.Runtime
open FSCL.Runtime.Language
open System
open System.IO

// Vector addition
[<ReflectedDefinition>]
let VectorAdd(a: float32[], b: float32[], c: float32[]) =
    let gid = get_global_id(0)
    c.[gid] <- a.[gid] + b.[gid]

let Verify(r: float32[]) =
    let v = Array.tryFind(fun (it:float32) -> it <> 50.0f) r
    v.IsNone
        
let DoTest(minSize: long, maxSize: long, timePerIter: float) =
    let inputReadModes = [ BufferReadMode.EnqueueReadBuffer; BufferReadMode.MapBuffer ]
    let outputWriteModes = [ BufferWriteMode.EnqueueWriteBuffer; BufferWriteMode.MapBuffer ]
    let inputFlags = [ MemoryFlags.ReadOnly;
                       MemoryFlags.AllocHostPointer ||| MemoryFlags.ReadOnly;
                       MemoryFlags.UsePersistentMemAMD ||| MemoryFlags.ReadOnly ]
    let outputFlags = [ MemoryFlags.WriteOnly;
                        MemoryFlags.AllocHostPointer ||| MemoryFlags.WriteOnly;
                        MemoryFlags.UsePersistentMemAMD ||| MemoryFlags.WriteOnly ]

    use wr = new StreamWriter("VectorAdd.csv", false)
    wr.WriteLine("PLATFORM;DEVICE;READ MODE;WRITE MODE;INPUT FLAGS;OUTPUT FLAGS;SIZE;TIME;ITERATIONS")

    for pIndex, pName, devs in GetOpenCLPlatforms() do        
        Console.WriteLine("Platform: " + pName)
        for dIndex, dName, dType in devs do
            Console.WriteLine(" Device " + ": " + dName + "(" + dType.ToString() + ")")
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
                                let ttime, iters = Utils.ExcuteFor timePerIter (fun () -> comp.Run(!size, 64L))
                                
                                Console.WriteLine("---------------- " + String.Format("{0,11:######0.0000}", ttime) + "ms (" + String.Format("{0,10:#########0}", iters) + " iterations)")
                                let s = String.Format("{0};{1};{2};{3};{4};{6};{7}", 
                                                pIndex, 
                                                (dIndex.ToString() +  " " + dName), 
                                                rm, wm, ifl, ofl, 
                                                String.Format("{0,10:0000000000}", !size),
                                                String.Format("{0,11:000000.0000}", ttime),
                                                String.Format("{0,10:0000000000}", iters))
                                wr.WriteLine(s)
                                wr.Flush()

                                size := !size * 2L
                                    
                    