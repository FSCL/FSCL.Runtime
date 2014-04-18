open FSCL.Compiler.Language
open FSCL.Runtime.Language
open FSCL.Runtime
open FSCL.Runtime.MetricTools
open System
open Cloo
// Vector addition
[<Device(0,0)>][<ReflectedDefinition>]
let VectorAdd(a: float32[], b: float32[], c: float32[]) =
    let gid = get_global_id(0)
    c.[gid] <- a.[gid] + b.[gid]

[<EntryPoint>]
let main argv = 
    // Check opencl devices
    let plats = ListDevices()
    if plats.Count = 0 then
        Console.WriteLine("No OpenCL-enabled device found on this platform")
    else
        // Show OpenCL devices
        Console.WriteLine("Your OpenCL-enabled devices are listed below")
        
        for platformIndex = 0 to plats.Count - 1 do
            Console.WriteLine("- Platform " + platformIndex.ToString())
            for deviceName in plats.[platformIndex] do
                Console.WriteLine("  - Device " + ": " + deviceName)

    // Min and max size
    let minVectorSize = 1 <<< 10
    let maxVectorSize = 16 <<< 20
    let perTestDuration = 7000.0

    // Vector add
    Console.WriteLine(":::::::::::::::: Vector Addition ::::::::::::::::")
    for platformIndex = 0 to plats.Count - 1 do
        for deviceIndex = 0 to plats.[platformIndex].Count - 1 do
            let device = plats.[platformIndex].[deviceIndex]
            Console.WriteLine("- Device " + ": " + device)
            
            // First call causes compilation
            let a = Array.create 64 512.0f
            let b = Array.create 64 512.0f
            let c = Array.zeroCreate<float32> 64

            // Test begins
            let size = ref minVectorSize
            while !size <= maxVectorSize do
                let a = Array.create !size 512.0f
                let b = Array.create !size 512.0f
                let c = Array.zeroCreate<float32> !size
                let localSize = 64L
                
                // Pre computation
                <@ DEVICE(platformIndex, deviceIndex, VectorAdd(a, b, c)) @>.Run([| !size |> int64 |], [| localSize |])
                let correctResult = Array.create !size 1024.0f
                let mutable i = 0
                let mutable found = false
                while not found && i < !size do  
                    if correctResult.[i] <> c.[i] then
                        found <- true
                    else
                        i <- i + 1
                if found then
                    Console.WriteLine("[[ RESULT ERROR: " + i.ToString() + " ]]")

                // Computation
                let c = Array.zeroCreate<float32> !size
                let duration, iterations = Tools.ExcuteFor (perTestDuration) (fun() ->
                    <@ DEVICE(platformIndex, deviceIndex, VectorAdd(a, b, c)) @>.Run([| !size |> int64 |], [| localSize |]))
                
                Console.WriteLine(" - " + size.Value.ToString() + " elements: " + duration.ToString() + "ms (" + iterations.ToString() + " iterations)")
                size := !size * 4

    Console.Read() |> ignore
    0

            
            

