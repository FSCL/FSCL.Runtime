// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open FSCL.Compiler
open FSCL.Compiler.KernelLanguage
// The plugin
open FSCL.Compiler.Steps.AcceleratedArray
open System.Reflection

[<ReflectedDefinition>]
let incr x =
    x + 1

[<EntryPoint>]
let main argv = 
    let compiler = new CompilerPipeline(true, [ Assembly.GetAssembly(typeof<AcceleratedArrayParser>) ])
    let r = compiler.Run(<@ Array.map(incr) @>)
    printfn "%A" argv
    0 // return an integer exit code
