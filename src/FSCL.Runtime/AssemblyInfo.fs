namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FSCL.Runtime")>]
[<assembly: AssemblyProductAttribute("FSCL.Runtime")>]
[<assembly: AssemblyDescriptionAttribute("FSCL kernels scheduling and execution layer for heterogeneous platforms")>]
[<assembly: AssemblyVersionAttribute("1.3.6")>]
[<assembly: AssemblyFileVersionAttribute("1.3.6")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.3.6"
