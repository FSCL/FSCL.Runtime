namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FSCL.Runtime.Scheduling")>]
[<assembly: AssemblyProductAttribute("FSCL.Runtime")>]
[<assembly: AssemblyDescriptionAttribute("FSCL kernel scheduling and execution support")>]
[<assembly: AssemblyVersionAttribute("2.0.1")>]
[<assembly: AssemblyFileVersionAttribute("2.0.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "2.0.1"
