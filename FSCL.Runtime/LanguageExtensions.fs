namespace FSCL.Runtime
open System
open System.Collections.Generic

// Kernel run mode
type KernelRunningMode =
| OpenCL
| Multithread
| Sequential

type DeviceAttribute(platform: int, device: int) =
    inherit Attribute()

    member val Platform = platform with get
    member val Device = device with get

module HostFunctions =
    let notused b =
        b
        
        


