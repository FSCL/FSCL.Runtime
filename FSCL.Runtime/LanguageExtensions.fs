namespace FSCL.Runtime
open System

type DeviceAttribute(platform: int, device: int) =
    inherit Attribute()

    member val Platform = platform with get
    member val Device = device with get

module HostFunctions =
    let notused b =
        b


