namespace FSCL.KernelRunner.Metric.TransferEnergyMetric

open Cloo

type BufferAccess =
| READ_ONLY
| WRITE_ONLY
| READ_WRITE
| NO_ACCESS

type TransferEndpoint() = 
    let isHostPtr = true
    let flags = ComputeMemoryFlags.None
    let shouldMap = false

    member val IsHostPtr = isHostPtr with get, set
    member val Flags = flags with get, set
    member val ShouldMap = shouldMap with get, set

