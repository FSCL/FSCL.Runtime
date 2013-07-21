namespace FSCL.Runtime

type KernelDiscoveryException(msg: string) =
    inherit System.Exception(msg)

type KernelCompilationException(msg: string) =
    inherit System.Exception(msg)
    
type KernelTargetCodeGenerationException(msg: string) =
    inherit System.Exception(msg)

