namespace FSCL.Runtime

type KernelCompilationException(msg: string) =
    inherit System.Exception(msg)
   
type KernelTargetCodeGenerationException(msg: string) =
    inherit System.Exception(msg)

type KernelExecutionException(msg: string) =
    inherit System.Exception(msg)

type KernelDeviceSelectionException(msg: string) =
    inherit System.Exception(msg)

type KernelSchedulingException(msg: string) =
    inherit System.Exception(msg)
    
type KernelFlowGraphException(msg: string) =
    inherit System.Exception(msg)

type KernelSetupException(msg: string) =
    inherit System.Exception(msg)

