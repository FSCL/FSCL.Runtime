namespace FSCL.Runtime

type KernelAttributeException(msg: string) =
    inherit System.Exception(msg)

type KernelCallException(msg: string) =
    inherit System.Exception(msg)
    
type KernelDefinitionException(msg: string) =
    inherit System.Exception(msg)

type KernelBindingException(msg: string) =
    inherit System.Exception(msg)

type KernelSchedulingException(msg: string) =
    inherit System.Exception(msg)

