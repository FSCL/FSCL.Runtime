namespace FSCL.Runtime
open System

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
    
type KernelSetupException(msg: string) =
    inherit System.Exception(msg)

type KernelQueryException(msg: string) =
    inherit System.Exception(msg)

type FeatureExtractionException(msg: string) =
    inherit Exception(msg)

type FeatureEvaluationException(msg: string) =
    inherit Exception(msg)

type SampleTrainginException(msg: string) =
    inherit Exception(msg)