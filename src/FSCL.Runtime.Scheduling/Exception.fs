namespace FSCL.Runtime.Scheduling

open System 

type ExpressionCounterError(msg: string) =
    inherit Exception(msg)

type MetricEvaluationError(msg: string) =
    inherit Exception(msg)

type MetricProfilingError(msg: string) =
    inherit Exception(msg)

type MalformedKernelError(msg: string) =
    inherit Exception(msg)


