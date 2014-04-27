namespace FSCL.Runtime

open System
open FSCL.Compiler
open System.Collections.Generic
open OpenCL
open System.Reflection
open Microsoft.FSharp.Quotations
open FSCL.Runtime.Language

type KernelCreationResult(callArgs: Expr list,
                          deviceData: RuntimeDevice,
                          kernelData: RuntimeKernel,
                          targetData: RuntimeCompiledKernel) =

    member val CallArgs = callArgs with get
    member val DeviceData = deviceData with get
    member val KernelData = kernelData with get
    member val CompiledKernelData = targetData with get

type ExecutionOutput =     
    | ReturnedTrackedBuffer of OpenCLBuffer * Array
    | ReturnedUntrackedBuffer of OpenCLBuffer
    | ReturnedValue of obj


    