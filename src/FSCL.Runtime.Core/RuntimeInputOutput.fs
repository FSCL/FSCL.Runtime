namespace FSCL.Runtime

open System
open FSCL.Compiler
open System.Collections.Generic
open OpenCL
open System.Reflection
open Microsoft.FSharp.Quotations
open FSCL.Language

type OpenCLKernelCreationResult(deviceData: RuntimeDevice,
                                kernelData: RuntimeKernel,
                                runtimeKernelData: RuntimeCompiledKernel) =
    member val DeviceData = deviceData with get
    member val KernelData = kernelData with get
    member val CompiledKernelData = runtimeKernelData with get
    
type MultithreadKernelCreationResult(kernelData: IKernelInfo) =
    member val KernelData = kernelData with get
    
type ComputationCreationResult =
| OpenCLKernel of OpenCLKernelCreationResult
| MultithreadKernel of MultithreadKernelCreationResult

type ExecutionOutput =     
    | ReturnedTrackedBuffer of OpenCLBuffer * Array
    | ReturnedUntrackedBuffer of OpenCLBuffer
    | ReturnedValue of obj


    