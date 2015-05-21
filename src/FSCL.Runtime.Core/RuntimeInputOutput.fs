namespace FSCL.Runtime

open System
open FSCL.Compiler
open System.Collections.Generic
open OpenCL
open System.Reflection
open Microsoft.FSharp.Quotations
open FSCL.Language

type OpenCLKernelCreationResult(kernel: IKernelModule,
                                deviceData: RuntimeDevice,
                                runtimeKernelData: RuntimeCompiledKernel) =
    member val KernelData = kernel with get
    member val DeviceData = deviceData with get
    member val CompiledKernelData = runtimeKernelData with get
    
type MultithreadKernelCreationResult(kernel: IKernelModule) =
    member val KernelData = kernel with get
    
type ComputationCreationResult =
| OpenCLKernel of OpenCLKernelCreationResult
| MultithreadKernel of MultithreadKernelCreationResult

type ExecutionOutput =     
    | ReturnedBuffer of OpenCLBuffer
    | ReturnedValue of obj


    