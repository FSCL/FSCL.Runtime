namespace FSCL.Runtime

open System
open FSCL.Compiler
open System.Collections.Generic
open Cloo
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
    val returnValue:obj option
    val returnBuffer:ComputeMemory option
    
    new() = { returnBuffer = None; returnValue = None }
    new(rb) = { returnBuffer = Some(rb); returnValue = None }    
    new(ro) = { returnBuffer = None; returnValue = Some(ro) }

    member this.ReturnBuffer 
        with get() =
            this.returnBuffer
            
    member this.ReturnValue
        with get() =
            this.returnValue


    