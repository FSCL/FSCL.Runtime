namespace FSCL.Runtime.KernelExecution
open System
open FSCL.Compiler
open FSCL.Runtime
open System.Collections.Generic
open Cloo
open System.Reflection

type KernelExecutionInput(isRoot: bool,  
                          node: FlowGraphNode,  
                          runtimeInfo: Dictionary<MethodInfo, RuntimeDeviceData * RuntimeKernelData * RuntimeCompiledKernelData>,
                          gSize: int64 array, 
                          lSize: int64 array) =

    member this.IsRoot 
        with get() =
            isRoot
    
    member this.Node 
        with get() =
            node

    member this.GlobalSize 
        with get() =
            gSize

    member this.LocalSize 
        with get() =
            lSize

    member this.RuntimeInfo =
        runtimeInfo

type KernelExecutionOutput =
        
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


    