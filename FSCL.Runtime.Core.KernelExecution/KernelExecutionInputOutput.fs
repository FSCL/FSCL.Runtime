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
                          gSize: int array, 
                          lSize: int array) =

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
        
    val returnBuffers:List<obj>

    val outputBuffers:List<ComputeMemory>

    new() = { returnBuffers = new List<obj>(); outputBuffers = new List<ComputeMemory>() }

    new(buff) = { returnBuffers = buff; outputBuffers = new List<ComputeMemory>() }

    new(buff) = { returnBuffers = new List<obj>(); outputBuffers = buff }

    new(rb, ob) = { returnBuffers = rb; outputBuffers = ob }

    member this.ReturnBuffers 
        with get() =
            this.returnBuffers

    member this.OutputBuffers 
        with get() =
            this.outputBuffers
    