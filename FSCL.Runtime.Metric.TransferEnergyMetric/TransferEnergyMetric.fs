namespace FSCL.Runtime.Metric.TransferEnergyMetric

open FSCL.Runtime.MetricTools
open FSCL.Runtime.Metric

open Cloo
open Microsoft.FSharp.Quotations
open System
open Microsoft.FSharp.Reflection
open System.Reflection
open System.Collections.Generic
open Microsoft.FSharp.Linq.QuotationEvaluation
open System.Diagnostics
open EnergyPatterns.RemoteAmmeter
open System.Runtime.InteropServices
open System.IO

type EnergyProfilingResult = (int * double) list
type EnergyInstantiationResult = double
type EnergyEvaluationResult = Dictionary<ParameterInfo, BufferAccess>
type EnergyCustomData = obj

type TransferEnergyMetric(ammeterIp:string) =
    inherit AbsoluteSchedulingMetric<int * int, EnergyProfilingResult, EnergyEvaluationResult, EnergyInstantiationResult, EnergyCustomData>()

    let mutable min_size = 1
    let mutable max_size = 1
    let mutable step = 1
    let mutable per_step_duration = 0.0
    let mutable validate = false
    let mutable sourceInfo = new TransferEndpoint()
    let mutable destInfo = new TransferEndpoint()
    let mutable dumpFile = None

    member val AmmeterIp = ammeterIp with get, set
    
    member this.DumpFolder
        with get() = dumpFile
        and set v = dumpFile <- v
    
    member this.MinSize
        with get() = min_size
        and set instr = min_size <- instr

    member this.MaxSize
        with get() = max_size
        and set instr = max_size <- instr
        
    member this.Step 
        with get() = step
        and set instr = step <- instr

    member this.PerStepDuration 
        with get() = per_step_duration
        and set duration = per_step_duration <- duration
       
    member this.SrcInfo 
        with get() = sourceInfo
        and set info = sourceInfo <- info

    member this.DstInfo 
        with get() = destInfo
        and set info = destInfo <- info

    member this.Validate 
        with get() = validate
        and set valid = validate <- valid

    override this.Profile((plat, dev)) =
        // Create result
        let mutable result = []

        // Setup CL
        let computePlatform = ComputePlatform.Platforms.[plat];
        let device = computePlatform.Devices.[dev];
        let contextProperties = new ComputeContextPropertyList(computePlatform)
        let devices = new System.Collections.Generic.List<ComputeDevice>();
        devices.Add(device)
        let computeContext = new ComputeContext(devices, contextProperties, null, System.IntPtr.Zero);
        let computeQueue = new ComputeCommandQueue(computeContext, device, ComputeCommandQueueFlags.OutOfOrderExecution)
            
        // Calculate list of buffer sizes
        let bufferSizes = seq { 
                                let i = ref this.MinSize
                                while !i <= this.MaxSize do 
                                    yield !i
                                    i := !i + this.Step
                                }

        // For each instr count run the test of allocation, initialization and transferring
        for currSize in bufferSizes do
            // Allocate and init src, allocate dst
            let srcPtr = ref None
            let dstPtr = ref None
            let srcBuffer = ref None
            let dstBuffer = ref None
            if this.SrcInfo.IsHostPtr then
                srcPtr := Some(TransferTools.AllocateHostPtr(currSize))
                do TransferTools.InitializeHostPtr(currSize, (!srcPtr).Value)
            else
                srcBuffer := Some(TransferTools.AllocateBuffer(computeContext, currSize, this.SrcInfo))
                do TransferTools.InitializeBuffer(computeQueue, currSize, this.SrcInfo, (!srcBuffer).Value)
            if this.DstInfo.IsHostPtr then
                dstPtr := Some(TransferTools.AllocateHostPtr(currSize))
            else
                dstBuffer := Some(TransferTools.AllocateBuffer(computeContext, currSize, this.DstInfo))

            // Run kernel n times to guarantee a total time >= PerStepDuration
            let (endMessage, time, iterations) = Tools.GetEnergyConsumption (this.AmmeterIp) (this.PerStepDuration) (fun () ->
                if this.SrcInfo.IsHostPtr then
                    if this.DstInfo.IsHostPtr then
                        TransferTools.HostPtrToHostPtr(currSize, this.Validate, (!srcPtr).Value, (!dstPtr).Value)
                    else
                        TransferTools.HostPtrToBuffer(computeContext, computeQueue, currSize, this.Validate, this.DstInfo, (!srcPtr).Value, (!dstBuffer).Value)
                elif this.DstInfo.IsHostPtr then
                    TransferTools.BufferToHostPtr(computeContext, computeQueue, currSize, this.Validate, this.SrcInfo, (!srcBuffer).Value, (!dstPtr).Value)  
                else  
                    TransferTools.BufferToBuffer(computeContext, computeQueue, currSize, this.Validate, this.SrcInfo, this.DstInfo, (!srcBuffer).Value, (!dstBuffer).Value))    
                
            // Calculate energy per byte transferred
            let avgEnergy = Double.Parse(endMessage.Replace(",", "."))
            let energyPerByte = ((avgEnergy / 1000.0) * (double)time) / ((double)currSize)
            result <- result @ [ (currSize, energyPerByte) ]
                
        // Dump on file if enable
        if dumpFile.IsSome then
            if not (Directory.Exists(dumpFile.Value)) then
                Directory.CreateDirectory(dumpFile.Value) |> ignore
            let fileName = dumpFile.Value + "\\" + "Profiling-" + this.GetType().Name + "-" + device.Name.Replace(' ', '_') + ".csv"  
            let content = ref "TransferSize,EnergyPerByte;"
            List.iter (fun (instr:int,en:float) ->
                content := !content + instr.ToString() + "," + en.ToString() + ";") result
            File.WriteAllText(fileName, !content)

        result

    override this.Evaluate(profiling, expr:Expr) =
        let kernel = KernelTools.ExtractKernelDefinition(expr)
        let parmsAccess = AccessAnalyzer.Analyze(kernel)
        parmsAccess
                
    override this.Instantiate(profiling, evaluation, invocation, customData) =            
        let (methodInfo, args) = KernelTools.ExtractKernelInvocation(invocation)
        let parameters = methodInfo.GetParameters()
        let totalBytes = ref 0
        Array.iteri (fun i (p:ParameterInfo) ->
            if p.ParameterType.IsArray then
                let v = args.[i].EvalUntyped()
                // Get length
                let elements = p.ParameterType.GetProperty("Length").GetValue(v) :?> int
                totalBytes := !totalBytes + (elements * Marshal.SizeOf(p.ParameterType.GetElementType()))) parameters
                
        // Dump on file if enable
        if dumpFile.IsSome then
            if not (Directory.Exists(dumpFile.Value)) then
                Directory.CreateDirectory(dumpFile.Value) |> ignore

            let fileName = dumpFile.Value + "\\" + "Instatiate-" + this.GetType().Name + "-" + methodInfo.Name + ".csv"  
            let content = ref ("TransferSize;" + ((double)!totalBytes).ToString())
            File.WriteAllText(fileName, !content)

        (double)!totalBytes
                
            

