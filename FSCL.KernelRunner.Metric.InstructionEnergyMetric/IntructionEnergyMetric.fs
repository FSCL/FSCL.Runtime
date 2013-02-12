namespace FSCL.KernelRunner.Metric.InstructionEnergyMetric

open FSCL.KernelRunner.Metric
open Cloo
open Microsoft.FSharp.Quotations
open System
open Microsoft.FSharp.Reflection
open EnergyPatterns.RemoteAmmeter
open FSCL.KernelRunner.MetricTools
open System.IO

// The below one needs PowerPack :(
open Microsoft.FSharp.Linq.QuotationEvaluation

type EnergyProfilingResult = (int * int64 * double * double * int64 * int) list
type EnergyInstantiationResult = (double * double * double)
type EnergyEvaluationResult = (Expr * Expr * Expr)
// Local and global sizes
type EnergyCustomData = int array * int array

type InstructionEnergyMetric(ammeterIp) =
    inherit AbsoluteMetric<int * int, EnergyProfilingResult, EnergyEvaluationResult, EnergyInstantiationResult, EnergyCustomData>()

    let mutable min_instr = 1
    let mutable max_instr = 1
    let mutable min_thread = 1L
    let mutable max_thread = 1L
    let mutable instr_step = (fun (i:int) -> i * 2)
    let mutable thread_step = (fun (i:int64) -> i * 2L)
    let mutable per_step_duration = 0.0

    member val AmmeterIp = ammeterIp with get, set

    member this.MinInstr 
        with get() = min_instr
        and set instr = min_instr <- instr

    member this.MaxInstr 
        with get() = max_instr
        and set instr = max_instr <- instr
        
    member this.MinThread 
        with get() = min_thread
        and set th = min_thread <- th

    member this.MaxThread 
        with get() = max_thread
        and set th = max_thread <- th

    member this.InstrStep 
        with get() = instr_step
        and set instr = instr_step <- instr
        
    member this.ThreadStep 
        with get() = thread_step
        and set instr = thread_step <- instr

    member this.PerStepDuration 
        with get() = per_step_duration
        and set duration = per_step_duration <- duration

    member private this.OnNewOperation (counter:Counter) (expr, parameters:Reflection.ParameterInfo[]) =
        match expr with
        | DerivedPatterns.SpecificCall <@ (+) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (-) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (*) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (/) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (%) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (&&) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (||) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (&&&) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (|||) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (<<<) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (>>>) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (^^^) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (~-) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (~+) @> (e, t, a) ->
            let result = ref <@ 1.0 @>
            for arg in a do
                let sub = counter.ContinueCount(arg)
                result := <@ (%(!result) : double) + (%%sub : double) @>
            Value(!result)
        | _ ->
            Continue

    member private this.OnNewRead (counter:Counter) (expr, parameters:Reflection.ParameterInfo[]) =
        match expr with
        | Patterns.Call(o, mi, args) ->            
            if mi.DeclaringType.Name = "IntrinsicFunctions" && mi.Name.StartsWith "GetArray" then
                match args.[0] with
                | Patterns.Var(v) ->
                    let result = ref <@ 1.0 @>
                    for arg in args do
                        let sub = counter.ContinueCount(arg)
                        result := <@ (%(!result) : double) + (%%sub : double) @>
                    Value(!result)
                | _ ->
                    Continue
            else
                Continue
        | _ ->
            Continue
                
    member private this.OnNewWrite (counter:Counter) (expr, parameters:Reflection.ParameterInfo[]) =
        match expr with
        | Patterns.Call(o, mi, args) ->            
            if mi.DeclaringType.Name = "IntrinsicFunctions" && mi.Name.StartsWith "SetArray" then
                match args.[0] with
                | Patterns.Var(v) ->
                    let result = ref <@ 1.0 @>
                    for arg in args do
                        let sub = counter.ContinueCount(arg)
                        result := <@ (%(!result) : double) + (%%sub : double) @>
                    Value(!result)
                | _ ->
                    Continue
            else
                Continue
        | _ ->
            Continue     

    override this.Profile((platform, dev)) =

        let mutable result = []
        
        // Setup CL
        let computePlatform = ComputePlatform.Platforms.[platform]
        let device = computePlatform.Devices.[dev]
        let contextProperties = new ComputeContextPropertyList(computePlatform)
        let devices = new System.Collections.Generic.List<ComputeDevice>();
        devices.Add(device)
        let computeContext = new ComputeContext(devices, contextProperties, null, System.IntPtr.Zero);
            
        // Calculate list of thread count
        let threadCount = List.ofSeq(seq { 
                                let i = ref this.MinThread
                                while !i <= this.MaxThread do 
                                    yield !i
                                    i := this.ThreadStep !i
                                })
        
        // Calculate list of instr count
        let instrCount = List.ofSeq(seq { 
                                let i = ref this.MinInstr
                                while !i <= this.MaxInstr do 
                                    yield !i
                                    i := this.InstrStep !i
                                })

        // For each instr count run the test
        for currInstr in instrCount do
            for currThread in threadCount do
                let computeProgram = new ComputeProgram(computeContext, [| KernelBuilder.BuildLoopKernel() |])
                computeProgram.Build(devices, "", null, System.IntPtr.Zero)
                let computeKernel = computeProgram.CreateKernel("run")
                let computeQueue = new ComputeCommandQueue(computeContext, device, ComputeCommandQueueFlags.OutOfOrderExecution)
                let inputBuffer = new ComputeBuffer<float>(computeContext, ComputeMemoryFlags.ReadOnly, 4L)
                let outputBuffer = new ComputeBuffer<float>(computeContext, ComputeMemoryFlags.WriteOnly, 4L)
                computeKernel.SetMemoryArgument(0, inputBuffer)
                computeKernel.SetMemoryArgument(1, outputBuffer)
                computeQueue.WriteToBuffer([| 1.0 |], inputBuffer, true, null) 
                // Only for loop kernel
                computeKernel.SetValueArgument(2, currInstr / 2)

                // Run kernel n times to guarantee a total time >= PerStepDuration
                let (endMessage, time, iterations) = Tools.GetEnergyConsumption (this.AmmeterIp) (this.PerStepDuration) (fun () ->
                    computeQueue.Execute(computeKernel, [| 0L |], [| currThread |], [|  Math.Min(128L, currThread) |], null) 
                    computeQueue.Finish())

                // Energy per instruction
                let v = ref 0.0
                let avgEnergy = Double.TryParse(endMessage.Replace(",", "."), v)
                match avgEnergy with
                | true ->
                    let energyPerInstr = ((!v / 1000.0) * (double)time) / ((double)currInstr)
                    result <- result @ [ (currInstr, currThread, !v, energyPerInstr, time, iterations) ]
                | false ->
                    let t = 0
                    ()

        // Dump on file if enable 
        let content = ref "Instructions,Threads,AvgEnergy,EnergyPerInstruction,Duration,Iterations\n"
        List.iter (fun (instr:int,thread:int64,avgen,en:float,time,iterations) ->
            content := !content + instr.ToString() + "," + thread.ToString() + "," + avgen.ToString() + "," + en.ToString() + "," + time.ToString() + "," + iterations.ToString() + ";\n") result
        this.Dump("Profiling-" + this.GetType().Name + "-" + device.Name.Replace(' ', '_') + ".csv", !content) 
        result
            
    override this.Evaluate(profiling, expr:Expr) =
        let kernel = KernelTools.ExtractKernelDefinition(expr)
        let counter = new Counter(kernel)
        let instructions = counter.Count(this.OnNewOperation counter, true)
        let reads = counter.Count(this.OnNewRead counter, false)
        let writes = counter.Count(this.OnNewWrite counter, false)
        (instructions, reads, writes)
            
                
    override this.Instantiate(profiling, evaluations, invocation, (globalSize, localSize)) =
        // Evaluation is something like "<compute instruction>"
        // To compute instructions we bind the variables that are free in <compute instruction>
        let (methodInfo, args) = KernelTools.ExtractKernelInvocation(invocation)
        let parameters = methodInfo.GetParameters()
        match evaluations with
        | (instructions, reads, writes) ->
            let result = List.map(fun (evaluation: Expr) ->
                            let mutable freeVars = evaluation.GetFreeVars()

                            // Assign values to parameter references
                            let mutable finalExpr = evaluation
                            for var in freeVars do
                                let pIndex = Array.tryFindIndex (fun (p:Reflection.ParameterInfo) -> p.Name = var.Name) parameters
                                if pIndex.IsSome then
                                    finalExpr <- Expr.Let(var, args.[pIndex.Value], finalExpr)
                                    // Remove var free ones                
                                    freeVars <- Seq.skip 1 freeVars
                                      
                            // Assign values to fscl call placeholders
                            let groups = Array.mapi (fun i el ->  el / localSize.[i]) globalSize
                            for var in freeVars do
                                if var.Name.StartsWith "get_global_id" then
                                    finalExpr <- Expr.Let(var, <@ Array.zeroCreate<int> 3 @>, finalExpr)
                                if var.Name.StartsWith "get_local_id" then
                                    finalExpr <- Expr.Let(var, <@ Array.zeroCreate<int> 3 @>, finalExpr)
                                if var.Name.StartsWith "get_global_size" then
                                    finalExpr <- Expr.Let(var, <@ globalSize @>, finalExpr)
                                if var.Name.StartsWith "get_local_size" then
                                    finalExpr <- Expr.Let(var, <@ localSize @>, finalExpr)
                                if var.Name.StartsWith "get_num_groups" then 
                                    finalExpr <- Expr.Let(var, <@ groups @>, finalExpr)
                                if var.Name.StartsWith "get_work_dim" then 
                                    finalExpr <- Expr.Let(var, <@ groups.Rank @>, finalExpr)           
                
                            let result = finalExpr.EvalUntyped()

                            result :?> double) [ instructions; reads; writes ]
        
            // Dump on file if enable
            let mutable content = String.concat "," (Seq.ofList (List.mapi (fun i (e:Expr) -> "Parameter" + i.ToString()) args)) + ";\n"
            content <- content + String.concat "," (Seq.ofList (List.map (fun (e:Expr) -> e.ToString()) args)) + ";\n"
            content <- content + String.concat "," (seq { for i = 0 to globalSize.Length - 1 do yield "Global size " + i.ToString() }) + ";\n"
            content <- content + String.concat "," (seq { for i = 0 to globalSize.Length - 1 do yield globalSize.[i].ToString() }) + ";\n"
            content <- content + String.concat "," (seq { for i = 0 to localSize.Length - 1 do yield "Local size " + i.ToString() }) + ";\n"
            content <- content + String.concat "," (seq { for i = 0 to localSize.Length - 1 do yield localSize.[i].ToString() }) + ";\n"
            content <- content + "Instructions,Reads,Writes;\n"
            content <- content + result.[0].ToString() + "," + result.[1].ToString() + "," + result.[2].ToString() + ";\n"
            this.Dump("Instantiate-" + this.GetType().Name + "-" + methodInfo.Name + ".csv", content)

            (result.[0], result.[1], result.[2])
            
