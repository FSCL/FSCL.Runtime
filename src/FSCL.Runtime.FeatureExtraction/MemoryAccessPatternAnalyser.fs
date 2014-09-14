namespace FSCL.Runtime.Scheduling.FeatureExtraction

open FSCL
open FSCL.Compiler
open FSCL.Runtime.Scheduling
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open System.Reflection
open Microsoft.FSharp.Linq.RuntimeHelpers
//open QuotEval.QuotationEvaluation
open System
open FSCL.Compiler.Util
open FSCL.Compiler.FunctionPostprocessing
open FSCL.Language
open System.Runtime.InteropServices
open FSCL.Runtime.Scheduling.ReflectionUtil
open FSCL.Runtime

type InterThreadMemoryAccessResult() =
    member val Stride0OnXAccessCount = 0.0f with get, set
    member val Stride0OnYAccessCount = 0.0f with get, set
    member val Stride0OnZAccessCount = 0.0f with get, set
    member val Stride1OnXAccessCount = 0.0f with get, set
    member val Stride1OnYAccessCount = 0.0f with get, set
    member val Stride1OnZAccessCount = 0.0f with get, set
    member val StrideNOnXAccessCount = 0.0f with get, set
    member val StrideNOnYAccessCount = 0.0f with get, set
    member val StrideNOnZAccessCount = 0.0f with get, set

// Work item info where global ID and local ID can be set
type MutableWorkItemInfo(globalID: int64[], localID: int64[], globalSize: int64[], localSize: int64[], globalOffset: int64[]) =
    inherit WorkSize(globalSize, localSize, globalOffset)
    
    override this.GlobalID(idx) =
        globalID.[idx] |> int      
    override this.LocalID(idx) =
        localID.[idx] |> int      

type MemoryAccessPatternAnalyser() = 
    inherit IDefaultFeatureExtractor()
    override this.FeatureNameList 
        with get() =
            [ "Average intra-thread global read access stride on dim 1 (bytes)";
              "Average intra-thread global read access stride on dim 2 (bytes)";
              "Average intra-thread global read access stride on dim 3 (bytes)" ]

    override this.Precompute(m: IKernelModule) =
        // We have parameters in m with access analysis computed
        let parameters = m.Kernel.OriginalParameters |> 
                         Seq.map(fun (p: IOriginalFunctionParameter) -> 
                                    (p.OriginalParamterInfo, p.OriginalPlaceholder)) |>
                         Array.ofSeq  
        
        let acount, ph = InterThreadMemoryAccessAnalyser.EstimateMemoryAccessStride(m.Kernel.OriginalBody,
                                                                                    parameters)                                                                                    
        ([ box acount ], ph |> List.ofSeq) :> obj

    // We compute the stride for threads of the same group and of different groups
    // 1) We use the thread 0-0-0 as reference
    // 2) We compute inter-group stride using the last thread of a group and the first of the successive
    // 3) We compute stride on all the dimensions, considered independent from each other
    member private this.EvaluateStride(count: Expr, accessExpr: Expr, dynDefArgs: obj list, args: obj list) =
        let rec replaceWorkItemInfo(args: obj list, wi: WorkSize) =
            match args with
            | [] -> 
                []
            | a::tail ->
                if typeof<WorkSize>.IsAssignableFrom(a.GetType()) then
                    [ box wi ] @ tail
                else
                    [ a ] @ replaceWorkItemInfo(tail, wi)

        // AccessExpr and count are two functions with the same args of the kernel. Evaluate them
        let mutable evCount = LeafExpressionConverter.EvaluateQuotation(count)
        for a in args do
            evCount <- evCount.GetType().GetMethod("Invoke").Invoke(evCount, [| a |])
        for d in dynDefArgs do
            evCount <- evCount.GetType().GetMethod("Invoke").Invoke(evCount, [| d |])

        // Now evaluate the accessExpr changing the group and local id on all the dimensions        
        let evAccessExpr = LeafExpressionConverter.EvaluateQuotation(accessExpr)

        // Find work size argument
        let workSize = args |> List.find (fun i -> typeof<WorkSize>.IsAssignableFrom(i.GetType())) :?> WorkSize
        let dims = workSize.GlobalSize().Length
        // Evaluate access baseline (the access of the thread 0-0-0)
        let mutable baseLine = evAccessExpr
        for a in args do
            baseLine <- baseLine.GetType().GetMethod("Invoke").Invoke(baseLine, [| a |])
        for d in dynDefArgs do
            baseLine <- baseLine.GetType().GetMethod("Invoke").Invoke(baseLine, [| d |])

        // Precompute globalSize, localSize and globalOffset arrays
        let globalSize = Array.init (dims) (fun i -> workSize.GlobalSize(i) |> int64)
        let localSize = Array.init (dims) (fun i -> workSize.LocalSize(i) |> int64)
        let numGroups = Array.init (dims) (fun i -> workSize.NumGroups(i) |> int64)
        let globalOffset = 
            try 
                Array.init (dims) (fun i -> workSize.GlobalOffset(i) |> int64)
            with 
            | :? System.NullReferenceException ->
                Array.zeroCreate<int64> (dims)
                 
        // Now compute delta of accesses from other threads compared to the baseLine. 
        // We take into account threads of the same group and of different group in all the dimensions
        let strides = Array.create 3 (-1.0f, -1.0f)
        for dim = 0 to dims - 1 do
            // Compute the stride for two threads of the same group in this dimension
            let mutable strideIntraGroup = 0.0f
            let mutable strideInterGroup = 0.0f
            if workSize.LocalSize(dim) > 1 then
                let globalID = Array.init (dims) (fun i -> if i = dim then 1L else 0L)
                let localID = Array.init (dims) (fun i -> if i = dim then 1L else 0L)
                let wi = new MutableWorkItemInfo(globalID, localID, globalSize, localSize, globalOffset)
                let newArgs = replaceWorkItemInfo(args, wi)
                let mutable access = evAccessExpr
                for a in newArgs do
                    access <- access.GetType().GetMethod("Invoke").Invoke(access, [| a |])
                for d in dynDefArgs do
                    access <- access.GetType().GetMethod("Invoke").Invoke(access, [| d |])
                // Now compute delta
                strideIntraGroup <- Math.Abs((access :?> float32) - (baseLine :?> float32))
            // Compute the stride for two threads of successive groups in this dimension
            if workSize.NumGroups(dim) > 1 then
                let globalID = Array.init (dims) (fun i -> if i = dim then workSize.LocalSize(i) |> int64 else 0L)
                let localID = Array.zeroCreate<int64> (dims) 
                let wi = new MutableWorkItemInfo(globalID, localID, globalSize, localSize, globalOffset)
                let newArgs = replaceWorkItemInfo(args, wi)
                let mutable access = evAccessExpr
                for a in newArgs do
                    access <- access.GetType().GetMethod("Invoke").Invoke(access, [| a |])
                for d in dynDefArgs do
                    access <- access.GetType().GetMethod("Invoke").Invoke(access, [| d |])
                // Now compute delta
                strideInterGroup <- Math.Abs((access :?> float32) - (baseLine :?> float32))

            // Add item to strides
            strides.[dim] <- (strideIntraGroup, strideInterGroup)

        evCount :?> int, strides

    override this.Evaluate(m, prec, args, opts) =
        // Default evaluation of precomputed features consists in
        // evaluating the expression to obtain a function to then apply using the current args
        let precFeatures, dynDefPlaceholders = prec :?> (obj list * Var list)
        let featureNames = this.FeatureNameList
        let evaluablePrecomputedFeature = precFeatures.[0]
        let accessExpressions = evaluablePrecomputedFeature :?> Dictionary<Var, (Expr * Expr) list>
                                        
        let constantsDefines = if opts.ContainsKey(RuntimeOptions.ConstantDefines) then
                                    Some(opts.[RuntimeOptions.ConstantDefines] :?> (string * obj) list)
                                else
                                    None
        let dynDefArgs = seq {
                            for v in dynDefPlaceholders do
                                let cdef = if constantsDefines.IsSome then constantsDefines.Value |> List.tryFind(fun (s, o) -> s = v.Name) else None
                                if cdef.IsSome then
                                    yield snd(cdef.Value)
                            } |> List.ofSeq     
                                                             
        // For each access we evaluate the stride, ignoring the particular array accessed
        let intraStrideList = new List<int * float32[]>()
        let interStrideList = new List<int * float32[]>()
        for v in accessExpressions do
            for accessExprCount, accessExpr in v.Value do
                let count, strides = this.EvaluateStride(accessExprCount, accessExpr, dynDefArgs, args)
                intraStrideList.Add((count, strides |> Array.unzip |> fst))
                interStrideList.Add((count, strides |> Array.unzip |> snd))

        // Now we group into stride-0, stride-1, stride > 1 for each dimension
        // Now we average using the count as weight and we replace tuple (intra, inter) with a 2-el array
        let aggregatedStride = new InterThreadMemoryAccessResult()
             
        // Average strides using access count as weight
        []

            
                

                