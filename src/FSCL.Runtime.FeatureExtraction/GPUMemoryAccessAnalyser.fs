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

// Work item info where global ID and local ID can be set
type MutableWorkItemInfo(globalID: int64[], localID: int64[], globalSize: int64[], localSize: int64[], globalOffset: int64[]) =
    inherit WorkSize(globalSize, localSize, globalOffset)
    
    override this.GlobalID(idx) =
        globalID.[idx] |> int      
    override this.LocalID(idx) =
        localID.[idx] |> int      

type GPUMemoryAccessAnalyser() = 
    inherit IDefaultFeatureExtractor()

    // Assumption on cache line size
    let cacheLineSize = 64.0f
    let wordSize = 4
    let accessWarpSize = 16

    override this.FeatureNameList 
        with get() =
         [ "Intra cache usage global";
           "Inter cache usage global";
           "Intra collision global";
           //"Inter collision global";
           "Intra cohaleshing global";
           "Intra cache usage local";
           "Intra collision local";
           "Intra cohaleshing local"; ]

    override this.Precompute(m: IKernelModule) =
        // We have parameters in m with access analysis computed
        let parameters = m.Kernel.OriginalParameters |> 
                         Seq.map(fun (p: IOriginalFunctionParameter) -> 
                                    (p.OriginalParamterInfo, p.OriginalPlaceholder)) |>
                         Array.ofSeq  
        
        let readAcc, writeAcc, ph = InterThreadMemoryAccessCollector.EstimateMemoryAccessStride(m.Kernel.OriginalBody,
                                                                                     parameters)
                                                               
        // Separate accesses to global/constant from accesses to local
        let globalRead = new Dictionary<Var, (Expr * Expr) list>()           
        let localRead = new Dictionary<Var, (Expr * Expr) list>()  
        let globalWrite = new Dictionary<Var, (Expr * Expr) list>()           
        let localWrite = new Dictionary<Var, (Expr * Expr) list>()  
        for item in readAcc do
            let p = m.Kernel.GetParameter(item.Key.Name)  
            if p.IsSome then   
                if (p.Value.Meta.Get(typeof<AddressSpaceAttribute>) :?> AddressSpaceAttribute).AddressSpace = AddressSpace.Local then
                    localRead.Add(item.Key, item.Value)
                else
                    globalRead.Add(item.Key, item.Value)  
        for item in writeAcc do
            let p = m.Kernel.GetParameter(item.Key.Name)  
            if p.IsSome then   
                if (p.Value.Meta.Get(typeof<AddressSpaceAttribute>) :?> AddressSpaceAttribute).AddressSpace = AddressSpace.Local then
                    localWrite.Add(item.Key, item.Value)
                else
                    globalWrite.Add(item.Key, item.Value)                                                                         
        ([ box globalRead; box localRead; box globalWrite; box localWrite ], ph |> List.ofSeq) :> obj

    // We compute the stride for threads of the same group and of different groups
    // 1) We use the thread 0-0-0 as reference
    // 2) We compute inter-group stride using the last thread of a group and the first of the successive
    // 3) We compute stride on all the dimensions, considered independent from each other
    member private this.EvaluateStride(count: Expr, accessExpr: Expr, elementSize: int, dynDefArgs: obj list, args: obj list) =
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
            (*if workSize
            try 
                Array.init (dims) (fun i -> workSize.GlobalOffset(i) |> int64)
            with 
            | :? System.NullReferenceException ->
              *)  Array.zeroCreate<int64> (dims)
                 
        // Now compute delta of accesses from other threads compared to the baseLine. 
        // We take into account threads of the same group and of different group in all the dimensions
        let localStrides = new List<int>()
        let mutable localBaseLine = baseLine
        for dim = 0 to dims - 1 do
            // Compute the stride for two threads of the same group in this dimension
            let mutable strideIntraGroup = -1.0f
            let mutable strideInterGroup = -1.0f
            for idx = 1 to (localSize.[dim] |> int) - 1 do
                let globalID = Array.init (dims) (fun i -> if i = dim then idx |> int64 else 0L)
                let localID = Array.init (dims) (fun i -> if i = dim then idx |> int64 else 0L)
                let wi = new MutableWorkItemInfo(globalID, localID, globalSize, localSize, globalOffset)
                let newArgs = replaceWorkItemInfo(args, wi)
                let mutable access = evAccessExpr
                for a in newArgs do
                    access <- access.GetType().GetMethod("Invoke").Invoke(access, [| a |])
                for d in dynDefArgs do
                    access <- access.GetType().GetMethod("Invoke").Invoke(access, [| d |])
                // Now compute delta
                localStrides.Add(Math.Abs((access :?> float32) - (localBaseLine :?> float32)) * (elementSize |> float32) |> int)
                localBaseLine <- access
                 
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
                strideInterGroup <- Math.Abs((access :?> float32) - (baseLine :?> float32)) * (elementSize |> float32)


        evCount :?> int, (localStrides |> Seq.toArray)

    // Creating meaningful features
    member private this.CreateFeaturesFromStrides(localStrides: List<int * int[]>,
                                                  globalInterStrideList: List<int * float32[]>,
                                                  localIntraStrideList: List<int * float32[]>,
                                                  workSize: WorkItemInfo) =    
        // 1. Number of uncoalesced accesses 
        let mutable uncoalescedAccessCount = 0
        let mutable effectiveMemAccessesPerGroup = 0
        for accCount, accStrides in localStrides do
            // Loop through warps in local size
            let mutable warpIdx = 0
            while (warpIdx < accStrides.Length) do
                let mutable coalesced = true
                for i = warpIdx to Math.Min(warpIdx + accessWarpSize - 2, accStrides.Length - 1) do
                    if accStrides.[i] = wordSize || accStrides.[i] = 2 * wordSize then
                        ()
                    else
                        coalesced <- false
                if not coalesced then
                    uncoalescedAccessCount <- uncoalescedAccessCount + accCount
                    effectiveMemAccessesPerGroup <- effectiveMemAccessesPerGroup + (accCount * accessWarpSize)
                else
                    effectiveMemAccessesPerGroup <- effectiveMemAccessesPerGroup + accCount                    
                warpIdx <- warpIdx + accessWarpSize - 1
                 
        
        if not isLocalMem then
            [ box averageIntraCacheUsage;
              box averageInterCacheUsage;
              box averageIntraCollision;
              box averageIntraCohaleshing ]
        else
            [ box averageIntraCacheUsage;
              box averageIntraCollision;
              box averageIntraCohaleshing ]

    override this.Evaluate(m, prec, args, opts) =
        // Default evaluation of precomputed features consists in
        // evaluating the expression to obtain a function to then apply using the current args
        let precFeatures, dynDefPlaceholders = prec :?> (obj list * Var list)
        let featureNames = this.FeatureNameList
        let globalReadAccessExpressions = precFeatures.[0] :?> Dictionary<Var, (Expr * Expr) list>
        let localReadAccessExpressions = precFeatures.[1] :?> Dictionary<Var, (Expr * Expr) list>
        let globalWriteAccessExpressions = precFeatures.[2] :?> Dictionary<Var, (Expr * Expr) list>
        let localWriteAccessExpressions = precFeatures.[3] :?> Dictionary<Var, (Expr * Expr) list>
                                        
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
        let globalReadIntraStrideList = new List<int * float32[]>()
        let globalReadInterStrideList = new List<int * float32[]>()
        let localReadIntraStrideList = new List<int * float32[]>()
        
        let globalWriteIntraStrideList = new List<int * float32[]>()
        let globalWriteInterStrideList = new List<int * float32[]>()
        let localWriteIntraStrideList = new List<int * float32[]>()

        for v in globalReadAccessExpressions do
            for accessExprCount, accessExpr in v.Value do
                let count, strides = this.EvaluateStride(accessExprCount, accessExpr, Marshal.SizeOf(v.Key.Type.GetElementType()), dynDefArgs, args)
                globalReadIntraStrideList.Add((count, strides |> Array.unzip |> fst))
                globalReadInterStrideList.Add((count, strides |> Array.unzip |> snd))
        for v in localReadAccessExpressions do
            for accessExprCount, accessExpr in v.Value do
                let count, strides = this.EvaluateStride(accessExprCount, accessExpr, Marshal.SizeOf(v.Key.Type.GetElementType()), dynDefArgs, args)
                localReadIntraStrideList.Add((count, strides |> Array.unzip |> fst))
                                
        for v in globalWriteAccessExpressions do
            for accessExprCount, accessExpr in v.Value do
                let count, strides = this.EvaluateStride(accessExprCount, accessExpr, Marshal.SizeOf(v.Key.Type.GetElementType()), dynDefArgs, args)
                globalWriteIntraStrideList.Add((count, strides |> Array.unzip |> fst))
                globalWriteInterStrideList.Add((count, strides |> Array.unzip |> snd))
        for v in localReadAccessExpressions do
            for accessExprCount, accessExpr in v.Value do
                let count, strides = this.EvaluateStride(accessExprCount, accessExpr, Marshal.SizeOf(v.Key.Type.GetElementType()), dynDefArgs, args)
                localWriteIntraStrideList.Add((count, strides |> Array.unzip |> fst))
                
        let workSize = 
            args |> List.tryFind(fun a -> typeof<WorkItemInfo>.IsAssignableFrom(a.GetType())) |> fun a -> a.Value :?> WorkItemInfo

        let globalData = this.CreateFeaturesFromStrides(globalReadIntraStrideList, globalReadInterStrideList, workSize, false)
        if localReadIntraStrideList.Count > 0 then
            let localData = this.CreateFeaturesFromStrides(localReadIntraStrideList, null, workSize, true)
        
            // Return feature values
            globalData @ localData
        else
            // Return feature values
            globalData @ [ box 0.0f; box 0.0f; box 1.0f ]
            

            
                

                