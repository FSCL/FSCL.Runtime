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

type MemoryAccessPatternAnalyser() = 
    inherit IDefaultFeatureExtractor()

    // Assumption on cache line size
    let cacheLineSize = 64.0f
    let wordSize = 4.0f

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
        
        let acount, ph = InterThreadMemoryAccessAnalyser.EstimateMemoryAccessStride(m.Kernel.OriginalBody,
                                                                                    parameters)
                                                               
        // Separate accesses to global/constant from accesses to local
        let glob = new Dictionary<Var, (Expr * Expr) list>()           
        let loca = new Dictionary<Var, (Expr * Expr) list>()  
        for item in acount do
            let p = m.Kernel.GetParameter(item.Key.Name)  
            if p.IsSome then   
                if (p.Value.Meta.Get(typeof<AddressSpaceAttribute>) :?> AddressSpaceAttribute).AddressSpace = AddressSpace.Local then
                    loca.Add(item.Key, item.Value)
                else
                    glob.Add(item.Key, item.Value)                                                                         
        ([ box glob; box loca ], ph |> List.ofSeq) :> obj

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
            let mutable strideIntraGroup = -1.0f
            let mutable strideInterGroup = -1.0f
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

    // Creating meaningful features
    member private this.CreateFeaturesFromStrides(intraStrideList: List<int * float32[]>,
                                                  interStrideList: List<int * float32[]>,
                                                  workSize: WorkItemInfo,
                                                  isLocalMem: bool) =    
        // Now lets create values for meaningful features
        // 1. Possible collisions along various dimensions between groups
        //    0 if linear
        //    1 if 0 or power of two stride
        //    0.5 if stride not power of two
        let mutable averageInterCollision = 0.0f
        if not isLocalMem then
            let mutable interCollisionCountSum = 0
            for item in interStrideList do
                let mutable numConfItems = 1.0f
                let mutable dimInf = 1.0f
                for dim in 0 .. workSize.WorkDim() - 1 do
                    match (item |> snd).[dim] with
                    | 0.0f ->
                         numConfItems <- numConfItems * (workSize.LocalSize(dim) |> float32)
                    | x ->
                        ()
                averageInterCollision <- averageInterCollision + ((numConfItems - 1.0f) * (item |> fst |> float32))
                interCollisionCountSum <- interCollisionCountSum + (fst item)
            averageInterCollision <- averageInterCollision / (interCollisionCountSum |> float32)

        // 2. Possible collisions along various dimensions inside group
        //    0 if linear
        //    1 if 0 or power of two stride
        //    0.5 if stride not power of two       
        let mutable averageIntraCollision = 0.0f
        let mutable intraCollisionCountSum = 0
        for item in intraStrideList do
            let mutable numConfItems = 1.0f
            let mutable dimInf = 1.0f
            for dim in 0 .. workSize.WorkDim() - 1 do
                match (item |> snd).[dim] with
                | 0.0f ->
                     numConfItems <- numConfItems * (workSize.LocalSize(dim) |> float32)
                | x ->
                    ()
            averageIntraCollision <- averageIntraCollision + ((numConfItems - 1.0f) * (item |> fst |> float32))
            intraCollisionCountSum <- intraCollisionCountSum + (fst item)
        averageIntraCollision <- averageIntraCollision / (intraCollisionCountSum |> float32)

        // 3. Possible cohalescing along various dimensions
        //    1 if linear
        //    0 if not
        //    cohalescing on multidim is possible if t_0_y+1 = 1 + t_max_x_y
        let mutable averageIntraCohaleshing = 0.0f
        let mutable intraCohalCountSum = 0
        for item in intraStrideList do
            match (item |> snd).[0] with
            | 1.0f ->
                averageIntraCohaleshing <- ((item |> fst |> float32) * 1.0f)
            | _ ->
                averageIntraCohaleshing <- ((item |> fst |> float32) * 0.0f)
            intraCohalCountSum <- intraCohalCountSum + (fst item)
        averageIntraCohaleshing <- averageIntraCohaleshing / (float32 intraCohalCountSum)

        // 4. Abstract cache-misses along various dimensions inside group
        //    0 if stride 0 or 1
        //    1 is stride power of 2
        //    0.5 if stride not power of 2
        let mutable averageIntraCacheUsage = 0.0f
        let mutable intraCacheUsageSum = 0
        for item in intraStrideList do
            let mutable itemIntraCacheUsage = 1.0f
            for dim in 0 .. workSize.WorkDim() - 1 do
                let lowerBound = 1.0f / (workSize.LocalSize(dim) |> float32)
                let x = 
                    if (item |> snd).[dim] < 0.f then
                       lowerBound
                    else
                       Math.Max(Math.Min((item |> snd).[dim] * wordSize / cacheLineSize, 1.f), lowerBound)
                    
                itemIntraCacheUsage <- itemIntraCacheUsage * x
            averageIntraCacheUsage <- averageIntraCacheUsage + itemIntraCacheUsage 
                
        // 5. Abstract cache-misses along various dimensions between groups
        //    0 if stride 0 or group size
        //    1 is stride power of 2
        //    0.5 if stride not power of 2
        let mutable averageInterCacheUsage = 0.0f
        if not isLocalMem then
            for item in interStrideList do
                let mutable itemInterCacheUsage = 1.0f
                for dim in 0 .. workSize.WorkDim() - 1 do
                    let lowerBound = 1.0f / (workSize.NumGroups(dim) |> float32)
                    let x = 
                        if (item |> snd).[dim] < 0.f then
                           lowerBound
                        else
                           Math.Max(Math.Min((item |> snd).[dim] * wordSize / cacheLineSize, 1.f), lowerBound)
                    
                    itemInterCacheUsage <- itemInterCacheUsage * x
                averageInterCacheUsage <- averageInterCacheUsage + itemInterCacheUsage 

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
        let globalAccessExpressions = precFeatures.[0] :?> Dictionary<Var, (Expr * Expr) list>
        let localAccessExpressions = precFeatures.[1] :?> Dictionary<Var, (Expr * Expr) list>
                                        
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
        let gIntraStrideList = new List<int * float32[]>()
        let gInterStrideList = new List<int * float32[]>()
        let lIntraStrideList = new List<int * float32[]>()
        for v in globalAccessExpressions do
            for accessExprCount, accessExpr in v.Value do
                let count, strides = this.EvaluateStride(accessExprCount, accessExpr, dynDefArgs, args)
                gIntraStrideList.Add((count, strides |> Array.unzip |> fst))
                gInterStrideList.Add((count, strides |> Array.unzip |> snd))
        for v in localAccessExpressions do
            for accessExprCount, accessExpr in v.Value do
                let count, strides = this.EvaluateStride(accessExprCount, accessExpr, dynDefArgs, args)
                lIntraStrideList.Add((count, strides |> Array.unzip |> fst))

        let workSize = 
            args |> List.tryFind(fun a -> typeof<WorkItemInfo>.IsAssignableFrom(a.GetType())) |> fun a -> a.Value :?> WorkItemInfo

        let globalData = this.CreateFeaturesFromStrides(gIntraStrideList, gInterStrideList, workSize, false)
        if lIntraStrideList.Count > 0 then
            let localData = this.CreateFeaturesFromStrides(lIntraStrideList, null, workSize, true)
        
            // Return feature values
            globalData @ localData
        else
            // Return feature values
            globalData @ [ box 0.0f; box 0.0f; box 1.1f ]
            

            
                

                