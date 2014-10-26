namespace FSCL.Runtime.Scheduling.FRTSchedulingEngine.FeatureExtraction

open FSCL
open FSCL.Compiler
open FSCL.Runtime.Scheduling
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open System.Reflection
open Microsoft.FSharp.Linq.RuntimeHelpers
open System
open FSCL.Compiler.Util
open FSCL.Compiler.FunctionPostprocessing
open FSCL.Language
open System.Runtime.InteropServices
open FSCL.Runtime.Scheduling.ReflectionUtil
open FSCL.Runtime
open System.Linq

(*
type SimplifiedMemoryAccessNode = 
| SimplifiedLoopLeaf of Expr<int> * Var * Expr * Expr * Expr * (Var * Expr) list
| SimplifiedLoopNode of Expr<int> * Var * Expr * Expr * Expr * SimplifiedMemoryAccessNode list
*)

type SingleThreadMemoryAccessAnalyser() = 
    inherit OmogeneousDefaultFeatureExtractor<IKernelModule, float32>() 
           
    // Assumption on cache line size
    let cacheLineSize = 64.0f
    let wordSize = 4.0f

    let rec onlyLoop (n: MemoryAccessNode list) =
        let containsLoop = n |> List.fold(fun hasLoop n ->
                                                match n with
                                                | MemAccess(v, addr) ->
                                                    hasLoop
                                                | _ ->
                                                    true) false
        if not containsLoop then
            n
        else
            let filt = n |> List.filter(fun n -> 
                                            match n with
                                            | MemAccess(v, addr) ->
                                                false
                                            | _ ->
                                                true)
            filt |> List.map(fun n -> 
                                match n with
                                | MemAccess(v, addr) ->
                                    n
                                | LoopNode(c, v, s, u, e, child) ->
                                    LoopNode(c, v, s, u, e, onlyLoop(child)))

    override this.FeatureNameList 
        with get() =
         [ "Cache Misses" ]

    member private this.AnalyseNode(n: MemoryAccessNode, loopVarVals: (Var * Expr) list, args: obj list, dynDefArgs: obj list) =
        let rec isReferencingVar(n: MemoryAccessNode, v: Var) =
            match n with
            | MemAccess(v1, _) ->
                if v1 = v then
                    true
                else
                    false
            | LoopNode(tc, itv, start, step, ende, child) ->
                child |> List.map(fun c -> isReferencingVar(c, v)) |> List.reduce (||)                

        match n with
        | MemAccess(v, addr) ->
            let misses = new Dictionary<Var, float32>()
            let firstLastAccess = new Dictionary<Var, int * int>()
            misses.Add(v, 0.0f)
            // Determine address 
            let addrVal = KernelUtil.EvaluateClosedExpression(
                                    QuotationUtil.ReplaceVars(addr, loopVarVals), args, dynDefArgs) :?> int
            firstLastAccess.Add(v, (addrVal, addrVal))
            misses, firstLastAccess
         | LoopNode(tripCountExpr, itVar, startExpr, secondExpr, endExpr, child) ->
            let misses = new Dictionary<Var, float32>()
            let firstLastAccess = new Dictionary<Var, int * int>()
            if child.Length = 0 then
                // No child or subloop
                misses, firstLastAccess
            else
                // Evaluate loop info
                let tripCount = KernelUtil.EvaluateClosedExpression(
                                    QuotationUtil.ReplaceVars(tripCountExpr, loopVarVals), args, dynDefArgs) :?> int |> float32                
                let startVal = KernelUtil.EvaluateClosedExpression(
                                    QuotationUtil.ReplaceVars(startExpr, loopVarVals), args, dynDefArgs)              
                let secondVal = KernelUtil.EvaluateClosedExpression(
                                    QuotationUtil.ReplaceVars(secondExpr, loopVarVals), args, dynDefArgs)      
                let endVal = KernelUtil.EvaluateClosedExpression(
                                    QuotationUtil.ReplaceVars(endExpr, loopVarVals), args, dynDefArgs)               
               
                if tripCount = 0.0f then
                    // No effect from this loop
                    misses, firstLastAccess
                else
                    //let interChildrenMisses = new Dictionary<Var, float32>()
                    //let minAddressInLoop = new Dictionary<Var, int>()
                    //let maxAddressInLoop = new Dictionary<Var, int>()
                    let mutable isLeafNode = false

                    for c in child do
                        let cm, cfla = this.AnalyseNode(c, loopVarVals @ [ (itVar, Expr.Value(startVal, startVal.GetType())) ], args, dynDefArgs)                        
                        for item in cm do
                            //if not <| interChildrenMisses.ContainsKey(item.Key) then
                                //interChildrenMisses.Add(item.Key, 0.0f)
                                //minAddressInLoop.Add(item.Key, Int32.MaxValue)
                                //maxAddressInLoop.Add(item.Key, Int32.MinValue)

                            // Update misses with this child misses (multiplied by this tripCount)
                            if misses.ContainsKey(item.Key) then
                                misses.[item.Key] <- misses.[item.Key] + (item.Value * tripCount)
                            else
                                misses.Add(item.Key, item.Value)
                            // Update misses with child-child misses
                            if firstLastAccess.ContainsKey(item.Key) then
                                let lastAcc = firstLastAccess.[item.Key] |> snd
                                let newAcc = cfla.[item.Key] |> fst
                                let stride = Math.Abs(lastAcc - newAcc) * Marshal.SizeOf(item.Key.Type.GetElementType()) |> float32                            
                                let missCount = Math.Floor(Math.Min(stride/cacheLineSize, 1.0f) |> float) |> float32                                
                                misses.[item.Key] <- misses.[item.Key] + (missCount * tripCount)
                                // If this is a leaf loop (only mem accesses as children)
                                // Then I ceil the number of miss at the end
                                // Otherwise (only loops as children) I do it now
                                (*
                                match c with
                                | MemAccess(_, _) ->
                                    isLeafNode <- true
                                    misses.[item.Key] <- misses.[item.Key] + 
                                        (interChildrenMisses.[item.Key] * tripCount)
                                    //if minAddressInLoop.[item.Key] > newAcc then
                                      // minAddressInLoop.[item.Key] <- newAcc 
                                    //if maxAddressInLoop.[item.Key] < newAcc then
                                       //maxAddressInLoop.[item.Key] <- newAcc 
                                | _ ->        
                                    let missCount = Math.Ceiling(stride / cacheLineSize |> float) |> float32
                                    //interChildrenMisses.[item.Key] <- interChildrenMisses.[item.Key] + missCount
                                *)
                                // Update last access
                                firstLastAccess.[item.Key] <- (firstLastAccess.[item.Key] |> fst, newAcc)
                             else
                                // First time I see this struct
                                // I DO NOT consider the first access as a miss
                                let newAcc = cfla.[item.Key] |> fst
                                // Update first and last access
                                firstLastAccess.Add(item.Key, (newAcc, newAcc))
                    
                    // If this is a leaf node we need to do a bit more to compute inter children miss
                    (*if isLeafNode then
                        for item in minAddressInLoop do
                            let stride = (maxAddressInLoop.[item.Key] - minAddressInLoop.[item.Key]) * Marshal.SizeOf(item.Key.Type.GetElementType()) |> float32                            
                            let missCount = 
                                if stride / cacheLineSize > 1.0f then
                                    1.0f
                                else
                                    0.0f
                            misses.[item.Key] <- misses.[item.Key] + (missCount * tripCount)
                    else*)
                    // Ceiling interChildrenMisses, multiply by trip count and add them to misses
                    (*for item in interChildrenMisses do
                        misses.[item.Key] <- misses.[item.Key] + 
                            (interChildrenMisses.[item.Key] * tripCount)*)

                    // Remains to add the misses between last access in first iter (already have this)
                    // and first in second iter 
                    // Compute first child data in second iter
                    // First child is the first child that is referencing a particular struct
                    let secondCfla = new Dictionary<Var, int * int>()
                    // Until I considered all the vars
                    let mutable childIndex = 0
                    while secondCfla.Count < firstLastAccess.Count do
                        let _, cfla = this.AnalyseNode(child.[childIndex], loopVarVals @ [ (itVar, Expr.Value(secondVal, secondVal.GetType())) ], args, dynDefArgs)                        
                        // Update first accesses
                        for item in cfla do
                            if not <| secondCfla.ContainsKey(item.Key) then
                                secondCfla.Add(item.Key, item.Value)
                        childIndex <- childIndex + 1
                    // Now compute stride for each struct
                    for item in secondCfla do
                        let stride = ((secondCfla.[item.Key] |> fst) - (firstLastAccess.[item.Key] |> snd)) * Marshal.SizeOf(item.Key.Type.GetElementType()) |> float32
                        let missCount = Math.Floor(Math.Min(stride/cacheLineSize, 1.0f) |> float) |> float32
                        // Update misses with missCount multiplied by tripCount - 1
                        misses.[item.Key] <- misses.[item.Key] + (missCount * (tripCount - 1.0f))

                    // Finally, update all the last accesses for the structs 
                    // with the accesses taken considering this loop last iteration
                    let lastCfla = new Dictionary<Var, int * int>()
                    // Until I considered all the vars
                    let mutable childIndex = child.Length - 1
                    while lastCfla.Count < firstLastAccess.Count do
                        let _, cfla = this.AnalyseNode(child.[childIndex], loopVarVals @ [ (itVar, Expr.Value(endVal, endVal.GetType())) ], args, dynDefArgs)                        
                        // Update last accesses
                        for item in cfla do
                            if not <| lastCfla.ContainsKey(item.Key) then
                                lastCfla.Add(item.Key, item.Value)
                        childIndex <- childIndex - 1
                    // Update firstLastAccess data
                    for item in lastCfla do
                        firstLastAccess.[item.Key] <- (firstLastAccess.[item.Key] |> fst, item.Value |> snd)

                    // Return
                    misses, firstLastAccess
                                    
                (*
            
    member private this.AnalyseLoop(tripCountExpr: Expr, v: Var, startExpr: Expr, secondExpr: Expr, endExpr: Expr, child: MemoryAccessNode list, loopVarVals: Dictionary<Var, Expr>, args: obj list, dynDefArgs: obj list) =
        // Evaluate loop info
        if tripCount = 0 || child.Length = 0 then
            // No cache problem
            new Dictionary<Var, int>(), new Dictionary<Var, int * int>()
        else
            match child.[0] with
            | MemAccess(_, _) ->
                // This is a leaf loop
                // Compute accesses for start value of loop expr
                loopVarVals.Add(v, Expr.Value(startVal, startVal.GetType()))
                // We store for each data struct the first, second and last access in the loop
                let insideLoopMemAccesses = new Dictionary<Var, List<int>>()
                for n in child do
                    match n with 
                    | MemAccess(v, addr) ->
                        if not <| insideLoopMemAccesses.ContainsKey(v) then
                            insideLoopMemAccesses.Add(v, new List<int>())
                        insideLoopMemAccesses.[v].Add(KernelUtil.EvaluateClosedExpression(QuotationUtil.ReplaceVars(addr, loopVarVals), args, dynDefArgs) :?> int)
                    | _ ->
                        failwith "Cannot have a loop here"

                if tripCount = 1 then
                    // No need to evaluate accesses for next iteration
                    // Now we can compute
                    // 1) Stride between first and second access to same struct inside loop
                    // 2) Stride between last access in an iter and first of the following to same struct (will be 0)
                    let loopAccessMiss = new Dictionary<Var, int>()
                    let loopFirstLastAccess = new Dictionary<Var, int * int>()
                    for item in insideLoopMemAccesses do
                        if not <| loopAccessMiss.ContainsKey(item.Key) then
                            loopAccessMiss.Add(item.Key, 0)
                            loopFirstLastAccess.Add(item.Key, (0, 0))
                        let insideMiss =
                            if insideLoopMemAccesses.[item.Key].Count = 1 then
                                0.0
                            else
                                // (Stride * sizeof(element)) / cacheline
                                // We keep float and round later considering the number of accesses 
                                // to this struct insize loop
                                ((Math.Abs(insideLoopMemAccesses.[item.Key].[1] - insideLoopMemAccesses.[item.Key].[0]) * 
                                        Marshal.SizeOf(item.Key.Type.GetElementType())) |> float) / (cacheLineSize |> float)

                        loopAccessMiss.[item.Key] <- 
                            // Total miss = (successive miss count) * (number of accesses in loop - 1)
                            Math.Ceiling(
                                ((insideLoopMemAccesses.[item.Key].Count - 1) |> float) * insideMiss) |> int
                        // Remember the last access address
                        loopFirstLastAccess.[item.Key] <- 
                            (insideLoopMemAccesses.[item.Key].First(),
                             insideLoopMemAccesses.[item.Key].Last())
                    loopAccessMiss, loopFirstLastAccess
                else
                    // Must evaluate accesses between iterations
                    // Evaluate first access to each data struct for updated iteration variable
                    loopVarVals.[v] <- Expr.Value(secondVal, secondVal.GetType())
                    let insideLoopMemAccessesNextIter = new Dictionary<Var, int>()
                    for n in child do
                        match n with 
                        | MemAccess(v, addr) ->
                            if not <| insideLoopMemAccessesNextIter.ContainsKey(v) then
                                insideLoopMemAccessesNextIter.Add(v, KernelUtil.EvaluateClosedExpression(QuotationUtil.ReplaceVars(addr, loopVarVals), args, dynDefArgs) :?> int)
                        | _ ->
                            failwith "Cannot have a loop here"
                    // Get the last access address for each struct
                    loopVarVals.[v] <- Expr.Value(endVal, endVal.GetType())
                    let loopFirstLastAccesses = new Dictionary<Var, int * int>()
                    for n in child do
                        match n with 
                        | MemAccess(v, addr) ->
                            if not <| loopFirstLastAccesses.ContainsKey(v) then
                                loopFirstLastAccesses.Add(v, (KernelUtil.EvaluateClosedExpression(QuotationUtil.ReplaceVars(addr, loopVarVals), args, dynDefArgs) :?> int, 0))
                            loopFirstLastAccesses.[v] <- 
                                (loopFirstLastAccesses.[v] |> fst,
                                 KernelUtil.EvaluateClosedExpression(QuotationUtil.ReplaceVars(addr, loopVarVals), args, dynDefArgs) :?> int)
                        | _ ->
                            failwith "Cannot have a loop here"

                    // Now we can compute:
                    // 1) Stride between first and second access to same struct inside loop
                    // 2) Stride between last access in an iter and first of the following to same struct
                    let loopAccessMiss = new Dictionary<Var, int>()
                    for item in insideLoopMemAccesses do
                        if not <| loopAccessMiss.ContainsKey(item.Key) then
                            loopAccessMiss.Add(item.Key, 0)
                        // Compute inside loop stride (0 if one only access)
                        let insideMiss =
                            if insideLoopMemAccesses.[item.Key].Count = 1 then
                                0.0
                            else
                                // (Stride * sizeof(element)) / cacheline
                                // We keep float and round later considering the number of accesses 
                                // to this struct insize loop
                                ((Math.Abs(insideLoopMemAccesses.[item.Key].[1] - insideLoopMemAccesses.[item.Key].[0]) * 
                                        Marshal.SizeOf(item.Key.Type.GetElementType())) |> float) / (cacheLineSize |> float)

                        loopAccessMiss.[item.Key] <-
                            // Total miss = (successive miss count) * (number of accesses in loop - 1) +
                            //              (trip count - 1) * (inter iterations miss count)                
                            (Math.Ceiling(
                                ((insideLoopMemAccesses.[item.Key].Count - 1) |> float) * insideMiss) |> int) +
                             ((tripCount - 1) * 
                              (Math.Ceiling(
                                (Math.Abs(
                                    insideLoopMemAccessesNextIter.[item.Key] - 
                                    insideLoopMemAccesses.[item.Key].Last()) |> float) * 
                                 (Marshal.SizeOf(item.Key.Type.GetElementType()) |> float) / (cacheLineSize |> float)) |> int))
                    
                    loopAccessMiss, loopFirstLastAccesses
            | _ ->
                // Nested loops
                loopVarVals.Add(v, Expr.Value(startVal, startVal.GetType()))
                // Compute accesses for start value of loop expr
                let loopMiss = new Dictionary<Var, int>()
                let loopFirstLastAccess = new Dictionary<Var, int * int>()
                let mutable lastLoopAccess = null
                for n in child do
                    match n with
                    | LoopNode(tripCountExpr, itVar, startExpr, updateExpr, endExpr, child) ->
                        // This is a nested loop. 
                        // We have to evaluate it for start, second and last iteration value of the current loop
                        let loopCacheMisses, lastAccess =
                            this.AnalyseLoop(tripCountExpr, itVar, startExpr, updateExpr, endExpr, child, loopVarVals, args, dynDefArgs)
                        lastLoopAccess <- lastAccess

                        // Merge with preceding loop results                        
                        for item in loopCacheMisses do
                            if not <| loopMiss.ContainsKey(item.Key) then
                                loopMiss.Add(item.Key, item.Value)
                                loopFirstLastAccess.Add(item.Key, lastAccess.[item.Key])
                            else
                                loopMiss.[item.Key] <- loopMiss.[item.Key] + loopMiss.[item.Key]
                                loopFirstLastAccess.[item.Key] <- 
                                    (loopFirstLastAccess.[item.Key] |> fst,
                                     lastAccess.[item.Key] |> snd)                         
                     | _ ->
                        failwith "Cannot be a memory access"  
                        
                // We now check for each struct the stride and misses between the last access
                // in the first iter (iter wher outer loop var = startVal) and first of successive
                if tripCount = 1 then
                    // No other iteration
                    loopMiss, loopFirstLastAccess
                else
                    // We evaluate
                    // 1) The first subloop with this loop var set to second value
                    // 2) The last subloop with this loop var set to first value (already done)
                    // 3) The last subloop with this loop var set to last value (to update last access)
                    // 4) TODO Comput stride/misses between last access first subloop and first access second subloop
                    let _, secondIterFirstSubloopFirstAccess =
                        loopVarVals.[v] <- Expr.Value(secondVal, secondVal.GetType())
                        match child.[0] with
                        | LoopNode(tripCountExpr, itVar, startExpr, updateExpr, endExpr, child) ->
                            this.AnalyseLoop(tripCountExpr, itVar, startExpr, updateExpr, endExpr, child, loopVarVals, args, dynDefArgs)
                        | _ ->
                            failwith "Cannot be"    
                    let firstIterLastSubloopLastAccess = 
                        lastLoopAccess            
                    let _, lastIterLastSubloopLastAccess =
                        loopVarVals.[v] <- Expr.Value(endVal, endVal.GetType())
                        match child.Last() with
                        | LoopNode(tripCountExpr, itVar, startExpr, updateExpr, endExpr, child) ->
                            this.AnalyseLoop(tripCountExpr, itVar, startExpr, updateExpr, endExpr, child, loopVarVals, args, dynDefArgs)
                        | _ ->
                            failwith "Cannot be"
                    // Compute last-first stride
                    let lastFirstMisses = new Dictionary<Var, int>()
                    for item in secondIterFirstSubloopFirstAccess do
                        if lastIterLastSubloopLastAccess.ContainsKey(item.Key) then
                            let misses = 
                                ((tripCount - 1) * 
                                  (Math.Ceiling(
                                    (Math.Abs(
                                        (secondIterFirstSubloopFirstAccess.[item.Key] |> fst) - 
                                        (firstIterLastSubloopLastAccess.[item.Key] |> snd)) |> float) * 
                                     (Marshal.SizeOf(item.Key.Type.GetElementType()) |> float) / (cacheLineSize |> float)) |> int))
                            ()
                    
                    let thisLoopIterVals = 
                        if tripCount = 0 then
                            []
                        elif tripCount = 1 then
                            [ startVal ]
                        else
                            [ startVal; secondVal, endVal ]                                    
                    for itVal in thisLoopIterVals do
                        ()
                    new Dictionary<Var, List<int>>(), new Dictionary<Var, int>()
                    *)

    override this.Precompute(m: IKernelModule) =
        // We have parameters in m with access analysis computed
        let parameters = m.Kernel.OriginalParameters |> 
                         Seq.map(fun (p: IOriginalFunctionParameter) -> 
                                    (p.OriginalParamterInfo, p.OriginalPlaceholder)) |>
                         Array.ofSeq  
        
        let accessData, ph = SingleThreadMemoryAccessCollector.EstimateMemoryAccessStride(m.Kernel.OriginalBody,
                                                                                          parameters)                                               
        (accessData, ph |> List.ofSeq) :> obj

    override this.Evaluate(m, prec, args, opts) =
        // Default evaluation of precomputed features consists in
        // evaluating the expression to obtain a function to then apply using the current args
        let precFeatures, dynDefPlaceholders = prec :?> (MemoryAccessNode * Var list)
        let featureNames = this.FeatureNameList
                                         
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
                            
        // Filter data (can be done statically, TODO)
        let filtered =
            match precFeatures with
            | MemAccess(_ , _) ->
                failwith "Cannot be"
            | LoopNode(tc, itv, st, sn, en, child) ->
                LoopNode(tc, itv, st, sn, en, onlyLoop(child))
        
        let cacheMiss, lastAccess = 
            this.AnalyseNode(filtered, [], args, dynDefArgs)
        
        // Sum the misses for all the structures
        let mutable totalMiss = 0.0f
        for item in cacheMiss do
            totalMiss <- totalMiss + item.Value
                                                         
        // For each access we evaluate the stride, ignoring the particular array accessed
        [ box totalMiss ]
            

            
                

                