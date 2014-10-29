namespace FSCL.Runtime.Scheduling.FRTSchedulingEngine.FeatureExtraction

open FSCL
open FSCL.Compiler
open FSCL.Runtime.Scheduling
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open System.Reflection
open Microsoft.FSharp.Linq.RuntimeHelpers
open FSCL.Runtime.Scheduling.FRTSchedulingEngine
open System
open FSCL.Runtime
open FSCL.Compiler.Util
open FSCL.Compiler.FunctionPostprocessing
open FSCL.Language
open System.Runtime.InteropServices
open FSCL.Runtime.Scheduling.ReflectionUtil

[<FRTFeatureExtractor("DataSizeCounter")>]
type DataSizeCounter() = 
    inherit FRTFeatureExtractor() 

    override this.FeatureIDs
        with get() =
            [ "Data size transf from host to device (bytes)"; 
               "Data size transf from device to host (bytes)"; 
               "Data size allocated locally (bytes)" ]

    override this.BuildFinalizers(m: IKernelModule) =
        // We have parameters in m with access analysis computed
        let mutable globalHostToDevP = []
        let mutable globalDevToHostP = []
        let mutable localP = []
        let mutable embeddedLocalP = []

        for i = 0 to m.Kernel.OriginalParameters.Count - 1 do
            let item = m.Kernel.OriginalParameters.[i]

            let analysis = item.AccessAnalysis
            let space = item.Meta.Get<AddressSpaceAttribute>().AddressSpace
            let htdtransferMode = item.Meta.Get<TransferModeAttribute>().HostToDeviceMode
            let dthtransferMode = item.Meta.Get<TransferModeAttribute>().DeviceToHostMode
            let flags = item.Meta.Get<MemoryFlagsAttribute>().Flags
            let isTransferredToDevice = item.DataType.IsArray &&
                                        analysis &&& AccessAnalysisResult.ReadAccess |> int > 0 && 
                                        space <> AddressSpace.Private &&
                                        space <> AddressSpace.Local &&
                                        ((htdtransferMode <> TransferMode.NoTransfer))

            let isTransferredFromDevice = item.DataType.IsArray &&
                                          analysis &&& AccessAnalysisResult.WriteAccess |> int > 0 &&
                                          space <> AddressSpace.Local &&
                                          space <> AddressSpace.Private &&
                                          space <> AddressSpace.Constant &&
                                          ((dthtransferMode <> TransferMode.NoTransfer))
            if isTransferredToDevice then
                globalHostToDevP <- globalHostToDevP @ [ i ]
            if isTransferredFromDevice then
                globalDevToHostP <- globalDevToHostP @ [ i ]
            if space = AddressSpace.Local && item.DataType.IsArray then
                localP <- localP @ [ i ]

        // Check embedded local arrays
        for item in m.Kernel.LocalVars do
            embeddedLocalP <- embeddedLocalP @ [ item.Key ]

        let embeddedLocalPExprs = new Dictionary<Var, obj list>()
        for item in m.Kernel.LocalVars do
            if (item.Value |> snd).IsSome then
                let ev = ((item.Value |> snd).Value) |> List.map(fun (e:Expr) ->
                    // Create function to evaluate the alloc exprs
                    let ev = QuotationUtil.ReplaceFunctionBody(m.Kernel.OriginalBody, e)
                    let res = LeafExpressionConverter.EvaluateQuotation(QuotationUtil.ToCurriedFunction(ev.Value))
                    // Evaluate alloc exprs
                    res) 
                if embeddedLocalPExprs.ContainsKey(item.Key) then
                    embeddedLocalPExprs.[item.Key] <- embeddedLocalPExprs.[item.Key] @ ev 
                else
                    embeddedLocalPExprs.Add(item.Key, ev)
                     
        // Build expr
        [ globalHostToDevP |> box; 
          globalDevToHostP |> box; 
          localP |> box; 
          embeddedLocalP |> box;
          embeddedLocalPExprs |> box ] |> box

    override this.EvaluateFinalizers(m, p, args) =
        let precomputed = p :?> obj list

        // Now use args to provide values
        let mutable globalHostToDevSize = 0L
        let mutable globalDevToHostSize = 0L
        let mutable localSize = 0L
       
        let globalHostToDevP, globalDevToHostP, localP, embeddedLocalP, embeddedLocalPExprs = 
            precomputed.[0] :?> int list,
            precomputed.[1] :?> int list,
            precomputed.[2] :?> int list,
            precomputed.[3] :?> Var list,
            precomputed.[4] :?> Dictionary<Var, obj list>

        let argOffset = 
            if m.Kernel.InstanceVar.IsSome then
                1
            else
                0
        // Compute size
        for index in globalHostToDevP do
            let arrSize = (args.[index + argOffset] :?> Array).LongLength
            let arrElementSize = Marshal.SizeOf(args.[index + argOffset].GetType().GetElementType()) |> int64
            globalHostToDevSize <- globalHostToDevSize + (arrSize * arrElementSize)

        for index in globalDevToHostP do
            let arrSize = (args.[index + argOffset] :?> Array).LongLength
            let arrElementSize = Marshal.SizeOf(args.[index + argOffset].GetType().GetElementType()) |> int64
            globalDevToHostSize <- globalDevToHostSize + (arrSize * arrElementSize)

        for index in localP do
            let arrSize = (args.[index + argOffset] :?> Array).LongLength
            let arrElementSize = Marshal.SizeOf(args.[index + argOffset].GetType().GetElementType()) |> int64
            localSize <- localSize + (arrSize * arrElementSize)

        // Evaluate alloc expression for embedded locals                                
        for v in embeddedLocalP do
            let allocCounts = embeddedLocalPExprs.[v] |> List.map(fun ev ->
                // Now we can apply the evaluator to obtain the value of the feature using actual args
                let mutable fv = ev
                for a in args do
                    fv <- fv.GetType().GetMethod("Invoke").Invoke(fv, [| a |])
                fv :?> int)
            let allocSize = allocCounts |> List.reduce (fun a b -> a * b) |> int64
            localSize <- localSize + (allocSize * (Marshal.SizeOf(v.Type.GetElementType()) |> int64))
                
        [  globalHostToDevSize |> float32; 
           globalDevToHostSize |> float32; 
           localSize |> float32 ]
            
                

                