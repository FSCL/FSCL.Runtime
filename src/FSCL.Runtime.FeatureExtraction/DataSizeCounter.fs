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

type DataSizeCounter() = 
    inherit IFeatureExtractor()
    override this.FeatureNameList 
        with get() =
            [ "Data size transf from host to device (bytes)"; "Data size transf from device to host (bytes)"; "Data size allocated locally (bytes)" ]

    override this.Precompute(m: IKernelModule) =
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
                     
        // Build expr
        [ globalHostToDevP :> obj; globalDevToHostP :> obj; localP :> obj; embeddedLocalP :> obj ] :> obj

    override this.Evaluate(m, prec, args, opts) =
        // Now use args to provide values
        let mutable globalHostToDevSize = 0L
        let mutable globalDevToHostSize = 0L
        let mutable localSize = 0L

        let precomputed = prec :?> obj list        
        let globalHostToDevP, globalDevToHostP, localP, embeddedLocalP = 
            precomputed.[0] :?> int list,
            precomputed.[1] :?> int list,
            precomputed.[2] :?> int list,
            precomputed.[3] :?> Var list

        // Compute size
        for index in globalHostToDevP do
            let arrSize = (args.[index] :?> Array).LongLength
            let arrElementSize = Marshal.SizeOf(args.[index].GetType().GetElementType()) |> int64
            globalHostToDevSize <- globalHostToDevSize + (arrSize * arrElementSize)

        for index in globalDevToHostP do
            let arrSize = (args.[index] :?> Array).LongLength
            let arrElementSize = Marshal.SizeOf(args.[index].GetType().GetElementType()) |> int64
            globalDevToHostSize <- globalDevToHostSize + (arrSize * arrElementSize)

        for index in localP do
            let arrSize = (args.[index] :?> Array).LongLength
            let arrElementSize = Marshal.SizeOf(args.[index].GetType().GetElementType()) |> int64
            localSize <- localSize + (arrSize * arrElementSize)

        // Evaluate alloc expression for embedded locals
        for v in embeddedLocalP do
            let vt, allocExpr = m.Kernel.LocalVars.[v]
            let arrElementSize = Marshal.SizeOf(v.Type.GetElementType()) |> int64
            let allocCounts = List.map(fun (e:Expr) ->
                                        // Create function to evaluate the alloc exprs
                                        let ev = QuotationUtil.ReplaceFunctionBody(m.Kernel.OriginalBody, e)
                                        let res = LeafExpressionConverter.EvaluateQuotation(ev.Value) 
                                        let tupledArg = ToTuple(args |> List.toArray)
                                        // Evaluate alloc exprs
                                        let allocCountValue = res.GetType().GetMethod("Invoke").Invoke(res, [| tupledArg |])
                                        allocCountValue :?> int) (allocExpr.Value)
            let allocSize = allocCounts |> List.reduce (fun a b -> a * b) |> int64
            localSize <- localSize + (allocSize * arrElementSize)
                
        [ globalHostToDevSize :> obj; 
          globalDevToHostSize :> obj; 
          localSize :> obj ]
            
                

                