namespace FSCL.Runtime.CompilerSteps

open FSCL.Compiler
open FSCL.Compiler.Util
open FSCL.Compiler.ModuleParsing
open FSCL.Language
open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit
open Microsoft.FSharp.Quotations
open FSCL.Language
open FSCL.Runtime
open System
open OpenCL

[<StepProcessor("FSCL_RUNTIME_TO_COMPILER_META_MAPPING_PROCESSOR", 
                "FSCL_MODULE_PARSING_STEP",
                Before=[| "FSCL_ACCELERATED_ARRAY_META_FILTERING_PROCESSOR" |])>]
type RuntimeToCompilerMetadataMapping() = 
    inherit MetadataFinalizerProcessor()
    
    override this.Run((kmeta, rmeta, pmeta, info), s, opts) =
        let step = s :?> ModuleParsingStep
        
        // Map Runtime attribute "Device(X, Y)" to the appropriate "DeviceType" attribute for the compiler
        let dev = (kmeta :> IKernelMetaCollection).Get<DeviceAttribute>()                    
        if not ((kmeta :> IKernelMetaCollection).Contains<DeviceTypeAttribute>()) then
            // Get dev type from Cloo
            let devType = OpenCLPlatform.Platforms.[dev.Platform].Devices.[dev.Device].Type
            kmeta.Add(match devType with
                        | OpenCLDeviceType.Gpu ->
                            new DeviceTypeAttribute(DeviceType.Gpu)
                        | _ ->
                            new DeviceTypeAttribute(DeviceType.Cpu))
        
        // Add global work size, local work size, running mode and fallback meta
        if not((kmeta :> IKernelMetaCollection).Contains<WorkSizeAttribute>()) && opts.ContainsKey(RuntimeOptions.WorkSize) then
            let gsize, lsize, goff = opts.[RuntimeOptions.WorkSize] :?> (int64 array) * (int64 array) * (int64 array)
            kmeta.Add(new WorkSizeAttribute(gsize, lsize, goff))            
        if not((kmeta :> IKernelMetaCollection).Contains<RunningModeAttribute>()) && opts.ContainsKey(RuntimeOptions.RunningMode) then
            kmeta.Add(new RunningModeAttribute(opts.[RuntimeOptions.RunningMode] :?> RunningMode))
        if not((kmeta :> IKernelMetaCollection).Contains<MultithreadFallbackAttribute>()) && opts.ContainsKey(RuntimeOptions.MultithreadFallback) then
            kmeta.Add(new MultithreadFallbackAttribute(opts.[RuntimeOptions.MultithreadFallback] :?> bool))
            
        (kmeta, rmeta, pmeta)

                

             
            