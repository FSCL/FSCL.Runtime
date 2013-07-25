namespace FSCL.Compiler.SeparatedCodegen

open System
open System.Reflection
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open FSCL.Compiler

[<Step("FSCL_SEPARATED_MODULE_CODEGENEN_STEP", Dependencies = [| "FSCL_FUNCTION_CODEGEN_STEP" |])>] 
type SeparatedCodegenStep(tm: TypeManager,
                          processors: ICompilerStepProcessor list) = 
    inherit CompilerStep<KernelModule, KernelModule * String>(tm, processors)
    
    let PrintStructDefinition(t: Type) =
        let mutable print = "struct " + t.Name + " {\n";
        if FSharpType.IsRecord(t) then
            for f in t.GetProperties (BindingFlags.Public ||| BindingFlags.Instance) do
                print <- print + tm.Print(f.PropertyType) + " " + f.Name + ";\n"
        else
            for f in t.GetFields (BindingFlags.Public ||| BindingFlags.Instance) do
                print <- print + tm.Print(f.FieldType) + " " + f.Name + ";\n"
        print <- print + "}\n";
        print

    override this.Run(km) =
        for k in km.CallGraph.Kernels do
            if not(km.CallGraph.GetKernel(k.ID).Skip) then
                let directives = String.concat "\n\n" (km.CallGraph.GetRequireDirectives(k.ID))
                let structs = km.CallGraph.GetTypesUsage(k.ID)
                let pstructs = String.concat "\n" (List.map (fun (s: Type) -> PrintStructDefinition(s)) structs)
                let functions = String.concat "\n\n" (List.map (fun (f: FunctionInfo) -> f.Codegen) (List.map (fun (m:MethodInfo) -> km.CallGraph.GetFunction(m)) (List.ofSeq(km.CallGraph.GetOutputCalls(k.ID).Keys))))
                let code = k.Codegen
                let result = String.concat "\n\n" [directives; pstructs; functions; code]
                k.CustomInfo.Add("SEPARATED_CODE", result)
        (km, "")

        

