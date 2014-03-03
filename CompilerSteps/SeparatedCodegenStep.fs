namespace FSCL.Compiler.SeparatedCodegen

open System
open System.Reflection
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open FSCL.Compiler

[<Step("FSCL_SEPARATED_MODULE_CODEGENEN_STEP", Dependencies = [| "FSCL_MODULE_CODEGEN_STEP" |])>] 
type SeparatedCodegenStep(tm: TypeManager,
                          processors: ICompilerStepProcessor list) = 
    inherit CompilerStep<KernelModule * String, KernelModule * String>(tm, processors)
    
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

    override this.Run((km, globalCode), opt) =
        for k in km.GetKernels() do
            if not(k.Info.Skip) then
                let directives = String.concat "\n\n" (km.GetFlattenRequiredDirectives(k.Info.ID))
                let structs = km.GetFlattenRequiredGlobalTypes(k.Info.ID)
                let pstructs = String.concat "\n" (List.map (fun (s: Type) -> PrintStructDefinition(s)) structs)
                let functions = String.concat "\n\n" (List.map (fun (f: FunctionEnvironment) -> f.Info.Code) (km.GetFlattenRequiredFunctions(k.Info.ID))) 
                let code = k.Info.Code
                let result = String.concat "\n\n" [directives; pstructs; functions; code]
                k.Info.CustomInfo.Add("SEPARATED_CODE", result)
        (km, globalCode)

        

