namespace FSCL.Runtime.Managers

open System
open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit
open FSCL.Compiler.Util
open FSCL.Language
open Mono.Reflection

///
///<summary>
///The container of workspace related functions
///</summary>
///<remarks>
///This is not generally used directly since the runtime-support (i.e. the semantic) of workspace functions is given by the 
///OpenCL-to-device code compiler (e.g. Intel/AMD OpenCL compiler). Nevertheless this can help the FSCL runtime to produce
///debuggable multithrad implementation that programmers can use to test their kernels
///</remarks>
///

type MultithreadKernelAdaptor() =
    let domain = AppDomain.CurrentDomain
    let assemblyName = new AssemblyName(Name = "FSCL.Runtime.MultithreadKernelAdaptorDynamicAssembly")
    let assemblyBuilder = domain.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.Run)
    let moduleBuilder = assemblyBuilder.DefineDynamicModule("FSCL.Runtime.MultithreadKernelAdaptorDynamicModule")

    let workspaceFunctions = [ "get_global_id";
                               "get_local_id";
                               "get_global_size"; 
                               "get_local_size";
                               "get_num_groups";
                               "get_group_id";
                               "get_global_offset";
                               "get_work_dim" ]

    member this.VectorAdd(a: float32[], b: float32[], c: float32[], w: WorkItemIdContainer) =
        let gid = w.GlobalID(0)
        c.[gid] <- c.[gid] + a.[gid]

    member private this.ProcessKernelBody(kernel: MethodInfo, instructions: IList<Instruction>, rawInstructions: byte[], gen: ILGenerator) =
        // Analyze instructions flow 
        for i = 0 to instructions.Count - 1 do   
            let instr = instructions.[i]
            let nextInstr = instr.Next

            // Check if next instruction is a call to a workspace function
            if nextInstr <> null && nextInstr.OpCode = OpCodes.Call then
                if typeof<MethodInfo>.IsAssignableFrom(nextInstr.Operand.GetType()) then     
                    // Must generate a ldarg of the last parameter (WorkItemIdContainer) 
                    let m = nextInstr.Operand :?> MethodInfo                    
                    if m.DeclaringType.Name = "KernelLanguage" && (List.tryFind(fun (s:string) -> s = m.Name) workspaceFunctions).IsSome then
                        match kernel.GetParameters().Length with
                        | 1 ->
                            gen.Emit(OpCodes.Ldarg_1)
                        | 2 ->
                            gen.Emit(OpCodes.Ldarg_2)
                        | _ ->
                            gen.Emit(OpCodes.Ldarg_S, kernel.GetParameters().Length)

            // Check if the instruction is a call                     
            if instr.OpCode = OpCodes.Call then
                let m = instr.Operand :?> MethodInfo
                if m.DeclaringType.Name = "KernelLanguage" && (List.tryFind(fun (s:string) -> s = m.Name) workspaceFunctions).IsSome then
                    let mutable matchingName = m.Name.Replace("get_", "").Replace("_", "")
                    matchingName <- matchingName.Substring(0, 1).ToUpper() + 
                                    matchingName.Substring(1).Replace("id", "ID")
                                                             .Replace("size", "Size")
                                                             .Replace("groups", "Groups")
                                                             .Replace("offset", "Offset")
                                                             .Replace("dim", "Dim")
                    // Replace with a virtual call to the workitem id container function
                    gen.Emit(OpCodes.Callvirt, typeof<WorkItemIdContainer>.GetMethod(matchingName))
                else
                    gen.Emit(instr.OpCode, m)
            else
                // If the operand is a token must be generated again
                match instr.OpCode.OperandType with
                | OperandType.InlineNone ->
                    gen.Emit(instr.OpCode)
                | OperandType.ShortInlineI ->
                    if (instr.OpCode = OpCodes.Ldc_I4_S) then
                        gen.Emit(instr.OpCode, (sbyte)rawInstructions.[instr.Offset + instr.OpCode.Size])
                    else
                        gen.Emit(instr.OpCode, rawInstructions.[instr.Offset + instr.OpCode.Size])
                | OperandType.InlineI ->
                    gen.Emit(instr.OpCode, BitConverter.ToInt32(rawInstructions, instr.Offset + instr.OpCode.Size)) 
                | OperandType.ShortInlineR ->
                    gen.Emit(instr.OpCode, BitConverter.ToSingle(rawInstructions, instr.Offset + instr.OpCode.Size))
                | OperandType.InlineR ->
                    gen.Emit(instr.OpCode, BitConverter.ToDouble(rawInstructions, instr.Offset + instr.OpCode.Size))
                | OperandType.InlineI8 ->
                    gen.Emit(instr.OpCode, BitConverter.ToInt64(rawInstructions, instr.Offset + instr.OpCode.Size))
                | OperandType.InlineString ->
                    let str = kernel.Module.ResolveString (BitConverter.ToInt32(rawInstructions, instr.Offset + instr.OpCode.Size))
                    gen.Emit(instr.OpCode, str)
                | OperandType.InlineTok  | OperandType.InlineType | OperandType.InlineMethod | OperandType.InlineField ->
                    let type_arguments = 
                        if kernel.DeclaringType <> null then
                            kernel.DeclaringType.GetGenericArguments()
                        else
                            null
                    let method_arguments = 
                        if kernel.IsGenericMethod then
                            kernel.GetGenericArguments()
                        else
                            null

                    let str = kernel.Module.ResolveMember(BitConverter.ToInt32(rawInstructions, instr.Offset + instr.OpCode.Size), type_arguments, method_arguments)                 
                    match instr.OpCode.OperandType with
                    | OperandType.InlineType ->
                        gen.Emit(instr.OpCode, str :?> TypeInfo)
                    | OperandType.InlineMethod ->
                        gen.Emit(instr.OpCode, str :?> MethodInfo)
                    | OperandType.InlineField ->                        
                        gen.Emit(instr.OpCode, str :?> FieldInfo)  
                    | _->
                        raise (NotSupportedException())                        
                | _ ->
                    raise (NotSupportedException())
                
    member this.CreateMultithreadKernel(kernel: MethodInfo) = 
        // Generate dynamic method inside dynamic assembly        
        let paramTypes = Array.append (Array.map(fun (p:ParameterInfo) -> p.ParameterType) (kernel.GetParameters())) [| typeof<WorkItemIdContainer> |]
        let methodName = IDGenerator.GenerateUniqueID(kernel.Name + "_")
        let methodBuilder = moduleBuilder.DefineGlobalMethod(methodName, MethodAttributes.Public ||| MethodAttributes.Static, kernel.ReturnType, paramTypes)

        // Define method parameters
        let additionalParameterName = IDGenerator.GenerateUniqueID("workItemIdContainer_")
        for i in 0 .. kernel.GetParameters().Length - 1 do 
            methodBuilder.DefineParameter(i + 1, kernel.GetParameters().[i].Attributes, kernel.GetParameters().[i].Name) |> ignore
        methodBuilder.DefineParameter(kernel.GetParameters().Length + 1, ParameterAttributes.None, "WorkItemIdContainer") |> ignore

        // Get kernel instructions
        let instructions = kernel.GetInstructions()
        let rawInstructions = kernel.GetMethodBody().GetILAsByteArray()

        let temp = this.GetType().GetMethod("VectorAdd").GetInstructions()
        // Create IL generator
        let gen = methodBuilder.GetILGenerator()
       
        // Define local variable mapping from source to dynamic method
        let localVars = new Dictionary<LocalVariableInfo, obj>()
        for v in kernel.GetMethodBody().LocalVariables do
            localVars.Add(v, gen.DeclareLocal(v.LocalType, v.IsPinned))
            
        // Process kernel body and generate new method IL
        this.ProcessKernelBody(kernel, instructions, rawInstructions, gen)

        (*
        let paramTypes = Array.append paramTypes [| typeof<WorkItemIdContainer> |]
        let delegateType = 
            match paramTypes.Length with 
            | 1 ->
                typeof<Func<_, _>>
            | 2 ->
                typeof<Func<_, _, _>>
            | 3 ->
                typeof<Func<_, _, _, _>>
            | 4 ->
                typeof<Func<_, _, _, _, _>>
            | 5 ->
                typeof<Func<_, _, _, _, _, _>>
            | 6 ->
                typeof<Func<_, _, _, _, _, _, _>>
            | 7 ->
                typeof<Func<_, _, _, _, _, _, _, _>>
            | 8 ->
                typeof<Func<_, _, _, _, _, _, _, _, _>>
            | 9 ->
                typeof<Func<_, _, _, _, _, _, _, _, _, _>>
            | 10 ->
                typeof<Func<_, _, _, _, _, _, _, _, _, _, _>>

        let genericDelegateType = delegateType.GetGenericTypeDefinition().MakeGenericType(Array.append paramTypes [| m.ReturnType |])
        let del = m.CreateDelegate(typeof<genericDelegateType>) *)
        
        // Finalize function creation
        moduleBuilder.CreateGlobalFunctions()
        let mm = moduleBuilder.GetMethod(methodName) 
        let ins = mm.GetInstructions()
        mm

    
    (*
    let mutable offset = 0
    for i = 0 to instructions.Length - 1 do   
        let instr = instructions.[i]
        let nextInstr = instr.Next
        // Check if next instruction is a specific call
        if nextInstr <> null && nextInstr.OpCode = OpCodes.Call then
            if typeof<MethodInfo>.IsAssignableFrom(nextInstr.Operand.GetType()) then      
                let m = nextInstr.Operand :?> MethodInfo
                if m.Name = "get_global_id" then
                    // Must generate a ldarg of the last parameter (WorkItemIdContainer)
                    match m.GetParameters().Length with
                    | 1 ->
                        rawInstructions.Insert(instr.Offset, (byte)OpCodes.Ldarg_2.Value)
                        offset <- offset + 1
                    | 2 ->
                        rawInstructions.Insert(instr.Offset, (byte)OpCodes.Ldarg_3.Value)                        
                        offset <- offset + 1
                    | _ ->
                        rawInstructions.InsertRange(instr.Offset, Array.append (BitConverter.GetBytes(OpCodes.Ldarg_S.Value)) (BitConverter.GetBytes(m.GetParameters().Length)))
                        offset <- offset + OpCodes.Ldarg_S.Size + 1
                        
        if (instr.OpCode = OpCodes.Call) then
            rawInstructions.RemoveRange(instr.Offset + offset, instr.Size)
            rawInstructions.InsertRange(instr.Offset + offset,
                                        Array.append [| (byte)OpCodes.Callvirt.Value |] (BitConverter.GetBytes(ilInfo.GetTokenFor(typeof<WorkItemIdContainer>.GetMethod("GlobalId").MethodHandle))))                                        
        else
            // If the operand is a token must be generated again
            match instr.OpCode.OperandType with
            | OperandType.InlineSig ->
                rawInstructions.RemoveRange(instr.Offset + offset, instr.Size)
                let signature = meth.Module.ResolveSignature (BitConverter.ToInt32(rawInstructions.ToArray(), instr.Offset + offset + instr.OpCode.Size))
                rawInstructions.InsertRange(instr.Offset + offset, Array.append [| (byte)instr.OpCode.Value |] (BitConverter.GetBytes(ilInfo.GetTokenFor(signature))))            
            | OperandType.InlineString ->
                rawInstructions.RemoveRange(instr.Offset + offset, instr.Size)
                let str = meth.Module.ResolveString (BitConverter.ToInt32(rawInstructions.ToArray(), instr.Offset + offset + instr.OpCode.Size))
                rawInstructions.InsertRange(instr.Offset + offset, Array.append [| (byte)instr.OpCode.Value |] (BitConverter.GetBytes(ilInfo.GetTokenFor(str))))  
            | OperandType.InlineTok
            | OperandType.InlineType
            | OperandType.InlineMethod 
            | OperandType.InlineField ->
                rawInstructions.RemoveRange(instr.Offset + offset, instr.Size)
                let type_arguments = 
                    if meth.DeclaringType <> null then
                        meth.DeclaringType.GetGenericArguments()
                    else
                        null
                let method_arguments = 
                    if meth.IsGenericMethod then
                        meth.GetGenericArguments()
                    else
                        null

                let str = meth.Module.ResolveMember(BitConverter.ToInt32(rawInstructions.ToArray(), instr.Offset + offset + instr.OpCode.Size), type_arguments, method_arguments) 
                let tok = 
                    match instr.OpCode.OperandType with
                    | OperandType.InlineType ->
                        ilInfo.GetTokenFor((str :?> TypeInfo).TypeHandle)
                    | OperandType.InlineMethod ->
                        ilInfo.GetTokenFor((str :?> MethodInfo).MethodHandle)
                    | OperandType.InlineField ->                        
                        ilInfo.GetTokenFor((str :?> FieldInfo).FieldHandle)
                rawInstructions.InsertRange(instr.Offset + offset, Array.append [| (byte)instr.OpCode.Value |] (BitConverter.GetBytes(tok)))  
            | OperandType.InlineNone ->
                ()
            | _ ->
                ()
            *)    