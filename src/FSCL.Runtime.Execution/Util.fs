namespace FSCL.Runtime

open System
open System.Reflection
open Microsoft.FSharp.Quotations
open System.Collections.Generic
open Microsoft.FSharp.Reflection

type Util() =
    static member ConvertToType<'T>(o:obj) =
        o :?> 'T

    static member ConvertMethodInfo = 
        typeof<Util>.GetMethod("ConvertToType", BindingFlags.Static ||| BindingFlags.Public).GetGenericMethodDefinition()

    static member inline Convert(t:Type, o:obj) =
        Util.ConvertMethodInfo.MakeGenericMethod([|t|]).Invoke(null, [|o|])
        
    static member RunParallelAsyncGeneric1<'U>(task: obj -> obj, input:Array) =  
        seq {
            for item in input do
                yield async { 
                    return task item  :?> 'U
                }
        } |> Async.Parallel |> Async.RunSynchronously
        
    static member RunParallelAsyncGeneric2<'U>(task: obj -> obj, input1:Array, input2:Array) =  
        seq {
            for i = 0 to input1.Length - 1 do     
                yield async { 
                    return task (input1.GetValue(i), input2.GetValue(i))  :?> 'U
                }
                
        } |> Async.Parallel |> Async.RunSynchronously
        
    static member RunParallelAsync1GenericMethodInfo = 
        typeof<Util>.GetMethod("RunParallelAsyncGeneric1", BindingFlags.Static ||| BindingFlags.Public).GetGenericMethodDefinition()
        
    static member RunParallelAsync2GenericMethodInfo = 
        typeof<Util>.GetMethod("RunParallelAsyncGeneric2", BindingFlags.Static ||| BindingFlags.Public).GetGenericMethodDefinition()
        
    static member RunParallelAsync(task: obj -> obj, input:Array, outputType: Type) =  
        Util.RunParallelAsync1GenericMethodInfo.MakeGenericMethod([| outputType |]).Invoke(null, [| task; input |])      

    static member RunParallelAsync(task: obj -> obj, input1:Array, input2:Array, outputType: Type) =  
        Util.RunParallelAsync2GenericMethodInfo.MakeGenericMethod([| outputType |]).Invoke(null, [| task; input1; input2 |])      

    static member AssociateObjArgToLambdaVars(o: obj, vars: Var list) =
        let dict = new Dictionary<Var, obj>()
        if vars.Length = 0 then
            failwith "Cannot associate an object to an empty set of variables"
        elif vars.Length = 1 then
            dict.Add(vars.[0], o)
        else
            // Check if obj is a tuple
            if FSharpType.IsTuple(o.GetType()) then
                let vals = FSharpValue.GetTupleFields(o)
                if (vals.Length <> vars.Length) then
                    failwith "The set of variables and values to be associated must have the same cardinality"
                else
                    for i = 0 to vars.Length - 1 do
                        dict.Add(vars.[i], vals.[i])
            else
                failwith "The set of variables implies a tuple argument, but the argument is not a tuple"
        dict
        
    static member MergeLambdaVarsToObjArgDictionaries(o1:Dictionary<Var, obj>, o2:Dictionary<Var, obj>) =
        for item in o2 do
            o1.Add(item.Key, item.Value)
