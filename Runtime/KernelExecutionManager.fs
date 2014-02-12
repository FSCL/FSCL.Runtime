namespace FSCL.Runtime

open System
open System.IO
open System.Reflection
open FSCL.Compiler
open FSCL.Compiler.Configuration
open FSCL.Runtime.KernelExecution
open Microsoft.FSharp.Reflection
///
///<summary>
///The FSCL Runtime Execution Manager
///</summary>
///
type KernelExecutionManager =  
    static member private defConfRoot = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "FSCL.Runtime")
    static member private defConfCompRoot = "Components"

    static member private defComponentsAssemply = 
        [| typeof<KernelExecutionStep>;
           typeof<ReduceKerernelExecutionProcessor> |]

    val mutable private steps : ICompilerStep list
    val mutable private configuration: PipelineConfiguration
    val mutable private configurationManager: PipelineConfigurationManager

    ///
    ///<summary>
    ///The default constructor of the compiler
    ///</summary>
    ///<returns>
    ///An instance of the compiler with the default configuration
    ///</returns>
    ///<remarks>
    ///Defalt configuration means that the FSCL.Compiler configuration folder is checked for the presence of a configuration file.
    ///If no configuration file is found, the Plugins subfolder is scanned looking for the components of the compiler to load.
    ///In addition of the 0 or more components found, the native components are always loaded (if no configuration file is found in the first step)
    ///</remarks>
    ///
    new() as this = { steps = []; 
                      configurationManager = new PipelineConfigurationManager(KernelExecutionManager.defComponentsAssemply, KernelExecutionManager.defConfRoot, KernelExecutionManager.defConfCompRoot); 
                      configuration = null }   
                    then
                        this.configuration <- this.configurationManager.DefaultConfiguration()
                        this.steps <- this.configurationManager.Build(this.configuration)
    
    ///
    ///<summary>
    ///The constructor to instantiate a compiler with a file-based configuration
    ///</summary>
    ///<param name="file">The absolute or relative path of the configuration file</param>
    ///<returns>
    ///An instance of the compiler configured with the input file
    ///</returns>
    ///
    new(file: string) as this = { steps = []; 
                                  configurationManager = new PipelineConfigurationManager(KernelExecutionManager.defComponentsAssemply, KernelExecutionManager.defConfRoot, KernelExecutionManager.defConfCompRoot); 
                                  configuration = null }   
                                then
                                    this.configuration <- this.configurationManager.LoadConfiguration(file)
                                    this.steps <- this.configurationManager.Build(this.configuration)
    ///
    ///<summary>
    ///The constructor to instantiate a compiler with an object-based configuration
    ///</summary>
    ///<param name="conf">The configuration object</param>
    ///<returns>
    ///An instance of the compiler configured with the input configuration object
    ///</returns>
    ///
    new(conf: PipelineConfiguration) as this = { steps = []; 
                                                 configurationManager = new PipelineConfigurationManager(KernelExecutionManager.defComponentsAssemply, KernelExecutionManager.defConfRoot, KernelExecutionManager.defConfCompRoot); 
                                                 configuration = conf }   
                                               then
                                                   this.steps <- this.configurationManager.Build(this.configuration)
    ///
    ///<summary>
    ///The compiler configuration
    ///</summary>
    ///                                                
    member this.Configuration 
        with get() =
            this.configuration
    ///
    ///<summary>
    ///The method to be invoke to compile a managed kernel
    ///</summary>
    ///  
    member this.Execute(input) =
        let mutable state = input
        for step in this.steps do
            state <- step.Execute(state)
        let result = state :?> KernelExecutionOutput
        if result.ReturnBuffers.Count = 0 then
            () :> obj
        else if result.ReturnBuffers.Count = 1 then
            result.ReturnBuffers.[0]
        else
            let retType = FSharpType.MakeTupleType(result.ReturnBuffers |> Seq.map(fun el -> el.GetType()) |> Seq.toArray)
            FSharpValue.MakeTuple(Seq.toArray (result.ReturnBuffers), retType)
          
    static member DefaultConfigurationRoot() =
        KernelExecutionManager.defConfCompRoot

    static member DefaultConfigurationComponentsRoot() =
        Path.Combine(KernelExecutionManager.defConfCompRoot, KernelExecutionManager.defConfCompRoot)

    static member DefaultComponents() =
        KernelExecutionManager.defComponentsAssemply
