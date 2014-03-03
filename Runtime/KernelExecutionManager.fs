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
    val private pool: BufferPoolManager

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
                      pool = new BufferPoolManager();
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
                                  pool = new BufferPoolManager();
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
                                                 pool = new BufferPoolManager();
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
    member this.Execute(input:KernelExecutionInput, opts) =
        if this.steps.Length > 0 then
            let mutable state = this.steps.[0].Execute((input, this.pool) :> obj, opts)
            for i = 1 to this.steps.Length - 1 do
                state <- this.steps.[i].Execute((state, this.pool), opts)
            let result = state :?> KernelExecutionOutput
            
            // Read output buffers
            this.pool.TransferBackModifiedBuffers()

            // If has return buffer read it
            if result.ReturnBuffer.IsSome then
                let v = this.pool.ReadRootReturnBuffer()
                // Dispose all buffers
                this.pool.Dispose()
                v :> obj
            else if result.ReturnValue.IsSome then
                // Dispose all buffers
                this.pool.Dispose()
                result.ReturnValue.Value
            else
                this.pool.Dispose()
                null
        else
            null
          
    static member DefaultConfigurationRoot() =
        KernelExecutionManager.defConfCompRoot

    static member DefaultConfigurationComponentsRoot() =
        Path.Combine(KernelExecutionManager.defConfCompRoot, KernelExecutionManager.defConfCompRoot)

    static member DefaultComponents() =
        KernelExecutionManager.defComponentsAssemply
