namespace FSCL.Runtime.Configuration

open FSCL.Runtime
open System
open System.IO
open System.Reflection
open System.Collections.Generic
open System.Xml
open System.Xml.Linq

exception RuntimerConfigurationException of string

type RuntimeConfigurationManager() = 
    // Trick to guarantee the default components assemblies are loaded
    static member private defAssemblyComponents = [typeof<KernelCallExpressionDiscovery>; ]

    // The root where to place configuration file
    static member ConfigurationRoot = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "FSCL.Runtime")
    // The root where to place plugins and configuration file
    static member ComponentsRoot = Path.Combine(RuntimeConfigurationManager.ConfigurationRoot, "Components")
    
    // Default configuration
    static member DefaultConfiguration() =
        let sources = List<SourceConfiguration>()
        // Create configuration from assembly
        for item in RuntimeConfigurationManager.defAssemblyComponents do
            let assembly = item.Assembly
            // Make configuration explicit
            sources.Add(SourceConfiguration(AssemblySource(assembly)))
        RuntimeConfiguration(false, List.ofSeq sources)

    // Load from configuration file    
    static member internal LoadConfiguration(cf:string) =
        let document = XDocument.Load(cf)
        let conf = RuntimeConfiguration.FromXml(document, Path.GetDirectoryName(Path.GetFullPath(cf)))
        conf
        
    // Load from configuration file    
    static member internal LoadConfiguration() =
        let conf = Path.Combine(RuntimeConfigurationManager.ConfigurationRoot, "FSCL.Config.xml")
        if not (File.Exists(conf)) then
            let sources = List<SourceConfiguration>()
            let pluginFolder = RuntimeConfigurationManager.ComponentsRoot
            if Directory.Exists(pluginFolder) then
                let dlls = Directory.GetFiles(pluginFolder, "*.dll")
                for f in dlls do
                    sources.Add(SourceConfiguration(FileSource(f)))
            RuntimeConfiguration(true, List.ofSeq sources)
        else
            RuntimeConfiguration(true)
            
    static member internal StoreConfiguration(conf: RuntimeConfiguration, f: string) =
        conf.MakeExplicit().ToXml().Save(f)
              
    static member internal Build() =
        let conf = RuntimeConfigurationManager.LoadConfiguration()
        RuntimeConfigurationManager.Build(conf)


    // Build from file
    static member internal Build(cf: string) =
        let conf = RuntimeConfigurationManager.LoadConfiguration(cf)
        RuntimeConfigurationManager.Build(conf)
        
    static member internal Build(conf: RuntimeConfiguration) =
        let explicitConf = conf.MergeDefault(RuntimeConfigurationManager.DefaultConfiguration())
        RuntimeBuilder.Build(explicitConf)

