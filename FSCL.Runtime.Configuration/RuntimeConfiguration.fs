namespace FSCL.Compiler.Configuration

open System
open System.IO
open System.Reflection
open FSCL.Runtime
open System.Collections.Generic
open System.Xml
open System.Xml.Linq
open Microsoft.FSharp.Reflection

///
///<summary>
///Kind of compiler source: assembly object or file path
///</summary>
///
type RuntimeSource =
| AssemblySource of Assembly
| FileSource of string

type KernelDiscoveryProcessorConfiguration(i: string, t: Type, ?dependencies, ?before) =
    ///
    ///<summary>
    ///The ID of the step processors
    ///</summary>
    ///
    member val ID = i with get
    ///
    ///<summary>
    ///The step processors dependencies. See <see cref="FSCL.Compiler.StepProcessorAttribute"/>
    ///</summary>
    ///
    member val Dependencies = if dependencies.IsSome then dependencies.Value else [] with get
    ///
    ///<summary>
    ///The set of processors that must be executed after this
    ///</summary>
    ///
    member val Before = if before.IsSome then before.Value else [] with get
    ///
    ///<summary>
    ///The runtime .NET type of the step
    ///</summary>
    ///
    member val Type = t with get

    member internal this.ToXml() =
        let el = new XElement(XName.Get(this.GetType().Name),
                    new XAttribute(XName.Get("ID"), this.ID),
                    new XAttribute(XName.Get("Type"), this.Type.FullName),
                    new XElement(XName.Get("Dependencies"),
                        Array.ofSeq(seq {
                            for item in this.Dependencies do
                                yield XElement(XName.Get("Item"), XAttribute(XName.Get("ID"), item))
                        })),
                    new XElement(XName.Get("Before"),
                        Array.ofSeq(seq {
                            for item in this.Before do
                                yield XElement(XName.Get("Item"), XAttribute(XName.Get("ID"), item))
                        })))
        el
    static member internal FromXml(el:XElement, a:Assembly) =
        let deps = List<string>()
        let bef = List<string>()
        for d in el.Elements(XName.Get("Dependencies")) do
            for item in d.Elements(XName.Get("Item")) do
                deps.Add(item.Attribute(XName.Get("ID")).Value)
        for d in el.Elements(XName.Get("Before")) do
            for item in d.Elements(XName.Get("Item")) do
                bef.Add(item.Attribute(XName.Get("ID")).Value)
        KernelDiscoveryProcessorConfiguration(el.Attribute(XName.Get("ID")).Value, 
                                               a.GetType(el.Attribute(XName.Get("Type")).Value), 
                                               List.ofSeq deps, List.ofSeq bef)

type SourceConfiguration(src: RuntimeSource, 
                         p: KernelDiscoveryProcessorConfiguration list) =     
    ///
    ///<summary>
    ///The path of the source file or the instance of the source assembly
    ///</summary>
    ///
    member val Source = 
        match src with
        | AssemblySource(a) ->
            AssemblySource(a)
        | FileSource(s) ->
            if (Path.IsPathRooted(s)) then
                FileSource(s)
            else
               FileSource(Path.Combine(Directory.GetCurrentDirectory(), s))               
    
    member val KernelDiscoveryProcessors = p with get    
    ///
    ///<summary>
    ///Whether this source is explicit or not
    ///</summary>
    ///<remarks>
    ///An explicit source is a source where all the components in the assembly/file and all the related information are explicitated. 
    ///In an implicit source the set of steps, processors and type handlers are instead to be found and parsed through reflection,
    ///investigating the types declared in the assembly and their eventual step, processor or type handler attributes.
    ///</remarks>
    ///
    member this.IsExplicit
        with get() =
            (this.KernelDiscoveryProcessors.Length > 0)
            
    member internal this.IsDefault 
        with get() =
            match this.Source with
            | FileSource(s) ->
                let a = Assembly.LoadFile(s)
                a.GetCustomAttribute<DefaultComponentAssembly>() <> null
            | AssemblySource(a) ->
                a.GetCustomAttribute<DefaultComponentAssembly>() <> null     
    ///
    ///<summary>
    ///Constructor to instantiate an implicit compiler source
    ///</summary>
    ///
    new(src: RuntimeSource) = 
        SourceConfiguration(src, [])

    member internal this.ToXml() =
        let el = new XElement(XName.Get(this.GetType().Name),
                    match this.Source with
                    | FileSource(s) ->
                        XAttribute(XName.Get("FileSource"), s)
                    | AssemblySource(s) ->
                        XAttribute(XName.Get("AssemblySource"), s.FullName))
        if (this.IsExplicit) then
            el.Add(new XElement(XName.Get("Components"),
                        new XElement(XName.Get("KernelDiscoveryProcessors"),
                            Array.ofSeq(seq {
                                for item in this.KernelDiscoveryProcessors do
                                    yield item.ToXml()
                            }))))
        el
        
    static member internal FromXml(el: XElement, srcRoot: string) =
        let mutable source = FileSource("")
        let mutable assembly = Assembly.GetExecutingAssembly()
        let root = srcRoot

        // Determine source type (file or assembly name) and load assembly
        if (el.Attribute(XName.Get("AssemblySource")) <> null) then
            assembly <- Assembly.Load(el.Attribute(XName.Get("AssemblySource")).Value)
            source <- AssemblySource(assembly)
        else            
            if Path.IsPathRooted(el.Attribute(XName.Get("FileSource")).Value) then
                source <- FileSource(el.Attribute(XName.Get("FileSource")).Value)
                assembly <- Assembly.LoadFile(el.Attribute(XName.Get("FileSource")).Value)
            else
                source <- FileSource(Path.Combine(root, el.Attribute(XName.Get("FileSource")).Value))
                assembly <- Assembly.LoadFile(Path.Combine(root, el.Attribute(XName.Get("FileSource")).Value))

        // Check if explicit or implicit
        if (el.Element(XName.Get("Components")) <> null) then
            let sp = new List<KernelDiscoveryProcessorConfiguration>()
            if (el.Element(XName.Get("Components")).Element(XName.Get("KernelDiscoveryProcessors")) <> null) then
                for item in el.Element(XName.Get("Components")).Element(XName.Get("KernelDiscoveryProcessors")).Elements() do
                    sp.Add(KernelDiscoveryProcessorConfiguration.FromXml(item, assembly))
            
            let conf = new SourceConfiguration(source, List.ofSeq sp)
            conf
        else
            let conf = new SourceConfiguration(source)
            conf
            
    member internal this.MakeExplicit() =
        if not this.IsExplicit then            
            let assembly = 
                match this.Source with
                | AssemblySource(a) ->
                    a
                | FileSource(f) ->
                    Assembly.LoadFile(f)

            // Load assembly and analyze content
            let sp = new List<KernelDiscoveryProcessorConfiguration>()
            for t in assembly.GetTypes() do
                let dep = List<string>()
                let bef = List<string>()
        
                let spAttribute = t.GetCustomAttribute<KernelDiscoveryProcessorAttribute>()
                if spAttribute <> null then
                    for item in spAttribute.Before do
                        bef.Add(item)
                    for item in spAttribute.Dependencies do
                        dep.Add(item)
                    sp.Add(KernelDiscoveryProcessorConfiguration(spAttribute.ID, t, List.ofSeq dep, List.ofSeq bef))

            // Return
            SourceConfiguration(this.Source, List.ofSeq sp)
        else
            this  
             
///
///<summary>
///Configuration of a compiler instance
///</summary>
///
type RuntimeConfiguration(defSteps, sources: SourceConfiguration list) =
    ///
    ///<summary>
    ///The set of components sources
    ///</summary>
    ///
    member val Sources = sources    
    ///
    ///<summary>
    //Instantiates a default configuration, which is made by the set of sources of the native compiler components
    ///</summary>
    ///
    new(defSteps) =
        RuntimeConfiguration(defSteps, [])        
    ///
    ///<summary>
    ///Whether or not this configuration is a default configuration
    ///</summary>
    ///
    member this.IsDefault 
        with get() =
            if this.Sources.IsEmpty then
                false
            else
                this.Sources |> List.map(fun (el:SourceConfiguration) -> el.IsDefault) |> List.reduce(fun (el1:bool) (el2:bool) -> el1 && el2)
    ///
    ///<summary>
    ///Whether or not the native components must be merged with this configuration
    ///</summary>
    ///
    member this.LoadDefaultSteps 
        with get() = 
            defSteps && (not this.IsDefault)

    member internal this.ToXml() =
        let el = new XElement(XName.Get(this.GetType().Name),
                    new XAttribute(XName.Get("LoadDefaultSteps"), this.LoadDefaultSteps),
                    new XElement(XName.Get("Sources"),
                        Array.ofSeq(seq {
                            for item in this.Sources do
                                yield item.ToXml()
                        })))
        let doc = new XDocument(el)
        doc
        
    static member internal FromXml(doc: XDocument, srcRoot: string) =
        let sources = new List<SourceConfiguration>()
        let loadDefault = if(doc.Root.Attribute(XName.Get("LoadDefaultSteps")) <> null) then
                            bool.Parse(doc.Root.Attribute(XName.Get("LoadDefaultSteps")).Value)
                          else
                             false
        for s in doc.Root.Elements(XName.Get("Sources")) do
            for source in s.Elements() do
                sources.Add(SourceConfiguration.FromXml(source, srcRoot))
        
        RuntimeConfiguration(loadDefault, List.ofSeq sources)
        
    member internal this.MakeExplicit() =
        let sources = new List<SourceConfiguration>()
        for item in this.Sources do
            // Filter out assembly with no components even if made explicit
            let exp = item.MakeExplicit()
            if exp.IsExplicit then
                sources.Add(item.MakeExplicit())        
        RuntimeConfiguration(this.LoadDefaultSteps, List.ofSeq sources)
        
    member internal this.MergeDefault(def: RuntimeConfiguration) =
        if this.LoadDefaultSteps then
            let sources = new List<SourceConfiguration>()
            for s in this.MakeExplicit().Sources do
                sources.Add(s)
            // Merge with default sources
            for s in def.MakeExplicit().Sources do
                sources.Add(s)
            RuntimeConfiguration(false, List.ofSeq sources)
        else
            this.MakeExplicit()

                            
       
            

