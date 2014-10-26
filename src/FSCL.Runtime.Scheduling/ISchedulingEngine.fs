namespace FSCL.Runtime.Scheduling

open Microsoft.FSharp.Quotations
open System.Reflection
open System
open System.IO
open System.Xml.Linq
open OpenCL
open System.Collections.Generic

type ISchedulingEngine =
    interface 
        abstract member OnRuntimeLoad: OpenCLDeviceSet -> unit
        abstract member OnKernelCompile: obj -> obj
        abstract member OnKernelRun: obj * obj * obj list * IReadOnlyDictionary<string, obj> -> OpenCLDevice
    end

[<AbstractClass>]
type ISchedulingEngine<'KTYPE,'KDATA>() =
    interface ISchedulingEngine with
        override this.OnRuntimeLoad(devices) =
            this.OnRuntimeLoad(devices)
        override this.OnKernelCompile(o) =
            this.OnKernelCompile(o :?> 'KTYPE) :> obj
        override this.OnKernelRun(i, p, args, opt) =
            this.OnKernelRun(i :?> 'KTYPE, p :?> 'KDATA, args, opt)
    abstract OnRuntimeLoad: OpenCLDeviceSet -> unit 
    abstract OnKernelCompile: 'KTYPE -> 'KDATA
    abstract OnKernelRun: 'KTYPE * 'KDATA * obj list * IReadOnlyDictionary<string, obj> -> OpenCLDevice

    

