﻿namespace FSCL.Runtime
open System
open System.Collections.Generic

// Kernel run mode
type KernelRunningMode =
| OpenCL
| Multithread
| Sequential

module HostLanguage =
    let notused b =
        b

    let worksize(comp, globalSize:int array, localSize:int array) =
        comp
        
        
        


