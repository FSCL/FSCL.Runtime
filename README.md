FSCL.Runtime
============

F# kernel scheduler and runtime support
-----------------------------------------

### Latest news

*****

October 15, 2013: FSCL.Runtime and Compiler compile correctly. Various updates to the Runtime. Sorry for the inconvenience.

*****

###At a glance
The FSCL runtime is built on top of the [FSCL compiler project](https://github.com/GabrieleCocco/FSCL.Compiler) to enable executing
OpenCL kernels inside .NET.
The FSCL runtime provides two main services:

+ Manage kernel execution (compile OpenCL to device executable, handle buffer allocation/reading/writing, synchronization, etc.)
+ Provide a plugin-based device-selection system to schedule a kernel on the most suitable device

###Usage
To use the FSCL runtime to execute F# kernels, programmers must:

1. Link the appropriate compiler libraries: *FSCL.Compiler.dll* and *FSCL.Compiler.Language.dll*;
2. Link the appropriate runtime libraries: *FSCL.Runtime.dll*;
3. Open the appropriate namespaces: *FSCL.Compiler*, *FSCL.Compiler.KernelLanguage*, *FSCL.Runtime* and *FSCL.Runtime.HostLanguage*;
4. Write the F# kernel *Fun* and mark it with the *ReflectedDefinition attribute*;
5. Execute the kernel invoking *<@@ Fun(x) @@>.Run(globalSize, localSize)*





