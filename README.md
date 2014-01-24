FSCL.Runtime
============

F# kernel scheduler and runtime support
-----------------------------------------

### Latest news

*****

January 24, 2014: FSCL.Runtime and Compiler compile and execute on Mono 3.2.3 + OSX 10.8.5 and Visual Studio 2012 + Window 7 and Windows 8.

The repo has changed, the submodule Compiler is now tracking the master branch of the relative repo. Please do "git submodule update --remote" everytime you need want to update it.

*****

October 20, 2013: FSCL.Runtime and Compiler compile and execute on Mono 3.2.3 + OSX 10.8.5. 
Porting required only to add a configuration file to dll-map OSX OpenCL framework to the conventional name (OpenCL.dll) used by the .NET OpenCL wrapper. Sorry for the inconvenience.
Visual Studio solutions files can be open and built using Xamarin Studio with no effects on the repo content. 

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





