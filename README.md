FSCL.Runtime
============

F# kernel scheduler and runtime support
-----------------------------------------

### Latest news

June 21, 2014: FSCL accelerated collections now support the following functions: *Array.map, Array.mapi, Array.map2, Array.mapi2, Array.rev, Array.sum and Array.reduce*.
Array.scan will be added soon.
The action to apply to the elements of the collection can be passed in the following ways (Array.map is taking as an example, but this holds for all the other functions):
+ *Array.map myReflectedMethod myArray*
  (myReflectedMethod is a function marked with ReflectedDefinitionAttribute)
+ *Array.map (fun el -> el + ... * ...) myArray*
+ *Array.map (fun el -> myReflectedMethod el someOtherParam1 someOtherParamN)*
  (Here we use a lambda just to call a reflected method. The reflected method may take some parameters in addition to "el". These parameters are evaluated at compile time and each reference is lifted out of the method body)

*****

June 20, 2014: I created two brand new solutions for the compiler and the runtime. I spent a lot of time recovering from misterious happenings that made xamarin studio loosing f# projects when opening solutions edited in VS 2012. I also encountered weird issues in opening with VS 2012 solutions created in xamarin studio. Among the others, an error related to the fsc task that could not be instantiated from the assembly FSharpBuild.dll (bla bla).
At the very end, I decided to create new solutions from scratch in VS 2012. Now both the compiler and the runtime should be opened correctly in VS 2012 & 2013. Also, xamarin studio should handle them preperly. It may happen that you "lose" some f# projects when you edit the solution (like adding a project or changing the startup one) in xamarin studio, you close xamarin studio, and you finally reopen it. Sorry for that, I'll try to address this problem by creating two different sln(s), one for VS 2012-2013 and one for xamarin studio (this one works well in VS 2013, but not in VS 2012 cause of th fsc task problem above).

*****

April 17, 2014: FSCL 1.2 is out! Checkout/pull the master branch to get all the latest features.
If you want to discover what's new in FSCL, jump to the FSCL blog: http://www.gabrielecocco.it/fscl/

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





