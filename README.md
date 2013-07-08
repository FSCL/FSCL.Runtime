FSCL.Runtime
============

F# kernel scheduler and runtime support
-----------------------------------------

###At a glance
The FSCL runtime is built on top of the [FSCL compiler project](https://github.com/GabrieleCocco/FSCL.Compiler) to enable executing
OpenCL kernels inside .NET.
The FSCL runtime provides two main services:

+Manage kernel execution (compile OpenCL to device executable, handle buffer allocation/reading/writing, synchronization, etc.)
+Provide a plugin-based device-selection system to schedule a kernel on the most suitable device


