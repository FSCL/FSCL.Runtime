### 1.3.4 - 15 September 2014
* Improved data transfer of reference types (records). Fixed bug in memory pinning. Added sample/test to verify correctness of all possible data transfer modes (map, enqueueRead/Write) and flags

### 1.3.2 - 23 August 2014
* Records and structs fully supported by both custom kernels and collection functions

### 1.3.1 - 23 August 2014
* Extended runtime support for structs and records. Now you can use both custom F# records and structs (and arrays of records and structs) as parameters of kernels and functions. Also, you can declare private/local structs and records using record initialisation construct, struct parameterless constructor and "special" struct constructor (a constructor taking N arguments, each of one matching one of the N fields, in the order). Records must be labeled with _LayoutKind.Sequential_ attribute. Important to note, records have lower performance, cause not blittable and must be marshalled into an unmanaged intptr (AllocHGlobal) and then copied to the OpenCL buffer (2 copies). Structs instead require one copy only (0 if UseHostPointer).
- Valid record decl: let myRec = { field1 = val1; ... fieldN = valN }
- Valid default struct decl: let myStruct = new MyStruct()
- Valid "special constructor" struct decl: let myStruct = new MyStruct(valForField1, valForField2, ... valForFieldN)
- NOT valid struct decl: let myStruct = new MyStruct(<Args where the i-TH is not a value assigned to the i-TH field>)

### 1.3 - 25 July 2014
* Restructured project according to F# Project Scaffold
* Replaced submodules with NuGet package dependencies
* Work size specification as part of kernel signature
* Started multithreading (explicit and fallback mode) implementation
