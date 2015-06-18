### 2.0.1 - 17 July 2015
* Update scaffold

### 2.0 - 17 July 2015
* Update to compiler 2.0

### 1.4.0 - 30 October 2014
* Update to compiler 1.5.5

### 1.3.9 - 30 October 2014
* Fixed pointing to wrong compiler nuget package

### 1.3.6 - 29 October 2014
* Bug fix

### 1.3.5 - 28 October 2014
* Using new compier 1.5.0
* Scheduling engine infrastructure
* FRT (Feature-Regression-Time) scheduling engine prepared to be the default for the runtime

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
