module Kernels

open FSCL.Compiler
open FSCL.Compiler.Language

[<ReflectedDefinition; DynamicConstantDefine>]
let mutable FILTER_WIDTH = 3

[<ReflectedDefinition>]
let Convolution(pInput: float32[], [<AddressSpace(AddressSpace.Constant)>] pFilter: float32[], pOutput: float32[], nInWidth: int) =
    let nWidth = get_global_size(0)
    let xOut = get_global_id(0) 
    let yOut = get_global_id(1)
    
    let xInTopLeft = xOut
    let yInTopLeft = yOut
    let mutable sum4 = float4(0.0f) 
    for r = 0 to FILTER_WIDTH - 1 do
        let idxFtmp = r * FILTER_WIDTH
        let yIn = yInTopLeft + r
        let idxIntmp = yIn * nInWidth + xInTopLeft
        let mutable c = 0
        let mutable c4 = 0L
        while (c <= FILTER_WIDTH - 4) do
            let filter4 = float4.vload(c4, pFilter.pasum(idxFtmp))
            let in4 = float4.vload(c4, pInput.pasum(idxIntmp))
            sum4 <- sum4 + in4 * filter4
            c <- c+ 4
            c4 <- c4 + 1L
        let cMod = FILTER_WIDTH - c
        if cMod = 1 then 
            let idxF = idxFtmp + c
            let idxIn = idxIntmp + c
            sum4.x <- sum4.x + pFilter.[idxF]*pInput.[idxIn]
        else if cMod = 2 then
            //Use float4 here to further optimize the kernel 
            let idxF = idxFtmp + c
            let idxIn = idxIntmp + c
            sum4.x <- sum4.x + pFilter.[idxF]* pInput.[idxIn]
            sum4.y <- sum4.y + pFilter.[idxF+1]*pInput.[idxIn+1]
        else if cMod = 3 then
            //Use float4 here to further optimize the kernel 
            let idxF = idxFtmp + c
            let idxIn = idxIntmp + c
            sum4.x <- sum4.x + pFilter.[idxF]*pInput.[idxIn]
            sum4.y <- sum4.y + pFilter.[idxF+1]*pInput.[idxIn+1]
            sum4.z <- sum4.z + pFilter.[idxF+2]*pInput.[idxIn+2]

    let idxOut = yOut * nWidth + xOut
    pOutput.[idxOut] <- sum4.x + sum4.y + sum4.z + sum4.w
