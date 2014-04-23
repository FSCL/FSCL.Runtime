module Kernels

open FSCL.Compiler
open FSCL.Compiler.Language

[<ReflectedDefinition>]
let BLOCK_SIZE = 16

[<ReflectedDefinition; DynamicConstantDefine>]
let mutable FILTER_WIDTH = 3

// Vector addition
[<ReflectedDefinition>]
let VectorAdd(a: float32[], b: float32[], c: float32[]) =
    let gid = get_global_id(0)
    c.[gid] <- a.[gid] + b.[gid]
        
// Matrix multiplication
[<ReflectedDefinition>]
let MatMul(matA: float32[,], matB: float32[,], matC: float32[,]) =
    // Block index
    let bx = get_group_id(0)
    let by = get_group_id(1) 
    // Thread index
    let tx = get_local_id(0)
    let ty = get_local_id(1)
    // Matrix width
    let wa = matA.GetLength(0)
    let wb = matB.GetLength(0)

    // Index of the first sub-matrix of B processed 
    // by the block
    let bCol = bx * BLOCK_SIZE
    let bBeginRow = 0
    // Step size used to iterate through the 
    // sub-matrices of B
    let bStep  = BLOCK_SIZE
 
    // Loop over all the sub-matrices of A and B
    // required to compute the block sub-matrix
    let mutable bRow = bBeginRow
    let mutable Csub = 0.0f
    
    // Declaration of the local memory array As 
    // used to store the sub-matrix of A
    let As = local(Array2D.zeroCreate<float32> BLOCK_SIZE BLOCK_SIZE)
 
    // Declaration of the local memory array Bs 
    // used to store the sub-matrix of B
    let Bs = local(Array2D.zeroCreate<float32> BLOCK_SIZE BLOCK_SIZE)

    for aCol in 0 .. BLOCK_SIZE .. (wa - 1) do
        // Load the matrices from global memory
        // to local memory; each thread loads
        // one element of each matrix
        As.[ty, tx] <- matA.[by * BLOCK_SIZE, aCol]
        Bs.[ty, tx] <- matB.[bRow, bCol]
 
        // Synchronize to make sure the matrices 
        // are loaded
        barrier(CLK_LOCAL_MEM_FENCE)
 
        // Multiply the two matrices together;
        // each thread computes one element
        // of the block sub-matrix
        for k = 0 to BLOCK_SIZE - 1 do
            Csub <- Csub + (As.[ty,k] * Bs.[k,tx])
 
        // Synchronize to make sure that the preceding
        // computation is done before loading two new
        // sub-matrices of A and B in the next iteration
        barrier(CLK_LOCAL_MEM_FENCE)

        bRow <- bRow + bStep
         
    // Write the block sub-matrix to device memory;
    // each thread writes one element
    matC.[by * BLOCK_SIZE + ty, bx * BLOCK_SIZE + tx] <- Csub
        
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
