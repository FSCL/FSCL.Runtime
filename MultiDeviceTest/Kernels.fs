module Kernels
open FSCL.Compiler.Language

[<ReflectedDefinition>]
let BLOCK_SIZE = 16

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
    