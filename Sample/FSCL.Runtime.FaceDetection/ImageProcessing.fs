
// NOTE: If warnings appear, you may need to retarget this project to .NET 4.0. Show the Solution
// Pad, right-click on the project node, choose 'Options --> Build --> General' and change the target
// framework to .NET 4.0 or .NET 4.5.


namespace FSCL.Runtime.FaceDetection

open FSCL.Compiler
open FSCL.Compiler.KernelLanguage
open Microsoft.FSharp.Reflection

module ImagePreprocessingKernels =

    [<ReflectedDefinition>]
    let rgb_to_grayscale_coeff = [| 0.299f; 0.587f; 0.114f; 0.0f |]
    [<ReflectedDefinition>]
    let rgb_to_grayscale_coeff_v = float4(0.299f, 0.587f, 0.114f, 0.0f)

    [<ReflectedDefinition>]
    let BgrToGrayscale(src:uchar[],
                       dst:uchar[],
                       width,
                       height,
                       stride) =
                       
        let coord = (get_global_id(1) * stride) + (get_global_id(0) * 3)
        
        let mutable temp = (rgb_to_grayscale_coeff.[2] * (float32)src.[coord] +
                            rgb_to_grayscale_coeff.[1] * (float32)src.[coord + 1] +
                            rgb_to_grayscale_coeff.[0] * (float32)src.[coord + 2])
        let c = clamp((uint32)temp, (uint32)0, (uint32)255)
        
        dst.[(get_global_id(1) * width) + get_global_id(0)] <- (byte)c

    [<ReflectedDefinition>]
    let BgraToGrayscale(src:uchar4[],
                        dst:uchar[],
                        width,
                        height,
                        stride) = 
                        
        let coord = (get_global_id(1) * stride) + (get_global_id(0) * 4)
        let data = src.[coord]
        let result1 = (float32)data.x * rgb_to_grayscale_coeff_v.x
        let result2 = (float32)data.y * rgb_to_grayscale_coeff_v.y
        let result3 = (float32)data.z * rgb_to_grayscale_coeff_v.z
        let result = clamp((uint32)(result1 + result2 + result3), (uint32)0, (uint32)255)
        dst.[(get_global_id(1) * width) + get_global_id(0)] <- (byte)result
    

    [<ReflectedDefinition>]
    let BgrToGrayscalePerRow(src:uchar3[],
                             dst:uchar4[],
                             width,
                             stride) =
                             
        let src_start = (get_global_id(0) * stride)
        let dst_start = (get_global_id(0) * (width >>> 2))
        let mutable dst_index = 0
        for i in 0 .. 4 .. width - 1 do 
            let mutable src_pixel = src.[src_start + i]
            let mutable dst_pixels = uchar4()
            let mutable temp = (uint32)(rgb_to_grayscale_coeff.[2] * (float32)src_pixel.x +
                                        rgb_to_grayscale_coeff.[1] * (float32)src_pixel.y +
                                        rgb_to_grayscale_coeff.[0] * (float32)src_pixel.z)
            dst_pixels.x <- (byte)(clamp((uint32)temp, (uint32)0, (uint32)255))
            
            src_pixel <- src.[src_start + i + 1];
            temp <- (uint32)(rgb_to_grayscale_coeff.[2] * (float32)src_pixel.x +
                             rgb_to_grayscale_coeff.[1] * (float32)src_pixel.y +
                             rgb_to_grayscale_coeff.[0] * (float32)src_pixel.z)
            dst_pixels.y <- (byte)(clamp((uint32)temp, (uint32)0, (uint32)255))
            
            src_pixel <- src.[src_start + i + 2];
            temp <- (uint32)(rgb_to_grayscale_coeff.[2] * (float32)src_pixel.x +
                             rgb_to_grayscale_coeff.[1] * (float32)src_pixel.y +
                             rgb_to_grayscale_coeff.[0] * (float32)src_pixel.z)
            dst_pixels.z <- (byte)(clamp((uint32)temp, (uint32)0, (uint32)255))
            
            src_pixel <- src.[src_start + i + 3];
            temp <- (uint32)(rgb_to_grayscale_coeff.[2] * (float32)src_pixel.x +
                             rgb_to_grayscale_coeff.[1] * (float32)src_pixel.y +
                             rgb_to_grayscale_coeff.[0] * (float32)src_pixel.z)
            dst_pixels.w <- (byte)(clamp((uint32)temp, (uint32)0, (uint32)255))
            
            dst.[dst_start + dst_index] <- dst_pixels
            dst_index <- dst_index + 1


    // Input is grayscale 8U image
    // Output is a (width + 1) * (height + 1) 32U image
    let IntegralImageSumRows(src:uchar[],
                             dst:uint[],
                             dst_square:ulong[],
                             width,
                             stride) =
                             
        // First row is 0
        let src_start = get_global_id(0) * stride
        let dst_start = (get_global_id(0) + 1) * (width + 1)
        
        dst.[dst_start] <- 0u
        let mutable sum = 0u
        let mutable sum_square = 0UL
        for col = 0 to width - 1 do
            let src_el = src.[src_start + col]
            sum <- sum + (uint32)src_el
            sum_square <- sum_square + (uint64)(src_el * src_el)
            dst.[dst_start + col + 1] <- sum
            dst_square.[dst_start + col + 1] <- sum_square

    // Input is a 32U image
    // Output is a 32U image
    let IntegralImageSumCols(src:uint[],
                             src_square:ulong[],
                             dst:ulong[],
                             dst_square:ulong[],
                             width,
                             height) =
                             
        // First columns is 0
        let start = (get_global_id(0) + 1)
        
        let mutable sum = 0UL
        for row = 0 to height - 1 do
            let src_el = src.[start + (row * (width + 1))]
            sum <- sum + (uint64)src_el
            dst.[start + (row * (width + 1))] <- sum
            dst_square.[start + (row * (width + 1))] <- sum
        

    let Invert(bmp:uchar[],
               temp:uchar[],
               width,
               height,
               stride) =
               
        let coord = (get_global_id(1) * stride) + (get_global_id(0) * 3)        
        let mutable pix = bmp.[coord]
        temp.[coord] <- 255uy - pix
        pix <- bmp.[coord + 1]
        temp.[coord + 1] <- 255uy - pix
        pix <- bmp.[coord + 2]
        temp.[coord + 2] <- 255uy - pix


