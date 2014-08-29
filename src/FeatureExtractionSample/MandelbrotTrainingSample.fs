module MandelbrotTrainingSample

open FSCL
open FSCL.Compiler
open FSCL.Runtime.Scheduling
open FSCL.Runtime.Scheduling.FeatureExtraction
open FSCL.Language
open System.Collections.Generic
open System
open System.IO
open FSCL.Runtime
open FSCL.Language
open System.Diagnostics

[<ReflectedDefinition>]
let Mandelbrot(a: uint[],
               [<AddressSpace(AddressSpace.Constant)>] colormap: uint[],
               nx: int, 
               ny: int,
               offset: int,
               lda: int,
               leftX: float32,
               topY: float32,
               stepX: float32,
               stepY: float32,
               maxIt: int,
               wi: WorkItemInfo) =

    for iy = 0 to ny - 1 do
        for ix = 0 to nx - 1 do
            let xpix = (wi.GlobalID(0) * nx) + ix
            let ypix = (wi.GlobalID(0) * ny) + iy

            let xc = ((float32)leftX) + (((float32)xpix) * stepX)
            let yc = ((float32)topY) - (((float32)ypix) * stepY)

            let mutable x = 0.0f
            let mutable y = 0.0f
            let mutable it = 0
            let mutable doBreak = 0
            while it < maxIt && doBreak = 0 do
                let x2 = x * x
                let y2 = y * y
                if x2 + y2 > 4.0f then
                    doBreak <- 1
                else
                    let twoxy = 2.0f * x * y
                    x <- x2 - y2 + xc
                    y <- twoxy + yc
                    it <- it + 1

            let color = 
                if it < maxIt then
                    colormap.[it]
                else
                    0xFF000000ul
            a.[offset+xpix+lda*ypix] <- color
