namespace FSCL.Runtime.Scheduling
open System
open MathNet
open MathNet.Numerics

type FeretSchedulingEngine() = 
    member this.Analyze(features: float[,], times: float[,]) =
        let X = LinearAlgebra.Double.DenseMatrix.OfArray(features)
        let y = LinearAlgebra.Double.DenseMatrix.OfArray(times)
        let svd = X.Svd(true)
        let W = svd.W
        let s = svd.S

        let tolerance = Precision.EpsilonOf(2.0) * (double)(Math.Max(X.RowCount, X.ColumnCount)) * W.[0, 0]
        for i = 0 to s.Count - 1 do
            if s.[i] < tolerance then
                s.[i] <- 0.0
            else
                s.[i] <- 1.0 / s.[i]
        W.SetDiagonal(s)

        // (U * W * VT)T is equivalent with V * WT * UT 
        (svd.U * W * svd.VT).Transpose().Multiply(y).ToArray()

    