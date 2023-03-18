namespace Quipu

module KolmogorovSmirnov =

    /// Compare a sample of observations to a reference distribution
    [<RequireQualifiedAccess>]
    module Reference =

        // probability of kolmogorov value <= x
        let kolmogorovCumulative precision x =
            let minChange = 0.5 * precision
            let sum =
                1
                |> Seq.unfold (fun i ->
                    let value =
                        (pown (-1.0) (i - 1))
                        *
                        (exp (-2.0 * (float i) * (float i) * x * x))
                    if abs value < minChange
                    then None
                    else Some (value, i + 1)
                    )
                |> Seq.sum
            1.0 - 2.0 * sum

        let errorCorrection x n =
            x + (1.0 / (6.0 * sqrt (float n))) + ((x - 1.0) / (4.0 * float n))

        /// Returns the largest difference observed between the empirical and
        /// theoretical cumulative distributions,
        /// and the probability to observe a difference that large,
        /// assuming the samples come from the provided underlying distribution.
        let compare (sample: float [], cumulative: float -> float) =

            let sample =
                sample
                |> Array.sort
            let size = sample.Length

            let maxDifference =
                sample
                |> Seq.mapi (fun i x ->
                    let p = float (i + 1) / float size
                    abs (p - cumulative x)
                    )
                |> Seq.max

            let correctedDifference =
                let reference = (sqrt (float size) * maxDifference)
                errorCorrection reference size

            maxDifference,
            1.0 - kolmogorovCumulative 0.001 correctedDifference

    [<RequireQualifiedAccess>]
    module Samples =

        let c alpha =
            sqrt (0.5 * (- log (alpha / 2.0)))

        let sampleCorrection (size1, size2) =
            sqrt (float (size1 + size2) / float (size1 * size2))

        let findAlpha diff (size1, size2) =
            let corr = sampleCorrection (size1, size2)
            let startSearch =
                let x = c 0.5 * corr < diff
                if x
                then 0.0, 0.5
                else 0.5, 1.0
            let rec binSearch (low, high) =
                let midpoint = (low + high) / 2.0
                if abs (low - high) < 0.001
                then midpoint
                else
                    let x = c midpoint * corr < diff
                    if x
                    then binSearch (low, midpoint)
                    else binSearch (midpoint, high)
            binSearch startSearch

        /// Identify the largest difference between two empirical distributions.
        let maximumDifference (sample1: float [], sample2: float []) =

            let sample1 = sample1 |> Array.sort
            let sample2 = sample2 |> Array.sort

            let size1 = sample1.Length
            let size2 = sample2.Length

            let rec walk acc (cumul1, cumul2) (index1, index2) =
                if (index1 = size1 - 1) || (index2 = size2 - 1)
                then acc
                else
                    let v1 = sample1.[index1 + 1]
                    let v2 = sample2.[index2 + 1]
                    let index1, cumul1 =
                        if v1 <= v2
                        then index1 + 1, float (index1 + 2) / float size1
                        else index1, cumul1
                    let index2, cumul2 =
                        if v2 <= v1
                        then index2 + 1,  float (index2 + 2) / float size2
                        else index2, cumul2
                    let acc = max acc (abs (cumul1 - cumul2))
                    walk acc (cumul1, cumul2) (index1, index2)

            walk 0.0 (0.0, 0.0) (-1, -1)

        /// Test: are the two samples drawn from the sample underlying
        /// distribution?
        /// Returns the largest difference observed between the empirical
        /// cumulative distributions, and the probability to observe a
        /// difference that large, assuming the 2 samples come from the same
        /// underlying distribution.
        let compare (sample1: float [], sample2: float []) =

            let maxDifference = maximumDifference (sample1, sample2)

            let size1 = sample1.Length
            let size2 = sample2.Length

            let criticalAlpha = findAlpha maxDifference (size1, size2)

            maxDifference, criticalAlpha