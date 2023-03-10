namespace Quipu

module KolmogorovSmirnov =

    /// Compare a sample of observations to a reference distribution
    [<RequireQualifiedAccess>]
    module Reference =

        // probability of kolmogorov value < x
        let q x =
            let sum =
                Seq.init 10_000 (fun i ->
                    let j = i + 1
                    (pown (-1.0) (j - 1))
                    *
                    (exp (-2.0 * (float j) * (float j) * x * x))
                    )
                |> Seq.sum
                |> (*) 2.0
            1.0 - sum

        let compare (sample: float [], cumulative: float -> float) =

            let sample =
                sample
                |> Array.sort
            let size = sample.Length

            let d =
                sample
                |> Seq.mapi (fun i x ->
                    let p = float (i + 1) / float size
                    abs (p - cumulative x)
                    )
                |> Seq.max

            d, q(sqrt (float size) * d)

    [<RequireQualifiedAccess>]
    module Samples =

        let criticalValue alpha =
            sqrt (0.5 * (- log (alpha / 2.0)))

        let sampleCorrection (size1, size2) =
            sqrt (float (size1 + size2) / float (size1 * size2))

        let findCritical diff (size1, size2) =
            let corr = sampleCorrection (size1, size2)
            let startSearch =
                let x = criticalValue 0.5 * corr < diff
                if x
                then 0.0, 0.5
                else 0.5, 1.0
            let rec binSearch (low, high) =
                let midpoint = (low + high) / 2.0
                if abs (low - high) < 0.001
                then midpoint
                else
                    let x = criticalValue midpoint * corr < diff
                    if x
                    then binSearch (low, midpoint)
                    else binSearch (midpoint, high)
            binSearch startSearch

        let compare (sample1: float [], sample2: float []) =

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

            let maxDifference = walk 0.0 (0.0, 0.0) (-1, -1)
            let criticalValue = findCritical maxDifference (size1, size2)

            maxDifference, (1.0 - criticalValue)