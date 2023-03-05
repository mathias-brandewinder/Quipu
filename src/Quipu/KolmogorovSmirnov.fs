namespace Quipu

module KolmogorovSmirnov =

    let distribution (sample: float [], cumulative: float -> float) =

        let sample = sample |> Array.sort
        let size = sample.Length

        sample
        |> Seq.mapi (fun i x ->
            let p = float (i + 1) / float size
            abs (p - cumulative x)
            )
        |> Seq.max

    let samples (sample1: float [], sample2: float []) =

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
                printfn $"{index1}, {index2} | {cumul1}, {cumul2}"
                walk acc (cumul1, cumul2) (index1, index2)

        let maxDifference = walk 0.0 (0.0, 0.0) (-1, -1)

        maxDifference