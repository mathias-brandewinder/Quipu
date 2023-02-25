namespace Quipu

module NelderMead =

    let update (dim: int, f: float [] -> float) (simplex: (float []) []) =

        // 1) order the values, from best to worst
        let ordered =
            simplex
            |> Array.sortBy f

        // 2) calculate centroid
        let size = simplex.Length
        // drop the worst candidate
        let bestCandidates = ordered[.. size - 2]
        // calculate average point (centroid)
        let centroid =
            Array.init dim (fun col ->
                bestCandidates
                |> Array.averageBy(fun pt -> pt[col])
                )

        // 3) reflection
        let worst = ordered[size - 1]
        let alpha = 1.0
        let reflected =
            Array.init dim (fun col ->
                centroid[col] + alpha * (centroid[col] - worst[col])
                )
        let secondWorst = ordered[size - 2]
        let best = ordered[0]
        if
            f reflected < f secondWorst
            &&
            f reflected >= f best
        then
            // replace worst by reflected
            ordered[size - 1] <- reflected
            ordered

        // 4) expansion
        elif
            f reflected < f best
        then
            let gamma = 2.0
            let expanded =
                Array.init dim (fun col ->
                    centroid[col] + gamma * (reflected[col] - centroid[col])
                    )
            if f expanded < f reflected
            then
                ordered[size - 1] <- expanded
            else
                ordered[size - 1] <- reflected
            ordered

        // 5) contraction
        elif f reflected < f worst
        then
            let rho = 0.5
            let contractedOutside =
                Array.init dim (fun col ->
                    centroid[col] + rho * (reflected[col] - centroid[col])
                    )
            if f contractedOutside < f reflected
            then
                ordered[size - 1] <- contractedOutside
                ordered
            else
            // 6) shrink
                let sigma = 0.5
                let shrunk =
                    ordered
                    |> Array.map (fun pt ->
                        Array.init dim (fun col ->
                            best[col] + sigma * (pt[col] - best[col])
                            )
                        )
                shrunk
        elif f reflected >= f worst
        then
            let rho = 0.5
            let contractedInside =
                Array.init dim (fun col ->
                    centroid[col] + rho * (worst[col] - centroid[col])
                    )
            if f contractedInside < f worst
            then
                ordered[size - 1] <- contractedInside
                ordered
            else
            // 6) shrink
                let sigma = 0.5
                let shrunk =
                    ordered
                    |> Array.map (fun pt ->
                        Array.init dim (fun col ->
                            best[col] + sigma * (pt[col] - best[col])
                            )
                        )
                shrunk
        else
            failwith "All cases should have been covered"

    let terminate (tolerance: float) (f: float [] -> float) (simplex: float [][]) =
        // We stop when for every point in the simplex,
        // the function values are all close to each other.
        let evaluations = simplex |> Seq.map f
        let min = evaluations |> Seq.min
        let max = evaluations |> Seq.max
        max - min < tolerance

    let initialize (dim: int, f: float [] -> float) (startingPoint: float []) =
        [|
            yield startingPoint
            for d in 0 .. (dim - 1) ->
                let x = startingPoint |> Array.copy
                x[d] <- startingPoint[d] + 1.0
                x
            for d in 0 .. (dim - 1) ->
                let x = startingPoint |> Array.copy
                x[d] <- startingPoint[d] - 1.0
                x
        |]

    let solve (tolerance: float) (dim: int, f: float [] -> float) (start: float []) =
        if start.Length <> dim
        then failwith $"Invalid starting point dimension: {start.Length}, expected {dim}."
        let simplex = initialize (dim, f) start
        simplex
        |> Seq.unfold (fun simplex ->
            let updatedSimplex = update (dim, f) simplex
            let solution =
                updatedSimplex
                |> Array.map (fun pt -> pt, f pt)
                |> Array.minBy snd
            Some ((solution, updatedSimplex), updatedSimplex)
            )
        |> Seq.skipWhile (fun (solution, simplex) ->
            simplex |> terminate tolerance f |> not
            )
        |> Seq.head
        |> fst
