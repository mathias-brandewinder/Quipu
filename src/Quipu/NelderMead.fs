namespace Quipu

module NelderMead =

    type Configuration = {
        /// Reflection parameter, Alpha > 0.0
        Alpha: float
        /// Expansion parameter, Gamma > 1.0
        Gamma: float
        /// Contraction parameter, 0.0 < Rho <= 1.0
        Rho: float
        /// Shrink parameter, 0.0 < Sigma < 1.0
        Sigma: float
        }
        with
        static member defaultValue = {
            Alpha = 1.0
            Gamma = 2.0
            Rho = 0.5
            Sigma = 0.5
            }

    type IObjective =
        abstract member Dimension: int
        abstract member Value: float [] -> float

    let update (config: Configuration) (objective: IObjective) (simplex: (float []) []) =

        let dim = objective.Dimension
        let f = objective.Value

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

        let reflected =
            Array.init dim (fun col ->
                centroid[col] + config.Alpha * (centroid[col] - worst[col])
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
            let expanded =
                Array.init dim (fun col ->
                    centroid[col] + config.Gamma * (reflected[col] - centroid[col])
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
            let contractedOutside =
                Array.init dim (fun col ->
                    centroid[col] + config.Rho * (reflected[col] - centroid[col])
                    )
            if f contractedOutside < f reflected
            then
                ordered[size - 1] <- contractedOutside
                ordered
            else
            // 6) shrink
                let shrunk =
                    ordered
                    |> Array.map (fun pt ->
                        Array.init dim (fun col ->
                            best[col] + config.Sigma * (pt[col] - best[col])
                            )
                        )
                shrunk
        elif f reflected >= f worst
        then
            let contractedInside =
                Array.init dim (fun col ->
                    centroid[col] + config.Rho * (worst[col] - centroid[col])
                    )
            if f contractedInside < f worst
            then
                ordered[size - 1] <- contractedInside
                ordered
            else
            // 6) shrink
                let shrunk =
                    ordered
                    |> Array.map (fun pt ->
                        Array.init dim (fun col ->
                            best[col] + config.Sigma * (pt[col] - best[col])
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

    let initialize (objective: IObjective) (startingPoint: float []) =
        let dim = objective.Dimension
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

    let solve
        (config: Configuration)
        (tolerance: float)
        (objective: IObjective)
        (start: float []) =

        let dim = objective.Dimension
        let f = objective.Value

        if start.Length <> dim
        then failwith $"Invalid starting point dimension: {start.Length}, expected {dim}."
        let simplex = initialize objective start
        simplex
        |> Seq.unfold (fun simplex ->
            let updatedSimplex = update config objective simplex
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
