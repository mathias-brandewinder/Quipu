namespace Quipu

/// Implementation of the Nelder-Mead method.
/// Inspired by https://en.wikipedia.org/wiki/Nelder%E2%80%93Mead_method
module Algorithm =

    exception private UnboundedObjective of Evaluation
    exception private AbnormalConditions of float [][]

    let private evaluate f (x: float []) =
        let value = f x
        // if the lowest value is -infinity, there is no solution:
        // the problem / objective is unbounded.
        if value = System.Double.NegativeInfinity
        then raise (UnboundedObjective { Value = System.Double.NegativeInfinity; Arguments = x })
        else { Arguments = x; Value = f x }

    let private update
        (config: Updates.Configuration)
        (objective: IVectorFunction)
        (candidates: Evaluation []) =

        let dim = objective.Dimension
        let f = objective.Value
        let eval = evaluate f

        // 1) order the values, from best to worst
        let ordered =
            candidates
            |> Array.sortBy (fun c -> c.Value)

        let best = candidates[0]

        // 2) calculate centroid
        let size = candidates.Length
        // drop the worst candidate
        let bestCandidates = ordered[.. size - 2]
        // calculate average point (centroid)
        let centroid =
            Array.init dim (fun col ->
                bestCandidates
                |> Array.averageBy(fun pt -> pt.Arguments[col])
                )

        let shrink () =
            ordered
            |> Array.mapi (fun i pt ->
                // keep the best point, at index 0, intact
                if i = 0
                then pt
                else
                    // shrink towards the best
                    let best = ordered.[0]
                    let shrunk =
                        Array.init dim (fun col ->
                            best.Arguments[col] + config.Sigma * (pt.Arguments[col] - best.Arguments[col])
                            )
                        |> eval
                    // enforce that shrinking produces a valid simplex
                    if shrunk.IsFeasible
                    then shrunk
                    else raise (AbnormalConditions (candidates |> Array.map (fun c -> c.Arguments)))
                )

        // 3) reflection
        let worst = ordered[size - 1]

        let reflected =
            Array.init dim (fun col ->
                centroid[col] + config.Alpha * (centroid[col] - worst.Arguments[col])
                )
            |> eval

        let secondWorst = ordered[size - 2]

        if reflected.IsInfeasible
        then shrink ()

        elif
            reflected.Value < secondWorst.Value
            &&
            reflected.Value >= best.Value
        then
            // replace worst by reflected
            ordered[size - 1] <- reflected
            ordered

        // 4) expansion
        elif
            reflected.Value < best.Value
        then
            let expanded =
                Array.init dim (fun col ->
                    centroid[col] + config.Gamma * (reflected.Arguments[col] - centroid[col])
                    )
                |> eval
            if
                expanded.IsFeasible
                &&
                expanded.Value < reflected.Value
            then
                ordered[size - 1] <- expanded
            else
                ordered[size - 1] <- reflected
            ordered

        // 5) contraction
        elif reflected.Value < worst.Value
        then
            let contractedOutside =
                Array.init dim (fun col ->
                    centroid[col] + config.Rho * (reflected.Arguments[col] - centroid[col])
                    )
                |> eval
            if
                contractedOutside.IsFeasible
                &&
                contractedOutside.Value < reflected.Value
            then
                ordered[size - 1] <- contractedOutside
                ordered
            else
                // 6) shrink
                shrink ()

        elif reflected.Value >= worst.Value
        then
            let contractedInside =
                Array.init dim (fun col ->
                    centroid[col] + config.Rho * (worst.Arguments[col] - centroid[col])
                    )
                |> eval
            if
                contractedInside.IsFeasible
                &&
                contractedInside.Value < worst.Value
            then
                ordered[size - 1] <- contractedInside
                ordered
            else
                // 6) shrink
                shrink ()
        else
            // TODO, check: is this branch even possible?
            raise (AbnormalConditions (candidates |> Array.map (fun c -> c.Arguments)))

    // Verify that the initial simplex is well-formed
    let preCheck (objective: IVectorFunction) simplex: Evaluation [] =
        let f = objective.Value
        simplex
        |> Array.map (fun pt ->
            // check that the vector pt does not contain any NaNs
            pt
            |> Array.iter (fun x ->
                if not (System.Double.IsFinite x)
                then raise (AbnormalConditions simplex)
                )
            let candidate = evaluate f pt
            if candidate.IsInfeasible
            then raise (AbnormalConditions simplex)
            candidate
            )

    let search (objective: IVectorFunction) simplex config =
        let terminator = config.Termination
        try
            // Is the starting simplex well-formed?
            preCheck objective simplex
            // Start the search
            |> Seq.unfold (fun simplex ->
                let updatedSimplex = update config.Updates objective simplex
                Some (updatedSimplex, updatedSimplex)
                )
            |> Seq.mapi (fun i x -> i, x)
            |> Seq.skipWhile (fun (iter, simplex) ->
                simplex |> terminator.HasTerminated |> not
                &&
                config.MaximumIterations
                |> Option.map (fun maxIter -> iter < maxIter)
                |> Option.defaultValue true
                )
            |> Seq.head
            |> fun (iter, simplex) ->
                let bestSolution =
                    simplex
                    |> Array.minBy (fun x -> x.Value)
                let args = bestSolution.Arguments
                let value = bestSolution.Value
                match config.MaximumIterations with
                | None ->
                    {
                        Status = Optimal
                        Candidate = { Value = value; Arguments = args }
                        Simplex = simplex |> Array.map (fun x -> x.Arguments)
                    }
                | Some maxIters ->
                    if iter < maxIters
                    then
                        {
                            Status = Optimal
                            Candidate = { Value = value; Arguments = args }
                            Simplex = simplex |> Array.map (fun x -> x.Arguments)
                        }
                    else
                        {
                            Status = Suboptimal
                            Candidate = { Value = value; Arguments = args }
                            Simplex = simplex |> Array.map (fun x -> x.Arguments)
                        }
                |> Successful
        with
        | :? UnboundedObjective as ex ->
            {
                Status = Unbounded
                Candidate = ex.Data0
                Simplex = simplex
            }
            |> Successful
        | :? AbnormalConditions ->
            Abnormal simplex
        | _ ->Abnormal simplex