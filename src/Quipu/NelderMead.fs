namespace Quipu

type Solution =
    | Optimal of (float * float [])
    | SubOptimal of (float * float [])
    | Unbounded
    | Abnormal of (float [][])

module Algorithm =

    exception private UnboundedObjective
    exception private AbnormalConditions of float [][]

    let private evaluate f (x: float []) =
        let value = f x
        // if the lowest value is -infinity, there is no solution:
        // the problem / objective is unbounded.
        if value = System.Double.NegativeInfinity
        then raise UnboundedObjective
        else { Point = x; Value = f x }

    let private update
        (config: Updates.Configuration)
        (objective: IObjective)
        (candidates: Candidate []) =

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
                |> Array.averageBy(fun pt -> pt.Point[col])
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
                            best.Point[col] + config.Sigma * (pt.Point[col] - best.Point[col])
                            )
                        |> eval
                    // enforce that shrinking produces a valid simplex
                    if shrunk.IsFeasible
                    then shrunk
                    else raise (AbnormalConditions (candidates |> Array.map (fun c -> c.Point)))
                )

        // 3) reflection
        let worst = ordered[size - 1]

        let reflected =
            Array.init dim (fun col ->
                centroid[col] + config.Alpha * (centroid[col] - worst.Point[col])
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
                    centroid[col] + config.Gamma * (reflected.Point[col] - centroid[col])
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
                    centroid[col] + config.Rho * (reflected.Point[col] - centroid[col])
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
                    centroid[col] + config.Rho * (worst.Point[col] - centroid[col])
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
            raise (AbnormalConditions (candidates |> Array.map (fun c -> c.Point)))

    // Verify that the initial simplex is well-formed
    let preCheck (objective: IObjective) simplex: Candidate [] =
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

    let search (objective: IObjective) simplex config =
        let terminator = config.Termination.Termination
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
                config.Termination.MaximumIterations
                |> Option.map (fun maxIter -> iter < maxIter)
                |> Option.defaultValue true
                )
            |> Seq.head
            |> fun (iter, simplex) ->
                let bestSolution =
                    simplex
                    |> Array.minBy (fun x -> x.Value)
                let args = bestSolution.Point
                let value = bestSolution.Value
                match config.Termination.MaximumIterations with
                | None -> Solution.Optimal (value, args)
                | Some maxIters ->
                    if iter < maxIters
                    then Solution.Optimal (value, args)
                    else Solution.SubOptimal (value, args)
        with
        | :? UnboundedObjective -> Solution.Unbounded
        | :? AbnormalConditions -> Solution.Abnormal simplex
        | _ -> Solution.Abnormal simplex

type Problem = {
    Objective: IObjective
    Configuration: Configuration
    StartingPoint: IStartingPoint
    }
    with
    member this.Dimension =
        this.Objective.Dimension
    static member defaultCreate(objective: IObjective) =
        {
            Objective = objective
            StartingPoint = Start.zero
            Configuration = Configuration.defaultValue
        }

type NelderMead =

    static member minimize (problem: Problem) =
        let simplex = problem.StartingPoint.create(problem.Dimension)
        Algorithm.search problem.Objective simplex problem.Configuration

    static member maximize (problem: Problem) =
        let problem = {
            problem with
                Objective =
                    problem.Objective
                    |> Objective.negate
            }
        problem
        |> NelderMead.minimize
        |> function
            | Optimal (value, solution) -> Optimal (- value, solution)
            | SubOptimal (value, solution) -> Optimal (- value, solution)
            | Unbounded -> Unbounded
            | Abnormal simplex -> Abnormal simplex

    static member solve (problem: Problem) =
        NelderMead.minimize problem

    static member objective (f: IObjective) =
        f
        |> Problem.defaultCreate

    static member objective (f: float -> float) =
        Objective.from f
        |> Problem.defaultCreate

    static member objective (f: (float * float) -> float) =
        Objective.from f
        |> Problem.defaultCreate

    static member objective (f: (float * float * float) -> float) =
        Objective.from f
        |> Problem.defaultCreate

    static member objective (dim: int, f: float[] -> float) =
        Objective.from (dim, f)
        |> Problem.defaultCreate

    static member withConfiguration (config: Configuration) (problem: Problem) =
        { problem with Configuration = config }

    static member startFrom (start: IStartingPoint) (problem: Problem) =
        { problem with StartingPoint = start }

    static member withMaximumIterations (iterations: int) (problem: Problem) =
        { problem with
            Configuration = {
                problem.Configuration with
                    Termination = {
                        problem.Configuration.Termination with
                            MaximumIterations = Some iterations
                        }
                }
        }

    static member withTolerance (tolerance: float) (problem: Problem) =
        { problem with
            Configuration = {
                problem.Configuration with
                    Termination = {
                        problem.Configuration.Termination with
                            Termination = Termination.tolerance tolerance
                        }
                }
        }