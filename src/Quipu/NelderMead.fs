namespace Quipu.NelderMead

open Quipu

module Updates =

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

module Termination =

    type Configuration = {
        Tolerance: float
        MaximumIterations: Option<int>
        }
        with
        static member defaultValue = {
            Tolerance = 0.001
            MaximumIterations = None
            }

type Configuration = {
    Updates: Updates.Configuration
    Termination: Termination.Configuration
    }
    with
    static member defaultValue = {
        Updates = Updates.Configuration.defaultValue
        Termination = Termination.Configuration.defaultValue
        }

exception UnboundedObjective
exception AbnormalConditions of float [][]

type Solution =
    | Optimal of (float * float [])
    | SubOptimal of (float * float [])
    | Unbounded
    | Abnormal of (float [][])

module Algorithm =

    let isNaN x = System.Double.IsNaN x
    let isReal x = not (System.Double.IsNaN x)
    let isFinite x = System.Double.IsFinite x

    let update
        (config: Updates.Configuration)
        (objective: IObjective)
        (simplex: (float []) []) =

        let dim = objective.Dimension
        let f = objective.Value

        // 1) order the values, from best to worst
        let ordered =
            simplex
            |> Array.sortBy f

        // if the lowest value is -infinity, there is no solution
        let best = ordered.[0]
        let bestValue = f best

        if bestValue = -infinity then raise UnboundedObjective

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
                            best[col] + config.Sigma * (pt[col] - best[col])
                            )
                    // enforce that shrinking produces a valid simplex
                    if isReal (f shrunk)
                    then shrunk
                    else raise (AbnormalConditions simplex)
                )

        // 3) reflection
        let worst = ordered[size - 1]
        let worstValue = f worst

        let reflected =
            Array.init dim (fun col ->
                centroid[col] + config.Alpha * (centroid[col] - worst[col])
                )

        let secondWorst = ordered[size - 2]

        let reflectedValue = f reflected

        if isNaN reflectedValue
        then shrink ()

        elif
            reflectedValue < f secondWorst
            &&
            reflectedValue >= bestValue
        then
            // replace worst by reflected
            ordered[size - 1] <- reflected
            ordered

        // 4) expansion
        elif
            reflectedValue < bestValue
        then
            let expanded =
                Array.init dim (fun col ->
                    centroid[col] + config.Gamma * (reflected[col] - centroid[col])
                    )
            let expandedValue = f expanded
            if
                isReal expandedValue
                &&
                expandedValue < reflectedValue
            then
                ordered[size - 1] <- expanded
            else
                ordered[size - 1] <- reflected
            ordered

        // 5) contraction
        elif reflectedValue < worstValue
        then
            let contractedOutside =
                Array.init dim (fun col ->
                    centroid[col] + config.Rho * (reflected[col] - centroid[col])
                    )
            if
                isReal (f contractedOutside)
                &&
                f contractedOutside < reflectedValue
            then
                ordered[size - 1] <- contractedOutside
                ordered
            else
                // 6) shrink
                shrink ()

        elif reflectedValue >= worstValue
        then
            let contractedInside =
                Array.init dim (fun col ->
                    centroid[col] + config.Rho * (worst[col] - centroid[col])
                    )
            if
                isReal (f contractedInside)
                &&
                f contractedInside < worstValue
            then
                ordered[size - 1] <- contractedInside
                ordered
            else
                // 6) shrink
                shrink ()
        else
            // TODO, check: is this branch even possible?
            raise (AbnormalConditions simplex)

    let private minMax f xs =
        let projection = xs |> Seq.map f
        let minimum = projection |> Seq.min
        let maximum = projection |> Seq.max
        (minimum, maximum)

    let terminate (tolerance: float) (f: float [] -> float) (simplex: float [][]) =
        // The function value must be within the tolerance bounds
        // for every candidate in the simplex.
        let min, max = simplex |> minMax f
        max - min < tolerance
        &&
        // Every argument must be within the tolerance bounds
        // for every candidate in the simplex.
        let dim = simplex.[0].Length
        seq { 0 .. dim - 1 }
        |> Seq.forall (fun i ->
            let min, max =
                simplex
                |> minMax (fun point -> point.[i])
            max - min < tolerance
            )

    // Verify that the initial simplex is well-formed
    let preCheck (objective: IObjective) simplex =
        let f = objective.Value
        simplex
        |> Array.iter (fun pt ->
            // check that the vector pt does not contain any NaNs
            pt
            |> Array.iter (fun x ->
                if not (isFinite x)
                then raise (AbnormalConditions simplex)
                )
            // check the evaluation of the vector pt for early termination
            let evaluation = f pt
            if evaluation = -infinity
            then raise UnboundedObjective
            if isNaN evaluation
            then raise (AbnormalConditions simplex)
            )

    let search (objective: IObjective) simplex config =
        let f = objective.Value
        try
            // Is the starting simplex well-formed?
            preCheck objective simplex
            // Start the search
            simplex
            |> Seq.unfold (fun simplex ->
                let updatedSimplex = update config.Updates objective simplex
                let solution =
                    updatedSimplex
                    |> Array.map (fun pt -> pt, f pt)
                    |> Array.minBy snd
                Some ((solution, updatedSimplex), updatedSimplex)
                )
            |> Seq.mapi (fun i x -> i, x)
            |> Seq.skipWhile (fun (iter, (solution, simplex)) ->
                simplex |> terminate config.Termination.Tolerance f |> not
                &&
                config.Termination.MaximumIterations
                |> Option.map (fun maxIter -> iter < maxIter)
                |> Option.defaultValue true
                )
            |> Seq.head
            |> fun (iter, ((args, value), _)) ->
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
            StartingPoint = StartingPoint.zero
            Configuration = Configuration.defaultValue
        }

type NelderMead =

    static member solve (problem: Problem) =
        let simplex = problem.StartingPoint.create(problem.Dimension)
        Algorithm.search problem.Objective simplex problem.Configuration

    static member minimize (f: IObjective) =
        f
        |> Problem.defaultCreate

    static member minimize (f: float -> float) =
        Objective.from f
        |> Problem.defaultCreate

    static member minimize (f: (float * float) -> float) =
        Objective.from f
        |> Problem.defaultCreate

    static member minimize (f: (float * float * float) -> float) =
        Objective.from f
        |> Problem.defaultCreate

    static member minimize (dim: int, f: float[] -> float) =
        Objective.from (dim, f)
        |> Problem.defaultCreate

    static member withConfiguration (config: Configuration) (problem: Problem) =
        { problem with Configuration = config }

    static member startFrom (start: IStartingPoint) (problem: Problem) =
        { problem with StartingPoint = start }