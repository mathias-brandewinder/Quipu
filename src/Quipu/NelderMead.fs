namespace Quipu

module NelderMead =

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

    type IObjective =
        abstract member Dimension: int
        abstract member Value: float [] -> float

    type Objective () =
        static member from (f: float -> float) =
            { new IObjective with
                member this.Dimension = 1
                member this.Value x = f(x.[0])
            }
        static member from (f: (float * float) -> float) =
            { new IObjective with
                member this.Dimension = 2
                member this.Value x = f (x.[0], x.[1])
            }
        static member from (f: (float * float * float) -> float) =
            { new IObjective with
                member this.Dimension = 3
                member this.Value x = f (x.[0], x.[1], x.[2])
            }
        static member from (dim: int, f: float[] -> float) =
            { new IObjective with
                member this.Dimension = dim
                member this.Value x = f x
            }

    exception Unbounded
    exception Abnormal of float [][]

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
        let best = f ordered.[0]
        if best = -infinity then raise Unbounded

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
            raise (Abnormal simplex)

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

    type Solution =
        | Optimal of (float * float [])
        | SubOptimal of (float * float [])
        | Unbounded
        | Abnormal of (float [][])

    let solve
        (config: Configuration)
        (objective: IObjective)
        (start: seq<float>) =

        let start = start |> Array.ofSeq
        let dim = objective.Dimension
        let f = objective.Value

        if start.Length <> dim
        then failwith $"Invalid starting point dimension: {start.Length}, expected {dim}."
        let simplex = initialize objective start
        try
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
        | :? Unbounded -> Solution.Unbounded
        | :? Abnormal -> Solution.Abnormal simplex
        | _ -> Solution.Abnormal simplex