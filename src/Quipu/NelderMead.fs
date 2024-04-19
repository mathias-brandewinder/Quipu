namespace Quipu.NelderMead

type Simplex =
    private | Vectors of dim: int * vectors: float [][]
    with
    member this.dimension =
        match this with
        | Vectors (dim, _) -> dim
    member this.size =
        match this with
        | Vectors (_, vectors) -> vectors.Length
    static member vertices (this: Simplex)=
        match this with
        | Vectors (_, vectors) ->
            vectors
            |> Array.copy
    /// Create a Simplex centered on the origin point, with a set outer radius.
    static member create (origin: float[], radius: float) =
        // Source:
        // https://en.wikipedia.org/wiki/Simplex#Cartesian_coordinates_for_a_regular_n-dimensional_simplex_in_Rn
        let dim = origin.Length
        // Coordinates of the last, non-basis vertex
        let lastValue = (1.0 / (float dim)) * (1.0 + (sqrt (float (dim + 1))))
        // Barycenter of the original simplex
        let barycenter = (1.0 + lastValue) / (float (dim + 1))
        // Radius of the original simplex
        let initialRadius = sqrt (float dim / float (2 * (dim + 1))) * sqrt 2.0
        // Scaling factor to desired radius
        let scale = radius / initialRadius
        let vectors =
            Array.init (dim + 1) (
                fun i ->
                    if i < dim
                    then
                        // n first basis vectors
                        Array.init dim (fun j ->
                            if j = i
                            then 1.0
                            else 0.0
                            )
                    else
                        // last, non-basis vector
                        Array.init dim (fun _ -> lastValue)
                    |> Array.mapi (fun i x ->
                        // scale and translate every coordinate
                        scale * (x - barycenter) + origin.[i]
                        )
                    )
        Vectors (dim, vectors)
    /// Create a Simplex centered on the origin point, with a radius of 1.0
    static member create (origin: float[]) =
        Simplex.create (origin, 1.0)
    /// Create a Simplex centered on 0.0, with a radius of 1.0
    static member create (dim: int) =
        let origin = Array.init dim (fun _ -> 0.0)
        Simplex.create origin
    /// Create a Simplex centered on 0.0, with a set outer radius.
    static member create (dim: int, radius: float) =
        let origin = Array.init dim (fun _ -> 0.0)
        Simplex.create (origin, radius)

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
                if isNaN x
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

type IStartingPoint =
    abstract member create: int -> float[][]

type StartingPoint =

    // To be deprecated / replaced with a better function
    static member initialize (dim: int) (startingPoint: float []) =
        Simplex.create dim
        |> Simplex.vertices
        |> Array.map (fun vector ->
            (startingPoint, vector)
            ||> Array.map2 (fun x1 x2 -> x1 + x2)
            )

    static member zero =
        { new IStartingPoint with
            member this.create(dim: int): float array array =
                Simplex.create dim
                |> Simplex.vertices
        }

    static member fromValue (startingPoint: seq<float>) =
        { new IStartingPoint with
            member this.create (dim: int): float[][] =
                let startingPoint = startingPoint |> Array.ofSeq
                if startingPoint.Length <> dim
                then failwith $"Invalid starting point dimension: {startingPoint.Length}, expected {dim}."
                Simplex.create startingPoint
                |> Simplex.vertices
        }

    static member fromValue (startingPoint: seq<float>, radius: float) =
        { new IStartingPoint with
            member this.create (dim: int): float[][] =
                let startingPoint = startingPoint |> Array.ofSeq
                if startingPoint.Length <> dim
                then failwith $"Invalid starting point dimension: {startingPoint.Length}, expected {dim}."
                Simplex.create (startingPoint, radius)
                |> Simplex.vertices
        }

    static member fromValue (startingPoint: float) =
        { new IStartingPoint with
            member this.create (dim: int): float[][] =
                let startingPoint = Array.singleton startingPoint
                if startingPoint.Length <> dim
                then failwith $"Invalid starting point dimension: {startingPoint.Length}, expected {dim}."
                Simplex.create startingPoint
                |> Simplex.vertices
        }

    static member fromValue (startingPoint: #seq<#seq<float>>) =
        { new IStartingPoint with
            member this.create (dim: int): float[][] =
                let startingPoint =
                    startingPoint
                    |> Seq.map (Array.ofSeq)
                    |> Array.ofSeq
                    |> Array.distinct
                startingPoint
                |> Array.iter (fun point ->
                    if point.Length <> dim
                    then
                        failwith $"Invalid starting point dimension: {point.Length}, expected {dim}."
                    )
                if startingPoint.Length <= dim
                then failwith $"Invalid starting simplex size: {startingPoint.Length}, expected {dim + 1}."
                startingPoint
        }

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