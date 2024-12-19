namespace Quipu

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

/// Value of a function, evaluated for an array of Arguments.
type Evaluation = {
    Arguments: float []
    Value: float
    }
    with
    member this.IsInfeasible =
        System.Double.IsNaN (this.Value)
    member this.IsFeasible =
        not (this.IsInfeasible)

type ITerminator =
    abstract member HasTerminated: Evaluation [] -> bool

module Termination =

    let private minMax f xs =
        let projection = xs |> Seq.map f
        let minimum = projection |> Seq.min
        let maximum = projection |> Seq.max
        (minimum, maximum)

    let tolerance (tolerance: float) =
        { new ITerminator with
            member this.HasTerminated(candidates: Evaluation array): bool =
                // The function value must be within the tolerance bounds
                // for every candidate in the simplex.
                let min, max =
                    candidates
                    |> minMax (fun x -> x.Value)
                max - min < tolerance
                &&
                // Every argument must be within the tolerance bounds
                // for every candidate in the simplex.
                let dim = candidates[0].Arguments.Length
                seq { 0 .. dim - 1 }
                |> Seq.forall (fun i ->
                    let min, max =
                        candidates
                        |> minMax (fun point -> point.Arguments.[i])
                    max - min < tolerance
                    )
        }

type Configuration = {
    Updates: Updates.Configuration
    Termination: ITerminator
    MaximumIterations: Option<int>
    }
    with
    static member defaultValue = {
        Updates = Updates.Configuration.defaultValue
        Termination = Termination.tolerance 0.001
        MaximumIterations = None
        }