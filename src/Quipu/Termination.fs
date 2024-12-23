namespace Quipu

/// Termination rule: given the value of an array of Evaluation,
/// should the algorithm terminate?
type ITerminator =
    abstract member HasTerminated: Evaluation [] -> bool

module Termination =

    /// Simple tolerance termination: the algorithm should terminate if
    /// all values are within tolerance, and all arguments are within tolerance.
    let tolerance (tolerance: float) =
        { new ITerminator with
            member this.HasTerminated(candidates: Evaluation array): bool =
                // The function value must be within the tolerance bounds
                // for every candidate in the simplex.
                let mutable vMin = candidates.[0].Value
                let mutable vMax = candidates.[0].Value
                for i in 1 .. candidates.Length - 1 do
                    let candidate = candidates.[i]
                    if candidate.Value < vMin
                    then vMin <- candidate.Value
                    if candidate.Value > vMax
                    then vMax <- candidate.Value
                vMax - vMin < tolerance
                &&
                // Every argument must be within the tolerance bounds
                // for every candidate in the simplex.
                let dim = candidates[0].Arguments.Length
                let mins = Array.create dim (System.Double.PositiveInfinity)
                let maxs = Array.create dim (System.Double.NegativeInfinity)
                for candidate in candidates do
                    let args = candidate.Arguments
                    for i in 0 .. (dim - 1) do
                        if args.[i] < mins.[i]
                        then mins.[i] <- args.[i]
                        if args.[i] > maxs.[i]
                        then maxs.[i] <- args.[i]
                let deltas = (mins, maxs) ||> Array.map2 (fun min max -> max - min)
                deltas |> Array.forall (fun delta -> delta < tolerance)
        }