#r "nuget: Quipu, 0.5.1"
open Quipu

let f (x, y) = pown (x - 1.0) 2 + pown (y - 2.0) 2 + 42.0

let solverResult =
    NelderMead.objective f
    |> NelderMead.minimize

if solverResult.HasSolution
then
    let solution = solverResult.Solution
    printfn $"Solution: {solution.Status}"
    let candidate = solution.Candidate
    let args = candidate.Arguments
    let value = candidate.Value
    printfn $"f(%.3f{args[0]}, %.3f{args[1]}) = %.3f{value}"
