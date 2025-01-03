﻿namespace Quipu

type NelderMead private (problem: Problem) =

    // -------------------------------------------------------------------------
    // F# pipe-forward interface
    // -------------------------------------------------------------------------

    static member minimize (problem: Problem) =
        let simplex = problem.StartingPoint.create(problem.Dimension)
        Algorithm.search problem.Objective simplex problem.Configuration

    static member maximize (problem: Problem) =
        let problem = {
            problem with
                Objective =
                    problem.Objective
                    |> Vectorize.negate
            }
        problem
        |> NelderMead.minimize
        |> function
            | Successful solution ->
                { solution with
                    Candidate = {
                        solution.Candidate with
                            Value = - solution.Candidate.Value
                        }
                }
                |> Successful
            | Abnormal simplex -> Abnormal simplex

    static member solve (problem: Problem) =
        problem
        |> NelderMead.minimize

    static member objective (f: IVectorFunction) =
        f
        |> Problem.defaultCreate

    static member objective (f: float -> float) =
        Vectorize.from f
        |> Problem.defaultCreate

    static member objective (f: (float * float) -> float) =
        Vectorize.from f
        |> Problem.defaultCreate

    static member objective (f: (float * float * float) -> float) =
        Vectorize.from f
        |> Problem.defaultCreate

    static member objective (f: float -> float -> float) =
        Vectorize.from f
        |> Problem.defaultCreate

    static member objective (f: float -> float -> float -> float) =
        Vectorize.from f
        |> Problem.defaultCreate

    static member objective (dim: int, f: float[] -> float) =
        Vectorize.from (dim, f)
        |> Problem.defaultCreate

    static member withConfiguration (config: Configuration) (problem: Problem) =
        { problem with Configuration = config }

    static member startFrom (start: IStartingPoint) (problem: Problem) =
        { problem with StartingPoint = start }

    static member withMaximumIterations (iterations: int) (problem: Problem) =
        { problem with
            Configuration = {
                problem.Configuration with
                    MaximumIterations = Some iterations
                }
        }

    static member withTolerance (tolerance: float) (problem: Problem) =
        { problem with
            Configuration = {
                problem.Configuration with
                    Termination = Termination.tolerance tolerance
                }
        }