namespace Quipu.CSharp

open Quipu

/// <summary>
/// Fluent interface to set up and solve function minimization problems,
/// using the Nelder-Mead algorithm.
/// <code lang="csharp">
/// var f = x => Math.Pow(x,2);
/// var result =
///   NelderMead
///     .Objective(f)
///     .Minimize();
/// </code>
/// </summary>
type NelderMead private (problem: Problem) =

    // -------------------------------------------------------------------------
    // C# Fluent Interface
    // -------------------------------------------------------------------------

    static member Objective(f: IVectorFunction) =
        NelderMead.objective f
        |> NelderMead

    static member Objective(f: System.Func<float,float>) =
        NelderMead.objective f.Invoke
        |> NelderMead

    static member Objective(f: System.Func<float,float,float>) =
        NelderMead.objective f.Invoke
        |> NelderMead

    static member Objective(f: System.Func<float,float,float,float>) =
        NelderMead.objective f.Invoke
        |> NelderMead

    member this.WithTolerance(tolerance: float) =
        problem
        |> NelderMead.withTolerance tolerance
        |> NelderMead

    member this.WithMaximumIterations(iterations: int) =
        problem
        |> NelderMead.withMaximumIterations iterations
        |> NelderMead

    /// <summary>
    /// Specify the starting point for the search. The Start class exposes
    /// multiple helper functions for that purpose, for example:
    /// <code lang="csharp">
    /// Start.Around(1.0)
    /// Start.Around(1.0, 1.0)
    /// </code>
    /// </summary>
    member this.StartFrom(startingPoint: IStartingPoint) =
        problem
        |> NelderMead.startFrom startingPoint
        |> NelderMead

    /// <summary>
    /// Start the search for arguments that minimize the objective
    /// function defined in the Problem.
    /// </summary>
    member this.Solve() =
        problem
        |> NelderMead.solve

    /// <summary>
    /// Start the search for arguments that maximize the objective
    /// function defined in the Problem.
    /// </summary>
    member this.Maximize() =
        problem
        |> NelderMead.maximize

    /// <summary>
    /// Start the search for arguments that minimize the objective
    /// function defined in the Problem.
    /// </summary>
    member this.Minimize() =
        problem
        |> NelderMead.minimize

    /// <summary>
    /// Start the search for arguments that make the objective
    /// function defined in the Problem equal to the target value.
    /// </summary>
    member this.GoalSeek(target: float) =
        problem
        |> NelderMead.goalSeek target