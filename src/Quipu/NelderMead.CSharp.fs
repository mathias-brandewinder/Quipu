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

    member this.StartFrom(startingPoint: IStartingPoint) =
        problem
        |> NelderMead.startFrom startingPoint
        |> NelderMead

    member this.Solve() =
        problem
        |> NelderMead.solve

    member this.Maximize() =
        problem
        |> NelderMead.maximize

    member this.Minimize() =
        problem
        |> NelderMead.minimize
