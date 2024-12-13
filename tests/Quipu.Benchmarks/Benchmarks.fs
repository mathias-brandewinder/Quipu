namespace Quipu.Benchmarks

open BenchmarkDotNet.Attributes
open Quipu
open Quipu.Tests

type Benchmarks () =

    // High tolerance to force the solver to work hard
    let tolerance = 0.000_000_001

    let solverConfiguration =
        { Configuration.defaultValue with
            Termination = {
                MaximumIterations = None
                Tolerance = tolerance
                }
        }

    [<Benchmark(Description="Beale function")>]
    member this.BealeFunction () =

        TestFunctions.beale
        |> NelderMead.objective
        |> NelderMead.withConfiguration solverConfiguration
        |> NelderMead.startFrom (StartingPoint.fromValue [ 4.5; 4.5 ])
        |> NelderMead.solve

    [<Benchmark(Description="Booth function")>]
    member this.BoothFunction () =

        TestFunctions.booth
        |> NelderMead.objective
        |> NelderMead.withConfiguration solverConfiguration
        |> NelderMead.startFrom (StartingPoint.fromValue [ 10.0; 10.0 ])
        |> NelderMead.solve
