namespace Quipu.Benchmarks

open BenchmarkDotNet.Attributes
open Quipu
open Quipu.Tests.Functions

type Benchmarks () =

    // High tolerance to force the solver to work hard
    let tolerance = 0.000_000_001

    let solverConfiguration =
        { Configuration.defaultValue with
            MaximumIterations = None
            Termination = Termination.tolerance tolerance
        }

    [<Benchmark(Description="Beale function")>]
    member this.BealeFunction () =

        beale
        |> NelderMead.objective
        |> NelderMead.withConfiguration solverConfiguration
        |> NelderMead.startFrom (Start.around [ 4.5; 4.5 ])
        |> NelderMead.solve

    [<Benchmark(Description="Beale function, safe")>]
    member this.BealeFunction_safe () =

        beale
        |> NelderMead.objective
        |> NelderMead.safe
        |> NelderMead.withConfiguration solverConfiguration
        |> NelderMead.startFrom (Start.around [ 4.5; 4.5 ])
        |> NelderMead.solve

    [<Benchmark(Description="Booth function")>]
    member this.BoothFunction () =

        booth
        |> NelderMead.objective
        |> NelderMead.withConfiguration solverConfiguration
        |> NelderMead.startFrom (Start.around [ 10.0; 10.0 ])
        |> NelderMead.solve

    [<Benchmark(Description="Booth function, safe")>]
    member this.BoothFunction_safe () =

        booth
        |> NelderMead.objective
        |> NelderMead.safe
        |> NelderMead.withConfiguration solverConfiguration
        |> NelderMead.startFrom (Start.around [ 10.0; 10.0 ])
        |> NelderMead.solve