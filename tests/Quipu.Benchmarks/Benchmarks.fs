namespace Quipu.Benchmarks

open BenchmarkDotNet.Attributes
open Quipu.NelderMead

type Benchmarks () =

    // See https://en.wikipedia.org/wiki/Test_functions_for_optimization
    let beale (x, y) =
        pown (1.5 - x + (x * y)) 2
        +
        pown (2.25 - x + (x * pown y 2)) 2
        +
        pown (2.625 - x + x * pown y 3) 2

    [<Benchmark(Description="Beale function")>]
    member this.BealeFunction () =

        beale
        |> NelderMead.minimize
        |> NelderMead.withConfiguration
            { Configuration.defaultValue with
                Termination = {
                    Tolerance = 0.000_000_001
                    MaximumIterations = None
                    }
            }
        |> NelderMead.solve