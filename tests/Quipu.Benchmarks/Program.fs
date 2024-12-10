namespace Quipu.Benchmarks

module App =

    open BenchmarkDotNet.Running

    [<EntryPoint>]
    let main (args: string []) =

        BenchmarkRunner.Run<Benchmarks>()
        |> ignore

        0