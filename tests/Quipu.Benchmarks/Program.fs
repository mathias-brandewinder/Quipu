namespace Quipu.Benchmarks

module App =

    open BenchmarkDotNet.Running
    open BenchmarkDotNet.Configs
    open BenchmarkDotNet.Jobs

    [<EntryPoint>]
    let main (args: string []) =

        let config =
            DefaultConfig.Instance
                .AddJob(
                    Job.Dry
                        .WithInvocationCount(10_000)
                        .WithIterationCount(100)
                        )

        BenchmarkRunner.Run<Benchmarks>(config)
        |> ignore

        0