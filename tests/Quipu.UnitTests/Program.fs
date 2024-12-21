namespace Quipu.Tests

module Program =

    open Expecto

    [<EntryPoint>]
    let  main args =
        testList "" [
            SampleUsage.tests
            HappyPath.BasicConvergence.tests
            NelderMead.EdgeCaseFunctions.tests
            NelderMead.AbnormalTermination.tests
            NelderMead.SubOptimalTermination.tests
            Termination.tests
            Simplex.tests
            ]
        |> runTestsWithCLIArgs [] args
