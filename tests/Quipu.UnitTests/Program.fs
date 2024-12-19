namespace Quipu.Tests

module Program =

    open Expecto

    [<EntryPoint>]
    let  main args =
        testList "" [
            NelderMead.BasicConvergence.tests
            NelderMead.PartiallyDefinedFunctions.tests
            NelderMead.AbnormalTermination.tests
            NelderMead.SubOptimalTermination.tests
            Termination.tests
            Simplex.tests
            HappyPath.BasicConvergence.tests
            ]
        |> runTestsWithCLIArgs [] args
