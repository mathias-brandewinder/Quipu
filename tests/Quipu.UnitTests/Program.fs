namespace Quipu.Tests

module Program =

    open Expecto

    [<EntryPoint>]
    let  main args =
        testList "" [
            NelderMead.BasicConvergence.tests
            NelderMead.PartiallyDefinedFunctions.tests
            NelderMead.AbnormalTermination.tests // ISSUE
            NelderMead.SubOptimalTermination.tests
            NelderMead.TerminationCriteria.tests
            NelderMead.Simplex.tests
            ]
        |> runTestsWithCLIArgs [] args
