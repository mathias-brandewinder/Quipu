namespace Quipu.Tests

module NelderMead =

    open Expecto
    open Quipu

    let tolerance = 0.01
    let config =
        { Configuration.defaultValue with
            Termination = Termination.tolerance tolerance
        }

    module BasicConvergence =

        // We test variations of minimize f(x) = x ^ 2,
        // which has a minimum value of 0.0 for x = 0.0.
        let expected = 0.0

        [<Tests>]
        let tests =
            testList "basic convergence tests" [

                test "constant function, 1 argument" {

                    let f (x: float) = 0.0

                    let solverResult =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around 100.0)
                        |> NelderMead.solve

                    let solution =
                        match solverResult with
                        | Successful solution -> solution
                        | _ -> failwith "unexpected"

                    Expect.equal solution.Status Status.Optimal "optimal solution"
                    let actual = solution.Candidate.Value
                    Expect.isTrue (0.0 - tolerance <= actual && actual <= 0.0 + tolerance) "minimum should be near 0.0"
                    }
        ]

    module Maximimization =

        [<Tests>]
        let tests =
            testList "basic maximization tests" [

                test "function, 1 argument" {

                    let f x = - pown (x - 1.0) 2 + 10.0

                    let solverResult =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around 100.0)
                        |> NelderMead.maximize

                    let solution =
                        match solverResult with
                        | Successful solution -> solution
                        | _ -> failwith "unexpected"

                    Expect.equal solution.Status Status.Optimal "optimal solution"

                    let value = solution.Candidate.Value
                    let args = solution.Candidate.Arguments

                    Expect.isTrue (10.0 - tolerance <= value && value <= 10.0 + tolerance) "maximum should be near 10.0"
                    Expect.isTrue (1.0 - tolerance <= args[0] && args[0] <= 1.0 + tolerance) "maximum should be near 10.0"
                    }
            ]

    module Fluent =

        [<Tests>]
        let tests =
            testList "fluent interface" [

                test "function, 1 argument" {

                    let f x = - pown (x - 1.0) 2 + 10.0
                    let solverResult =
                        NelderMead
                            .Objective(f)
                            .WithMaximumIterations(100)
                            .WithTolerance(0.001)
                            .StartFrom(Start.around 100.0)
                            .Maximize()

                    let solution =
                        match solverResult with
                        | Successful solution -> solution
                        | _ -> failwith "unexpected"

                    Expect.equal solution.Status Status.Optimal "optimal solution"

                    let value = solution.Candidate.Value
                    let args = solution.Candidate.Arguments

                    Expect.isTrue (10.0 - tolerance <= value && value <= 10.0 + tolerance) "maximum should be near 10.0"
                    Expect.isTrue (1.0 - tolerance <= args[0] && args[0] <= 1.0 + tolerance) "maximum should be near 10.0"
                    }
            ]

    module PartiallyDefinedFunctions =

        [<Tests>]
        let tests =
            testList "partially defined functions tests" [

                test "function defined over positive numbers only" {

                    let f x = sqrt x

                    let solverResult =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around 10.0)
                        |> NelderMead.solve

                    let solution =
                        match solverResult with
                        | Successful solution -> solution
                        | _ -> failwith "unexpected"

                    Expect.equal solution.Status Status.Optimal "optimal solution"
                    let actual = solution.Candidate.Value
                    Expect.isTrue (0.0 - tolerance <= actual && actual <= 0.0 + tolerance) "minimum should be near 0.0"
                    }
                ]


    module SubOptimalTermination =

        let config =
            { config with
                MaximumIterations = Some 1
            }

        [<Tests>]
        let tests =
            testList "sub optimal termination tests" [

                test "function, 1 argument" {

                    let f x = pown x 2

                    let solverResult =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around 100.0)
                        |> NelderMead.solve

                    let solution =
                        match solverResult with
                        | Successful solution -> solution
                        | _ -> failwith "unexpected"

                    Expect.equal solution.Status Status.Suboptimal ""
                    }
                ]

    module AbnormalTermination =

        [<Tests>]
        let tests =
            testList "abnormal termination tests" [

                test "unbounded function" {

                    let f x = x
                    let solverResult =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around 0.0)
                        |> NelderMead.solve

                    let solution =
                        match solverResult with
                        | Successful solution -> solution
                        | _ -> failwith "unexpected"

                    Expect.equal solution.Status Status.Unbounded "Solution should be unbounded"
                    }

                test "function returning nan" {

                    let f (x: float) = nan

                    let solverResult =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around 0.0)
                        |> NelderMead.solve

                    let isAbnormal =
                        match solverResult with
                        | Abnormal _ -> true
                        | _ -> false

                    Expect.isTrue(isAbnormal) ""
                    }

                test "simplex containing +infinity" {

                    let f (x: float) = 0.0

                    let solverResult =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around (+infinity))
                        |> NelderMead.solve

                    let isAbnormal =
                        match solverResult with
                        | Abnormal _ -> true
                        | _ -> false

                    Expect.isTrue(isAbnormal) ""
                    }

                test "simplex containing infinity" {

                    let f (x: float) = 0.0

                    let solverResult =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around (infinity))
                        |> NelderMead.solve

                    let isAbnormal =
                        match solverResult with
                        | Abnormal _ -> true
                        | _ -> false

                    Expect.isTrue(isAbnormal) ""
                    }

                test "simplex containing -infinity" {

                    let f (x: float) = 0.0

                    let solverResult =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around (-infinity))
                        |> NelderMead.solve

                    let isAbnormal =
                        match solverResult with
                        | Abnormal _ -> true
                        | _ -> false

                    Expect.isTrue(isAbnormal) ""
                    }

                test "simplex containing nan" {

                    let f (x: float) = 0.0

                    let solverResult =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around nan)
                        |> NelderMead.solve

                    let isAbnormal =
                        match solverResult with
                        | Abnormal _ -> true
                        | _ -> false

                    Expect.isTrue(isAbnormal) ""
                    }

                test "function throwing" {

                    let f (x: float) : float = failwith "some exception"
                    let solverResult =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around 100.0)
                        |> NelderMead.solve

                    let isAbnormal =
                        match solverResult with
                        | Abnormal _ -> true
                        | _ -> false

                    Expect.isTrue(isAbnormal) ""
                    }
            ]
