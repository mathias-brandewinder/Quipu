namespace Quipu.Tests

module NelderMead =

    open Expecto
    open Quipu

    let tolerance = 0.01
    let config =
        { Configuration.defaultValue with
            Termination = {
                Configuration.defaultValue.Termination with
                    Termination = Termination.tolerance tolerance
                    MaximumIterations = None
                }
        }

    module BasicConvergence =

        // We test variations of minimize f(x) = x ^ 2,
        // which has a minimum value of 0.0 for x = 0.0.
        let expected = 0.0

        type TestClass() =
            member this.OneParameter(x: float) = pown x 2
            member this.TwoParameters(x: float, y: float) = pown x 2 + pown y 2
            member this.ThreeParameters(x: float, y: float, z: float) = pown x 2 + pown y 2 + pown z 2
            static member StaticOne(x: float) = pown x 2

        [<Tests>]
        let tests =
            testList "basic convergence tests" [

                test "function, 1 argument" {

                    let f x = pown x 2
                    let solution =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around 100.0)
                        |> NelderMead.solve
                    let actual =
                        match solution with
                        | Optimal x -> x.Value
                        | _ -> failwith "unexpected"

                    Expect.isTrue (0.0 - tolerance <= actual && actual <= 0.0 + tolerance) "minimum should be near 0.0"
                    }

                test "function, tuple" {

                    let f (x, y) = pown x 2 + pown y 2
                    let solution =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around [ 100.0; 100.0 ])
                        |> NelderMead.solve
                    let actual =
                        match solution with
                        | Optimal x -> x.Value
                        | _ -> failwith "unexpected"
                    Expect.isTrue (0.0 - tolerance <= actual && actual <= 0.0 + tolerance) "minimum should be near 0.0"
                    }

                test "function, truple" {

                    let f (x, y, z) = pown x 2 + pown y 2 + pown z 2
                    let solution =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around [ 100.0; 100.0; 100.0 ])
                        |> NelderMead.solve

                    let actual =
                        match solution with
                        | Optimal x -> x.Value
                        | _ -> failwith "unexpected"

                    Expect.isTrue (0.0 - tolerance <= actual && actual <= 0.0 + tolerance) "minimum should be near 0.0"
                    }

                test "function, array of arguments" {

                    let f (x: float []) = pown x.[0] 2 + pown x.[1] 2
                    let solution =
                        NelderMead.objective (2, f)
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around [ 100.0; 100.0 ])
                        |> NelderMead.solve

                    let actual =
                        match solution with
                        | Optimal x -> x.Value
                        | _ -> failwith "unexpected"

                    Expect.isTrue (0.0 - tolerance <= actual && actual <= 0.0 + tolerance) "minimum should be near 0.0"
                    }

                test "method, 1 argument" {

                    let testClass = TestClass()
                    let solution =
                        NelderMead.objective testClass.OneParameter
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around 100.0)
                        |> NelderMead.solve
                    let actual =
                        match solution with
                        | Optimal x -> x.Value
                        | _ -> failwith "unexpected"
                    Expect.isTrue (0.0 - tolerance <= actual && actual <= 0.0 + tolerance) "minimum should be near 0.0"
                    }


                test "method, 2 arguments" {

                    let testClass = TestClass()
                    let solution =
                        NelderMead.objective testClass.TwoParameters
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around [ 100.0; 100.0 ])
                        |> NelderMead.solve
                    let actual =
                        match solution with
                        | Optimal x -> x.Value
                        | _ -> failwith "unexpected"
                    Expect.isTrue (0.0 - tolerance <= actual && actual <= 0.0 + tolerance) "minimum should be near 0.0"
                    }

                test "method, 3 arguments" {

                    let testClass = TestClass()
                    let solution =
                        NelderMead.objective testClass.ThreeParameters
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around [ 100.0; 100.0; 100.0 ])
                        |> NelderMead.solve
                    let actual =
                        match solution with
                        | Optimal x -> x.Value
                        | _ -> failwith "unexpected"
                    Expect.isTrue (0.0 - tolerance <= actual && actual <= 0.0 + tolerance) "minimum should be near 0.0"
                    }

                test "static method, 1 argument" {

                    let solution =
                        NelderMead.objective TestClass.StaticOne
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around 100.0)
                        |> NelderMead.solve
                    let actual =
                        match solution with
                        | Optimal x -> x.Value
                        | _ -> failwith "unexpected"
                    Expect.isTrue (0.0 - tolerance <= actual && actual <= 0.0 + tolerance) "minimum should be near 0.0"
                    }

                test "constant function, 1 argument" {

                    let f (x: float) = 0.0
                    let solution =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around 100.0)
                        |> NelderMead.solve
                    let actual =
                        match solution with
                        | Optimal x -> x.Value
                        | _ -> failwith "unexpected"
                    Expect.isTrue (0.0 - tolerance <= actual && actual <= 0.0 + tolerance) "minimum should be near 0.0"
                    }

                test "function, tuple, starting from simplex" {

                    let f (x, y) = pown x 2 + pown y 2
                    let solution =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (
                            Start.at [
                                [ 100.0; 100.0 ]
                                [ 110.0; 100.0 ]
                                [ 105.0; 105.0 ]
                                ]
                            )
                        |> NelderMead.solve
                    let actual =
                        match solution with
                        | Optimal x -> x.Value
                        | _ -> failwith "unexpected"
                    Expect.isTrue (0.0 - tolerance <= actual && actual <= 0.0 + tolerance) "minimum should be near 0.0"
                    }
        ]

    module Maximimization =

        [<Tests>]
        let tests =
            testList "basic maximization tests" [

                test "function, 1 argument" {

                    let f x = - pown (x - 1.0) 2 + 10.0
                    let solution =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around 100.0)
                        |> NelderMead.maximize

                    let value, args =
                        match solution with
                        | Optimal solution -> solution.Value, solution.Point
                        | _ -> failwith "unexpected"

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
                    let solution =
                        NelderMead
                            .Objective(f)
                            .WithMaximumIterations(100)
                            .WithTolerance(0.001)
                            .StartFrom(Start.around 100.0)
                            .Maximize()

                    let value, args =
                        match solution with
                        | Optimal solution -> solution.Value, solution.Point
                        | _ -> failwith "unexpected"

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

                    let solution =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around 10.0)
                        |> NelderMead.solve

                    let actual =
                        match solution with
                        | Optimal x -> x.Value
                        | _ -> failwith "unexpected"

                    Expect.isTrue (0.0 - tolerance <= actual && actual <= 0.0 + tolerance) "minimum should be near 0.0"
                    }
                ]


    module SubOptimalTermination =

        let config =
            { config with
                Termination = {
                    config.Termination with
                        MaximumIterations = Some 1
                }
            }

        [<Tests>]
        let tests =
            testList "sub optimal termination tests" [

                test "function, 1 argument" {

                    let f x = pown x 2
                    let solution =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around 100.0)
                        |> NelderMead.solve

                    let isSuboptimal =
                        match solution with
                        | SubOptimal _ -> true
                        | _ -> failwith "unexpected"

                    Expect.isTrue(isSuboptimal) ""
                    }
                ]

    module AbnormalTermination =

        [<Tests>]
        let tests =
            testList "abnormal termination tests" [

                test "unbounded function" {

                    let f x = x
                    let solution =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around 0.0)
                        |> NelderMead.solve
                    Expect.equal Unbounded solution ""
                    }


                test "function returning nan" {

                    let f (x: float) = nan
                    let solution =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around 0.0)
                        |> NelderMead.solve
                    let isAbnormal =
                        match solution with
                        | Abnormal _ -> true
                        | _ -> false

                    Expect.isTrue(isAbnormal) ""
                    }

                test "simplex containing +infinity" {

                    let f (x: float) = 0.0
                    let solution =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around (+infinity))
                        |> NelderMead.solve
                    let isAbnormal =
                        match solution with
                        | Abnormal _ -> true
                        | _ -> false

                    Expect.isTrue(isAbnormal) ""
                    }

                test "simplex containing infinity" {

                    let f (x: float) = 0.0
                    let solution =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around (infinity))
                        |> NelderMead.solve
                    let isAbnormal =
                        match solution with
                        | Abnormal _ -> true
                        | _ -> false
                    Expect.isTrue(isAbnormal) ""
                    }

                test "simplex containing -infinity" {

                    let f (x: float) = 0.0
                    let solution =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around (-infinity))
                        |> NelderMead.solve
                    let isAbnormal =
                        match solution with
                        | Abnormal _ -> true
                        | _ -> false
                    Expect.isTrue(isAbnormal) ""
                    }

                test "simplex containing nan" {

                    let f (x: float) = 0.0
                    let solution =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around nan)
                        |> NelderMead.solve
                    let isAbnormal =
                        match solution with
                        | Abnormal _ -> true
                        | _ -> false
                    Expect.isTrue(isAbnormal) ""
                    }

                test "function throwing" {

                    let f (x: float) : float = failwith "some exception"
                    let solution =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around 100.0)
                        |> NelderMead.solve
                    let isAbnormal =
                        match solution with
                        | Abnormal _ -> true
                        | _ -> false
                    Expect.isTrue(isAbnormal) ""
                    }

            ]
