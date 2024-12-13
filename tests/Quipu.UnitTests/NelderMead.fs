namespace Quipu.Tests

module NelderMead =

    open Expecto
    open Quipu
    open Quipu.NelderMead

    let tolerance = 0.01
    let config =
        { Configuration.defaultValue with
            Termination = {
                Configuration.defaultValue.Termination with
                    Tolerance = tolerance
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
                        |> NelderMead.startFrom (StartingPoint.fromValue 100.0)
                        |> NelderMead.solve
                    let actual =
                        match solution with
                        | Optimal (x, _) -> x
                        | _ -> failwith "unexpected"

                    Expect.isTrue (0.0 - tolerance <= actual && actual <= 0.0 + tolerance) "minimum should be near 0.0"
                    }

                test "function, tuple" {

                    let f (x, y) = pown x 2 + pown y 2
                    let solution =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (StartingPoint.fromValue [ 100.0; 100.0 ])
                        |> NelderMead.solve
                    let actual =
                        match solution with
                        | Optimal (x, _) -> x
                        | _ -> failwith "unexpected"
                    Expect.isTrue (0.0 - tolerance <= actual && actual <= 0.0 + tolerance) "minimum should be near 0.0"
                    }

                test "function, truple" {

                    let f (x, y, z) = pown x 2 + pown y 2 + pown z 2
                    let solution =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (StartingPoint.fromValue [ 100.0; 100.0; 100.0 ])
                        |> NelderMead.solve

                    let actual =
                        match solution with
                        | Optimal (x, _) -> x
                        | _ -> failwith "unexpected"

                    Expect.isTrue (0.0 - tolerance <= actual && actual <= 0.0 + tolerance) "minimum should be near 0.0"
                    }

                test "function, array of arguments" {

                    let f (x: float []) = pown x.[0] 2 + pown x.[1] 2
                    let solution =
                        NelderMead.objective (2, f)
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (StartingPoint.fromValue [ 100.0; 100.0 ])
                        |> NelderMead.solve

                    let actual =
                        match solution with
                        | Optimal (x, _) -> x
                        | _ -> failwith "unexpected"

                    Expect.isTrue (0.0 - tolerance <= actual && actual <= 0.0 + tolerance) "minimum should be near 0.0"
                    }

                test "method, 1 argument" {

                    let testClass = TestClass()
                    let solution =
                        NelderMead.objective testClass.OneParameter
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (StartingPoint.fromValue 100.0)
                        |> NelderMead.solve
                    let actual =
                        match solution with
                        | Optimal (x, _) -> x
                        | _ -> failwith "unexpected"
                    Expect.isTrue (0.0 - tolerance <= actual && actual <= 0.0 + tolerance) "minimum should be near 0.0"
                    }


                test "method, 2 arguments" {

                    let testClass = TestClass()
                    let solution =
                        NelderMead.objective testClass.TwoParameters
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (StartingPoint.fromValue [ 100.0; 100.0 ])
                        |> NelderMead.solve
                    let actual =
                        match solution with
                        | Optimal (x, _) -> x
                        | _ -> failwith "unexpected"
                    Expect.isTrue (0.0 - tolerance <= actual && actual <= 0.0 + tolerance) "minimum should be near 0.0"
                    }

                test "method, 3 arguments" {

                    let testClass = TestClass()
                    let solution =
                        NelderMead.objective testClass.ThreeParameters
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (StartingPoint.fromValue [ 100.0; 100.0; 100.0 ])
                        |> NelderMead.solve
                    let actual =
                        match solution with
                        | Optimal (x, _) -> x
                        | _ -> failwith "unexpected"
                    Expect.isTrue (0.0 - tolerance <= actual && actual <= 0.0 + tolerance) "minimum should be near 0.0"
                    }

                test "static method, 1 argument" {

                    let solution =
                        NelderMead.objective TestClass.StaticOne
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (StartingPoint.fromValue 100.0)
                        |> NelderMead.solve
                    let actual =
                        match solution with
                        | Optimal (x, _) -> x
                        | _ -> failwith "unexpected"
                    Expect.isTrue (0.0 - tolerance <= actual && actual <= 0.0 + tolerance) "minimum should be near 0.0"
                    }

                test "constant function, 1 argument" {

                    let f (x: float) = 0.0
                    let solution =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (StartingPoint.fromValue 100.0)
                        |> NelderMead.solve
                    let actual =
                        match solution with
                        | Optimal (x, _) -> x
                        | _ -> failwith "unexpected"
                    Expect.isTrue (0.0 - tolerance <= actual && actual <= 0.0 + tolerance) "minimum should be near 0.0"
                    }

                test "function, tuple, starting from simplex" {

                    let f (x, y) = pown x 2 + pown y 2
                    let solution =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (
                            StartingPoint.fromValue [
                                [ 100.0; 100.0 ]
                                [ 110.0; 100.0 ]
                                [ 105.0; 105.0 ]
                                ]
                            )
                        |> NelderMead.solve
                    let actual =
                        match solution with
                        | Optimal (x, _) -> x
                        | _ -> failwith "unexpected"
                    Expect.isTrue (0.0 - tolerance <= actual && actual <= 0.0 + tolerance) "minimum should be near 0.0"
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
                        |> NelderMead.startFrom (StartingPoint.fromValue 10.0)
                        |> NelderMead.solve

                    let actual =
                        match solution with
                        | Optimal (x, _) -> x
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
                        |> NelderMead.startFrom (StartingPoint.fromValue 100.0)
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
                        |> NelderMead.startFrom (StartingPoint.fromValue 0.0)
                        |> NelderMead.solve
                    Expect.equal Unbounded solution ""
                    }


                test "function returning nan" {

                    let f (x: float) = nan
                    let solution =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (StartingPoint.fromValue 0.0)
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
                        |> NelderMead.startFrom (StartingPoint.fromValue (+infinity))
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
                        |> NelderMead.startFrom (StartingPoint.fromValue (infinity))
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
                        |> NelderMead.startFrom (StartingPoint.fromValue (-infinity))
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
                        |> NelderMead.startFrom (StartingPoint.fromValue nan)
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
                        |> NelderMead.startFrom (StartingPoint.fromValue 100.0)
                        |> NelderMead.solve
                    let isAbnormal =
                        match solution with
                        | Abnormal _ -> true
                        | _ -> false
                    Expect.isTrue(isAbnormal) ""
                    }

            ]

    module TerminationCriteria =

        [<Tests>]
        let tests =
            testList "termination tests" [
                test "when function values are not within bounds termination should not occur" {

                    // simplex within bounds, but function values are not
                    let tolerance = 0.5
                    let simplex =
                        [|
                            { Point = [| 0.0; 0.0 |]; Value = 0.0 }
                            { Point = [| 1.0; 0.0 |]; Value = tolerance + 0.1 }
                        |]

                    let shouldTerminate = Algorithm.terminate tolerance simplex
                    Expect.isFalse(shouldTerminate) ""
                    }

                test "when argument values are not within bounds termination should not occur" {

                    // function values within bounds, but simplex values are not
                    let tolerance = 0.5
                    let simplex =
                        [|
                            { Point = [| 0.0; 0.0 |]; Value = 0.0 }
                            { Point = [| tolerance + 0.1; 0.0 |]; Value = 0.1 }
                        |]

                    let shouldTerminate = Algorithm.terminate tolerance simplex
                    Expect.isFalse(shouldTerminate) ""
                    }

                test "when function and argument values are within bounds termination should occur" {

                    // function values and simplex values are within bounds
                    let tolerance = 0.5
                    let f (x: float[]) = 0.1 * x.[0] + 0.1 * x.[1]
                    let simplex =
                        [|
                            { Point = [| 0.0; 0.0 |]; Value = 0.0 }
                            { Point = [| 0.0 + tolerance / 2.0; 0.0 + tolerance / 2.0 |]; Value = 0.0 + tolerance / 2.0 }
                        |]

                    let shouldTerminate = Algorithm.terminate tolerance simplex
                    Expect.isTrue(shouldTerminate) ""
                    }
            ]

    module Simplex =

        [<Tests>]
        let tests =
            testList "simplex tests" [
                test "simplex should be centered on origin" {

                    let origin = [| 10.0; 5.0; -20.0 |]
                    let radius = 3.0
                    let simplex = Simplex.create(origin, radius)

                    let dims = [| 0 .. simplex.dimension - 1 |]
                    let expected =
                        dims
                        |> Array.map (fun dim ->
                            simplex
                            |> Simplex.vertices
                            |> Array.averageBy (fun vertex -> vertex.[dim])
                            )

                    dims
                    |> Array.iter (fun dim ->
                        Expect.equal expected.[dim] origin.[dim] ""
                        )
                    }


                test "simplex should have the requested radius" {

                    let origin = [| 10.0; 5.0; -20.0 |]
                    let radius = 3.0
                    let simplex = Simplex.create(origin, radius)

                    let distance xs ys =
                        (xs, ys)
                        ||> Array.map2 (fun x y -> pown (x - y) 2)
                        |> Array.sum
                        |> sqrt

                    simplex
                    |> Simplex.vertices
                    |> Array.iter (fun vertex ->
                        let d = distance vertex origin
                        Expect.floatClose { absolute = 0.001; relative = 0.001 } radius d "" // 3 decimals, really
                        )
                    }
                ]