namespace Quipu.Tests

module NelderMead =

    open Xunit
    open Quipu
    open Quipu.NelderMead

    let tolerance = 0.01
    let config =
        { Configuration.defaultValue with
            Termination = {
                Configuration.defaultValue.Termination with
                    Tolerance = tolerance
                    MaximumIterations = None //Some 1_000_000
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

        [<Fact>]
        let ``function, 1 argument`` () =

            let f x = pown x 2
            let solution =
                NelderMead.minimize f
                |> NelderMead.withConfiguration config
                |> NelderMead.startFrom (StartingPoint.fromValue 100.0)
                |> NelderMead.solve
            let actual =
                match solution with
                | Optimal (x, _) -> x
                | _ -> failwith "unexpected"
            Assert.InRange(actual, 0.0 - tolerance, 0.0 + tolerance)

        [<Fact>]
        let ``function, tuple`` () =

            let f (x, y) = pown x 2 + pown y 2
            let solution =
                NelderMead.minimize f
                |> NelderMead.withConfiguration config
                |> NelderMead.startFrom (StartingPoint.fromValue [ 100.0; 100.0 ])
                |> NelderMead.solve
            let actual =
                match solution with
                | Optimal (x, _) -> x
                | _ -> failwith "unexpected"
            Assert.InRange(actual, 0.0 - tolerance, 0.0 + tolerance)

        [<Fact>]
        let ``function, truple`` () =

            let f (x, y, z) = pown x 2 + pown y 2 + pown z 2
            let solution =
                NelderMead.minimize f
                |> NelderMead.withConfiguration config
                |> NelderMead.startFrom (StartingPoint.fromValue [ 100.0; 100.0; 100.0 ])
                |> NelderMead.solve
            let actual =
                match solution with
                | Optimal (x, _) -> x
                | _ -> failwith "unexpected"
            Assert.InRange(actual, 0.0 - tolerance, 0.0 + tolerance)

        [<Fact>]
        let ``function, array of arguments`` () =

            let f (x: float []) = pown x.[0] 2 + pown x.[1] 2
            let solution =
                NelderMead.minimize (2, f)
                |> NelderMead.withConfiguration config
                |> NelderMead.startFrom (StartingPoint.fromValue [ 100.0; 100.0 ])
                |> NelderMead.solve
            let actual =
                match solution with
                | Optimal (x, _) -> x
                | _ -> failwith "unexpected"
            Assert.InRange(actual, 0.0 - tolerance, 0.0 + tolerance)

        [<Fact>]
        let ``method, 1 argument`` () =

            let testClass = TestClass()
            let solution =
                NelderMead.minimize testClass.OneParameter
                |> NelderMead.withConfiguration config
                |> NelderMead.startFrom (StartingPoint.fromValue 100.0)
                |> NelderMead.solve
            let actual =
                match solution with
                | Optimal (x, _) -> x
                | _ -> failwith "unexpected"
            Assert.InRange(actual, 0.0 - tolerance, 0.0 + tolerance)

        [<Fact>]
        let ``method, 2 arguments`` () =

            let testClass = TestClass()
            let solution =
                NelderMead.minimize testClass.TwoParameters
                |> NelderMead.withConfiguration config
                |> NelderMead.startFrom (StartingPoint.fromValue [ 100.0; 100.0 ])
                |> NelderMead.solve
            let actual =
                match solution with
                | Optimal (x, _) -> x
                | _ -> failwith "unexpected"
            Assert.InRange(actual, 0.0 - tolerance, 0.0 + tolerance)

        [<Fact>]
        let ``method, 3 arguments`` () =

            let testClass = TestClass()
            let solution =
                NelderMead.minimize testClass.ThreeParameters
                |> NelderMead.withConfiguration config
                |> NelderMead.startFrom (StartingPoint.fromValue [ 100.0; 100.0; 100.0 ])
                |> NelderMead.solve
            let actual =
                match solution with
                | Optimal (x, _) -> x
                | _ -> failwith "unexpected"
            Assert.InRange(actual, 0.0 - tolerance, 0.0 + tolerance)

        [<Fact>]
        let ``static method, 1 argument`` () =

            let solution =
                NelderMead.minimize TestClass.StaticOne
                |> NelderMead.withConfiguration config
                |> NelderMead.startFrom (StartingPoint.fromValue 100.0)
                |> NelderMead.solve
            let actual =
                match solution with
                | Optimal (x, _) -> x
                | _ -> failwith "unexpected"
            Assert.InRange(actual, 0.0 - tolerance, 0.0 + tolerance)

        [<Fact>]
        let ``constant function, 1 argument`` () =

            let f (x: float) = 0.0
            let solution =
                NelderMead.minimize f
                |> NelderMead.withConfiguration config
                |> NelderMead.startFrom (StartingPoint.fromValue 100.0)
                |> NelderMead.solve
            let actual =
                match solution with
                | Optimal (x, _) -> x
                | _ -> failwith "unexpected"
            Assert.InRange(actual, 0.0 - tolerance, 0.0 + tolerance)

        [<Fact>]
        let ``function, tuple, starting from simplex`` () =

            let f (x, y) = pown x 2 + pown y 2
            let solution =
                NelderMead.minimize f
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
            Assert.InRange(actual, 0.0 - tolerance, 0.0 + tolerance)

    module PartiallyDefinedFunctions =

        [<Fact>]
        let ``function defined over positive numbers only`` () =

            let f x = sqrt x

            let solution =
                NelderMead.minimize f
                |> NelderMead.withConfiguration config
                |> NelderMead.startFrom (StartingPoint.fromValue 10.0)
                |> NelderMead.solve

            let actual =
                match solution with
                | Optimal (x, _) -> x
                | _ -> failwith "unexpected"

            Assert.InRange(actual, 0.0 - tolerance, 0.0 + tolerance)

    module SubOptimalTermination =

        let config =
            { config with
                Termination = {
                    config.Termination with
                        MaximumIterations = Some 1
                }
            }

        [<Fact>]
        let ``function, 1 argument`` () =

            let f x = pown x 2
            let solution =
                NelderMead.minimize f
                |> NelderMead.withConfiguration config
                |> NelderMead.startFrom (StartingPoint.fromValue 100.0)
                |> NelderMead.solve

            let isSuboptimal =
                match solution with
                | SubOptimal _ -> true
                | _ -> failwith "unexpected"
            Assert.True(isSuboptimal)

    module AbnormalTermination =

        [<Fact>]
        let ``unbounded function`` () =

            let f x = x
            let solution =
                NelderMead.minimize f
                |> NelderMead.withConfiguration config
                |> NelderMead.startFrom (StartingPoint.fromValue 0.0)
                |> NelderMead.solve
            Assert.Equal(Unbounded, solution)

        [<Fact>]
        let ``function returning nan`` () =

            let f (x: float) = nan
            let solution =
                NelderMead.minimize f
                |> NelderMead.withConfiguration config
                |> NelderMead.startFrom (StartingPoint.fromValue 0.0)
                |> NelderMead.solve
            let isAbnormal =
                match solution with
                | Abnormal _ -> true
                | _ -> false
            Assert.True(isAbnormal)

        [<Fact>]
        let ``simplex containing +infinity`` () =

            let f (x: float) = 0.0
            let solution =
                NelderMead.minimize f
                |> NelderMead.withConfiguration config
                |> NelderMead.startFrom (StartingPoint.fromValue (+infinity))
                |> NelderMead.solve
            let isAbnormal =
                match solution with
                | Abnormal _ -> true
                | _ -> false
            Assert.True(isAbnormal)

        [<Fact>]
        let ``simplex containing infinity`` () =

            let f (x: float) = 0.0
            let solution =
                NelderMead.minimize f
                |> NelderMead.withConfiguration config
                |> NelderMead.startFrom (StartingPoint.fromValue (infinity))
                |> NelderMead.solve
            let isAbnormal =
                match solution with
                | Abnormal _ -> true
                | _ -> false
            Assert.True(isAbnormal)

        [<Fact>]
        let ``simplex containing -infinity`` () =

            let f (x: float) = 0.0
            let solution =
                NelderMead.minimize f
                |> NelderMead.withConfiguration config
                |> NelderMead.startFrom (StartingPoint.fromValue (-infinity))
                |> NelderMead.solve
            let isAbnormal =
                match solution with
                | Abnormal _ -> true
                | _ -> false
            Assert.True(isAbnormal)

        [<Fact>]
        let ``simplex containing nan`` () =

            let f (x: float) = 0.0
            let solution =
                NelderMead.minimize f
                |> NelderMead.withConfiguration config
                |> NelderMead.startFrom (StartingPoint.fromValue nan)
                |> NelderMead.solve
            let isAbnormal =
                match solution with
                | Abnormal _ -> true
                | _ -> false
            Assert.True(isAbnormal)

        [<Fact>]
        let ``function throwing`` () =

            let f (x: float) : float = failwith "some exception"
            let solution =
                NelderMead.minimize f
                |> NelderMead.withConfiguration config
                |> NelderMead.startFrom (StartingPoint.fromValue 100.0)
                |> NelderMead.solve
            let isAbnormal =
                match solution with
                | Abnormal _ -> true
                | _ -> false
            Assert.True(isAbnormal)

    module TerminationCriteria =

        [<Fact>]
        let ``when function values are not within bounds termination should not occur`` () =

            // simplex within bounds, but function values are not
            let tolerance = 0.5
            let f (x: float[]) = pown x.[0] 2 + x.[1]
            let simplex =
                [|
                    [| 1.0; 0.0 |] // f = 1.0
                    [| 1.4; 0.0 |] // f = 1.96
                |]

            let shouldTerminate = Algorithm.terminate tolerance f simplex
            Assert.False(shouldTerminate)

        [<Fact>]
        let ``when argument values are not within bounds termination should not occur`` () =

            // function values within bounds, but simplex values are not
            let tolerance = 0.5
            let f (x: float[]) = 0.1 * x.[0] + 0.1 * x.[1]
            let simplex =
                [|
                    [| 1.0; 0.0 |] // f = 0.1
                    [| 2.0; 0.0 |] // f = 0.2
                |]

            let shouldTerminate = Algorithm.terminate tolerance f simplex
            Assert.False(shouldTerminate)

        [<Fact>]
        let ``when function and argument values are within bounds termination should occur`` () =

            // function values and simplex values are within bounds
            let tolerance = 0.5
            let f (x: float[]) = 0.1 * x.[0] + 0.1 * x.[1]
            let simplex =
                [|
                    [| 0.0; 0.0 |] // f = 0.0
                    [| 0.1; 0.0 |] // f = 0.01
                |]

            let shouldTerminate = Algorithm.terminate tolerance f simplex
            Assert.True(shouldTerminate)

    module Simplex =

        [<Fact>]
        let ``simplex should be centered on origin`` () =

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
                Assert.Equal(expected.[dim], origin.[dim])
                )

        [<Fact>]
        let ``simplex should have the requested radius`` () =

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
                Assert.Equal(radius, d, 3)
                )
