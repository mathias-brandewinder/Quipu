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
            let solution = NelderMead.solve config (Objective.from f) [ 100.0 ]
            let actual =
                match solution with
                | Optimal (x, _) -> x
                | _ -> failwith "unexpected"
            Assert.InRange(actual, 0.0 - tolerance, 0.0 + tolerance)

        [<Fact>]
        let ``function, tuple`` () =

            let f (x, y) = pown x 2 + pown y 2
            let solution = NelderMead.solve config (Objective.from f) [ 100.0; 100.0 ]
            let actual =
                match solution with
                | Optimal (x, _) -> x
                | _ -> failwith "unexpected"
            Assert.InRange(actual, 0.0 - tolerance, 0.0 + tolerance)

        [<Fact>]
        let ``function, truple`` () =

            let f (x, y, z) = pown x 2 + pown y 2 + pown z 2
            let solution = NelderMead.solve config (Objective.from f) [ 100.0; 100.0; 100.0 ]
            let actual =
                match solution with
                | Optimal (x, _) -> x
                | _ -> failwith "unexpected"
            Assert.InRange(actual, 0.0 - tolerance, 0.0 + tolerance)

        [<Fact>]
        let ``function, array of arguments`` () =

            let f (x: float []) = pown x.[0] 2 + pown x.[1] 2
            let solution = NelderMead.solve config (Objective.from (2, f)) [ 100.0; 100.0 ]
            let actual =
                match solution with
                | Optimal (x, _) -> x
                | _ -> failwith "unexpected"
            Assert.InRange(actual, 0.0 - tolerance, 0.0 + tolerance)

        [<Fact>]
        let ``method, 1 argument`` () =

            let testClass = TestClass()
            let solution = NelderMead.solve config (Objective.from testClass.OneParameter) [ 100.0 ]
            let actual =
                match solution with
                | Optimal (x, _) -> x
                | _ -> failwith "unexpected"
            Assert.InRange(actual, 0.0 - tolerance, 0.0 + tolerance)

        [<Fact>]
        let ``method, 2 arguments`` () =

            let testClass = TestClass()
            let solution = NelderMead.solve config (Objective.from testClass.TwoParameters) [ 100.0; 100.0 ]
            let actual =
                match solution with
                | Optimal (x, _) -> x
                | _ -> failwith "unexpected"
            Assert.InRange(actual, 0.0 - tolerance, 0.0 + tolerance)

        [<Fact>]
        let ``method, 3 arguments`` () =

            let testClass = TestClass()
            let solution = NelderMead.solve config (Objective.from testClass.ThreeParameters) [ 100.0; 100.0; 100.0 ]
            let actual =
                match solution with
                | Optimal (x, _) -> x
                | _ -> failwith "unexpected"
            Assert.InRange(actual, 0.0 - tolerance, 0.0 + tolerance)

        [<Fact>]
        let ``static method, 1 argument`` () =

            let solution = NelderMead.solve config (Objective.from TestClass.StaticOne) [ 100.0 ]
            let actual =
                match solution with
                | Optimal (x, _) -> x
                | _ -> failwith "unexpected"
            Assert.InRange(actual, 0.0 - tolerance, 0.0 + tolerance)

        [<Fact>]
        let ``constant function, 1 argument`` () =

            let f (x: float) = 0.0
            let solution = NelderMead.solve config (Objective.from f) [ 100.0 ]
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
            let solution = NelderMead.solve config (Objective.from f) [ 100.0 ]
            let isSuboptimal =
                match solution with
                | SubOptimal _ -> true
                | _ -> failwith "unexpected"
            Assert.True(isSuboptimal)

    module AbnormalTermination =

        [<Fact>]
        let ``unbounded function`` () =

            let f x = x
            let solution = NelderMead.solve config (Objective.from f) [ 100.0 ]
            Assert.Equal(Unbounded, solution)

        [<Fact>]
        let ``function returning nan`` () =

            let f (x: float) = nan
            let solution = NelderMead.solve config (Objective.from f) [ 100.0 ]
            let isAbnormal =
                match solution with
                | Abnormal _ -> true
                | _ -> false
            Assert.True(isAbnormal)

        [<Fact(Skip="Todo")>]
        let ``function returning +infinity`` () =

            let f (x: float) = +infinity
            Assert.Throws<Abnormal>(fun _ ->
                NelderMead.solve config (Objective.from f) [ 100.0 ]
                |> ignore
                )

        [<Fact(Skip="Todo")>]
        let ``function returning infinity`` () =

            let f (x: float) = infinity
            Assert.Throws<Abnormal>(fun _ ->
                NelderMead.solve config (Objective.from f) [ 100.0 ]
                |> ignore
                )

        [<Fact>]
        let ``function throwing`` () =

            let f (x: float) : float = failwith "some exception"
            let solution = NelderMead.solve config (Objective.from f) [ 100.0 ]
            let isAbnormal =
                match solution with
                | Abnormal _ -> true
                | _ -> false
            Assert.True(isAbnormal)
