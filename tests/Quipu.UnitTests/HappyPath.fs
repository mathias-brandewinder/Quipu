namespace Quipu.Tests

module HappyPath =

    open Expecto
    open Quipu

    let tolerance = 0.01
    let config =
        { Configuration.defaultValue with
            Tolerance = tolerance
        }

    // Create a test case function in dimension d,
    // where we know both the minimum value and the arguments:
    // f(x) = (x1 - 1) ^ 2 + (x2 - 2) ^ 2 ... + (xdim - dim) ^2 + 42
    // The function has a unique minimum, 42, reached for
    // x1 = 1, x2 = 2, ... xdim = dim
    type SimpleMinimization (dim: int) =

        let expectedMinimum = 42.0
        let expectedSolution =
            Array.init dim (fun i -> float (i + 1))

        member this.objective =
            fun (xs: float []) ->
                [ 0 .. (dim - 1) ]
                |> List.sumBy (fun i ->
                    pown (xs.[i] - float (i + 1)) 2
                    )
                |> fun total -> total + expectedMinimum

        member this.verify (solution: Solution) =
            Expect.isTrue (solution.Status = Status.Optimal) "solution should be optimal"
            // verify value
            let expected = expectedMinimum
            let actual = solution.Candidate.Value
            Expect.isWithin tolerance actual expected $"minimum should be near {expected}, actual {actual}"
            // verify function arguments
            for i in 0 .. (dim - 1) do
                let expected = expectedSolution.[i]
                let actual = solution.Candidate.Arguments.[i]
                Expect.isWithin tolerance actual expected $"argument {i} should be near {expected}, actual {actual}"

    type SimpleMaximization (dim: int) =

        let expectedMinimum = 42.0
        let expectedSolution =
            Array.init dim (fun i -> float (i + 1))

        member this.objective =
            fun (xs: float []) ->
                [ 0 .. (dim - 1) ]
                |> List.sumBy (fun i ->
                    pown (xs.[i] - float (i + 1)) 2
                    )
                |> fun total -> - total + expectedMinimum

        member this.verify (solution: Solution) =
            Expect.isTrue (solution.Status = Status.Optimal) "solution should be optimal"
            // verify value
            let expected = expectedMinimum
            let actual = solution.Candidate.Value
            Expect.isWithin tolerance actual expected $"minimum should be near {expected}, actual {actual}"
            // verify function arguments
            for i in 0 .. (dim - 1) do
                let expected = expectedSolution.[i]
                let actual = solution.Candidate.Arguments.[i]
                Expect.isWithin tolerance actual expected $"argument {i} should be near {expected}, actual {actual}"

    module BasicConvergence =

        type TestClass() =
            member this.OneParameter(x: float) =
                SimpleMinimization(1).objective [| x |]
            member this.TwoParameters(x: float, y: float) =
                SimpleMinimization(2).objective [| x; y |]
            member this.ThreeParameters(x: float, y: float, z: float) =
                SimpleMinimization(3).objective [| x; y; z |]
            static member StaticOne(x: float) =
                SimpleMinimization(1).objective [| x |]

        [<Tests>]
        let tests =

            testList "happy path" [

                testList "minimization" [

                    test "function, 1 argument" {

                        let f x = SimpleMinimization(1).objective [| x |]

                        let solverResult =
                            NelderMead.objective f
                            |> NelderMead.withConfiguration config
                            |> NelderMead.startFrom (Start.around 100.0)
                            |> NelderMead.solve

                        SimpleMinimization(1).verify solverResult.Solution
                        }

                    test "function, tuple" {

                        let f (x, y) = SimpleMinimization(2).objective [| x; y |]

                        let solverResult =
                            NelderMead.objective f
                            |> NelderMead.withConfiguration config
                            |> NelderMead.startFrom (Start.around [ 100.0; 100.0 ])
                            |> NelderMead.solve

                        SimpleMinimization(2).verify solverResult.Solution
                        }

                    test "function, truple" {

                        let f (x, y, z) = SimpleMinimization(3).objective [| x; y; z |]

                        let solverResult =
                            NelderMead.objective f
                            |> NelderMead.withConfiguration config
                            |> NelderMead.startFrom (Start.around [ 100.0; 100.0; 100.0 ])
                            |> NelderMead.solve

                        SimpleMinimization(2).verify solverResult.Solution
                        }

                    test "function, 2 arguments" {

                        let f x y = SimpleMinimization(2).objective [| x; y |]

                        let solverResult =
                            NelderMead.objective f
                            |> NelderMead.withConfiguration config
                            |> NelderMead.startFrom (Start.around [ 100.0; 100.0 ])
                            |> NelderMead.solve

                        SimpleMinimization(2).verify solverResult.Solution
                        }

                    test "function, 3 arguments" {

                        let f x y z = SimpleMinimization(3).objective [| x; y; z |]

                        let solverResult =
                            NelderMead.objective f
                            |> NelderMead.withConfiguration config
                            |> NelderMead.startFrom (Start.around [ 100.0; 100.0; 100.0 ])
                            |> NelderMead.solve

                        SimpleMinimization(3).verify solverResult.Solution
                        }

                    test "function, array of arguments" {

                        let dim = 5
                        let f (x: float []) = SimpleMinimization(dim).objective x

                        let solverResult =
                            NelderMead.objective (dim, f)
                            |> NelderMead.withConfiguration config
                            |> NelderMead.startFrom (Start.around (Array.create 5 100.0))
                            |> NelderMead.solve

                        SimpleMinimization(dim).verify solverResult.Solution
                        }

                    test "method, 1 argument" {

                        let testClass = TestClass()

                        let solverResult =
                            NelderMead.objective testClass.OneParameter
                            |> NelderMead.withConfiguration config
                            |> NelderMead.startFrom (Start.around 100.0)
                            |> NelderMead.solve

                        SimpleMinimization(1).verify solverResult.Solution
                        }

                    test "method, 2 arguments" {

                        let testClass = TestClass()

                        let solverResult =
                            NelderMead.objective testClass.TwoParameters
                            |> NelderMead.withConfiguration config
                            |> NelderMead.startFrom (Start.around [ 100.0; 100.0 ])
                            |> NelderMead.solve

                        SimpleMinimization(2).verify solverResult.Solution
                        }

                    test "method, 3 arguments" {

                        let testClass = TestClass()

                        let solverResult =
                            NelderMead.objective testClass.ThreeParameters
                            |> NelderMead.withConfiguration config
                            |> NelderMead.startFrom (Start.around [ 100.0; 100.0; 100.0 ])
                            |> NelderMead.solve

                        SimpleMinimization(3).verify solverResult.Solution
                        }

                    test "static method, 1 argument" {

                        let solverResult =
                            NelderMead.objective TestClass.StaticOne
                            |> NelderMead.withConfiguration config
                            |> NelderMead.startFrom (Start.around 100.0)
                            |> NelderMead.solve

                        SimpleMinimization(1).verify solverResult.Solution
                        }

                    test "function, tuple, starting from simplex" {

                        let f (x, y) = SimpleMinimization(2).objective [| x; y |]

                        let solverResult =
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

                        SimpleMinimization(2).verify solverResult.Solution
                        }

                    test "function, tuple, starting from tuple" {

                        let f (x, y) = SimpleMinimization(2).objective [| x; y |]

                        let solverResult =
                            NelderMead.objective f
                            |> NelderMead.withConfiguration config
                            |> NelderMead.startFrom (Start.around (100.0, 100.0))
                            |> NelderMead.solve

                        SimpleMinimization(2).verify solverResult.Solution
                        }

                    test "function, truple, starting from truple" {

                        let f (x, y, z) = SimpleMinimization(3).objective [| x; y; z |]

                        let solverResult =
                            NelderMead.objective f
                            |> NelderMead.withConfiguration config
                            |> NelderMead.startFrom (Start.around (100.0, 100.0, 100.0))
                            |> NelderMead.solve

                        SimpleMinimization(3).verify solverResult.Solution
                        }
                    ]

                testList "maximization" [

                    test "function, 1 argument" {

                        let f x = SimpleMaximization(1).objective [| x |]

                        let solverResult =
                            NelderMead.objective f
                            |> NelderMead.withConfiguration config
                            |> NelderMead.startFrom (Start.around 100.0)
                            |> NelderMead.maximize

                        SimpleMaximization(1).verify solverResult.Solution
                        }

                    test "function, tuple" {

                        let f (x, y) = SimpleMaximization(2).objective [| x; y |]

                        let solverResult =
                            NelderMead.objective f
                            |> NelderMead.withConfiguration config
                            |> NelderMead.startFrom (Start.around [ 100.0; 100.0 ])
                            |> NelderMead.maximize

                        SimpleMaximization(2).verify solverResult.Solution
                        }

                    test "function, truple" {

                        let f (x, y, z) = SimpleMaximization(3).objective [| x; y; z |]

                        let solverResult =
                            NelderMead.objective f
                            |> NelderMead.withConfiguration config
                            |> NelderMead.startFrom (Start.around [ 100.0; 100.0; 100.0 ])
                            |> NelderMead.maximize

                        SimpleMaximization(2).verify solverResult.Solution
                        }

                    test "function, array of arguments" {

                        let dim = 5
                        let f (x: float []) = SimpleMaximization(dim).objective x

                        let solverResult =
                            NelderMead.objective (dim, f)
                            |> NelderMead.withConfiguration config
                            |> NelderMead.startFrom (Start.around (Array.create 5 100.0))
                            |> NelderMead.maximize

                        SimpleMaximization(dim).verify solverResult.Solution
                        }
                ]
            ]