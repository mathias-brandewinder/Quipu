namespace Quipu.Tests

module HappyPath =

    open Expecto
    open Quipu

    let tolerance = 0.01
    let config =
        { Configuration.defaultValue with
            Termination = Termination.tolerance tolerance
        }

    // Create a test case function in dimension d,
    // where we know both the minimum value and the arguments:
    // f(x) = (x1 - 1) ^ 2 + (x2 - 2) ^ 2 ... + (xdim - dim) ^2 + 42
    // The function has a unique minimum, 42, reached for
    // x1 = 1, x2 = 2, ... xdim = dim
    type SimpleTest (dim: int) =

        let expectedMinimum = 42.0
        let expectedSolution =
            Array.init dim (fun i -> float (i + 1))

        member this.testFunction =
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
            Expect.isTrue (expected - tolerance <= actual && actual <= expected + tolerance) $"minimum should be near {expected}, actual {actual}"
            // verify function arguments
            for i in 0 .. (dim - 1) do
                let expected = expectedSolution.[i]
                let actual = solution.Candidate.Arguments.[i]
                Expect.isTrue (expected - tolerance <= actual && actual <= expected + tolerance) $"argument {i} should be near {expected}, actual {actual}"

    module BasicConvergence =

        // We test variations of minimize f(x) = x ^ 2,
        // which has a minimum value of 0.0 for x = 0.0.
        let expected = 0.0

        type TestClass() =
            member this.OneParameter(x: float) =
                SimpleTest(1).testFunction [| x |]
            member this.TwoParameters(x: float, y: float) =
                SimpleTest(2).testFunction [| x; y |]
            member this.ThreeParameters(x: float, y: float, z: float) =
                SimpleTest(3).testFunction [| x; y; z |]
            static member StaticOne(x: float) =
                SimpleTest(1).testFunction [| x |]

        [<Tests>]
        let tests =
            testList "happy path tests" [

                test "function, 1 argument" {

                    let f x = SimpleTest(1).testFunction [| x |]

                    let solverResult =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around 100.0)
                        |> NelderMead.solve

                    if solverResult.HasSolution
                    then
                        SimpleTest(1).verify solverResult.Solution
                    else failwith "unexpected"
                    }

                test "function, tuple" {

                    let f (x, y) = SimpleTest(2).testFunction [| x; y |]

                    let solverResult =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around [ 100.0; 100.0 ])
                        |> NelderMead.solve

                    if solverResult.HasSolution
                    then
                        SimpleTest(2).verify solverResult.Solution
                    else failwith "unexpected"
                    }

                test "function, truple" {

                    let f (x, y, z) = SimpleTest(3).testFunction [| x; y; z |]

                    let solverResult =
                        NelderMead.objective f
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around [ 100.0; 100.0; 100.0 ])
                        |> NelderMead.solve

                    if solverResult.HasSolution
                    then
                        SimpleTest(2).verify solverResult.Solution
                    else failwith "unexpected"
                    }

                test "function, array of arguments" {

                    let dim = 5
                    let f (x: float []) = SimpleTest(dim).testFunction x

                    let solverResult =
                        NelderMead.objective (dim, f)
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around (Array.create 5 100.0))
                        |> NelderMead.solve

                    if solverResult.HasSolution
                    then
                        SimpleTest(dim).verify solverResult.Solution
                    else failwith "unexpected"
                    }

                test "method, 1 argument" {

                    let testClass = TestClass()

                    let solverResult =
                        NelderMead.objective testClass.OneParameter
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around 100.0)
                        |> NelderMead.solve

                    if solverResult.HasSolution
                    then
                        SimpleTest(1).verify solverResult.Solution
                    else failwith "unexpected"
                    }

                test "method, 2 arguments" {

                    let testClass = TestClass()

                    let solverResult =
                        NelderMead.objective testClass.TwoParameters
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around [ 100.0; 100.0 ])
                        |> NelderMead.solve

                    if solverResult.HasSolution
                    then
                        SimpleTest(2).verify solverResult.Solution
                    else failwith "unexpected"
                    }

                test "method, 3 arguments" {

                    let testClass = TestClass()

                    let solverResult =
                        NelderMead.objective testClass.ThreeParameters
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around [ 100.0; 100.0; 100.0 ])
                        |> NelderMead.solve

                    if solverResult.HasSolution
                    then
                        SimpleTest(3).verify solverResult.Solution
                    else failwith "unexpected"
                    }

                test "static method, 1 argument" {

                    let solverResult =
                        NelderMead.objective TestClass.StaticOne
                        |> NelderMead.withConfiguration config
                        |> NelderMead.startFrom (Start.around 100.0)
                        |> NelderMead.solve

                    if solverResult.HasSolution
                    then
                        SimpleTest(1).verify solverResult.Solution
                    else failwith "unexpected"
                    }

                test "function, tuple, starting from simplex" {

                    let f (x, y) = SimpleTest(2).testFunction [| x; y |]

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

                    if solverResult.HasSolution
                    then
                        SimpleTest(2).verify solverResult.Solution
                    else failwith "unexpected"
                    }
        ]