namespace Quipu.Tests

module SampleUsage =

    open Quipu
    open Expecto

    [<Tests>]
    let tests =

        testList "sample usage" [

            test "simple" {

                let f (x, y) = pown (x - 1.0) 2 + pown (y - 2.0) 2 + 42.0

                let solverResult =
                    NelderMead.objective f
                    |> NelderMead.minimize

                Expect.isTrue (solverResult.HasSolution) "should have a solution"
                let solution = solverResult.Solution
                Expect.isTrue (solution.Status = Status.Optimal) "should be optimal"
                Expect.isWithin 0.001 solution.Candidate.Value 42.0 "minimum should be at 42"
                Expect.isWithin 0.001 solution.Candidate.Arguments[0] 1.0 "x should be at 1"
                Expect.isWithin 0.001 solution.Candidate.Arguments[1] 2.0 "y should be at 2"
                }

            test "advanced configuration" {

                let f (x, y) = pown (x - 1.0) 2 + pown (y - 2.0) 2 + 42.0

                let tolerance = 0.000_0001

                let solverResult =
                    NelderMead.objective f
                    |> NelderMead.withTolerance tolerance
                    |> NelderMead.startFrom (Start.around [ 100.0; 100.0])
                    |> NelderMead.minimize

                Expect.isTrue (solverResult.HasSolution) "should have a solution"
                let solution = solverResult.Solution
                Expect.isTrue (solution.Status = Status.Optimal) "should be optimal"
                Expect.isWithin tolerance solution.Candidate.Value 42.0 "minimum should be at 42"
                Expect.isWithin tolerance solution.Candidate.Arguments[0] 1.0 "x should be at 1"
                Expect.isWithin tolerance solution.Candidate.Arguments[1] 2.0 "y should be at 2"
                }
            ]