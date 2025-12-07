namespace Quipu.Tests

module GoalSeek =

    open Expecto
    open Quipu

    let tolerance = 0.01
    let config =
        { Configuration.defaultValue with
            Tolerance = tolerance
        }

    [<Tests>]
    let tests =
        testList "goal seek tests" [

            test "happy path: function, 1 argument" {

                let f x = pown (x - 1.0) 2
                let target = 16.0

                let solverResult =
                    NelderMead.objective f
                    |> NelderMead.withConfiguration config
                    |> NelderMead.goalSeek target

                let solution = solverResult.Solution
                let value = solution.Candidate.Value

                Expect.isWithin tolerance value target "The solution value should be close to target"
                Expect.equal solverResult.Solution.Status Status.Optimal "The solution should be optimal"
                }

            test "happy path: function, 2 arguments" {

                let f (x, y) = pown (x - 1.0) 2 + pown (y - 2.0) 2 - 10.0
                let target = 100.0

                let solverResult =
                    NelderMead.objective f
                    |> NelderMead.withConfiguration config
                    |> NelderMead.goalSeek target

                let solution = solverResult.Solution
                let value = solution.Candidate.Value

                Expect.isWithin tolerance value target "The solution value should be close to target"
                Expect.equal solverResult.Solution.Status Status.Optimal "The solution should be optimal"
                }

            test "a function with no exact solution should have status suboptimal" {

                // f is always positive, so it cannot equal -100.0
                let f (x) = pown (x - 1.0) 2
                let target = -100.0

                let solverResult =
                    NelderMead.objective f
                    |> NelderMead.withConfiguration config
                    |> NelderMead.goalSeek target

                // the solver will return the closest solution it found,
                // but mark it as sub-optimal
                Expect.isTrue solverResult.HasSolution "The solver should find a solution"
                Expect.equal solverResult.Solution.Status Status.Suboptimal "The solution should be sub-optimal"
                }

            test "solution should be within tolerance" {

                // we set a looser tolerance than the default value
                let tolerance = 0.1
                let f x = pown (x - 1.0) 2
                // f is always greater than 0.0, but -0.1 is within tolerance
                let target = - 0.1

                let solverResult =
                    NelderMead.objective f
                    |> NelderMead.withConfiguration config
                    |> NelderMead.withTolerance tolerance
                    |> NelderMead.goalSeek target

                let solution = solverResult.Solution
                let value = solution.Candidate.Value

                Expect.isWithin tolerance value target "The solution value should be close to target"
                Expect.equal solution.Status Status.Optimal "The solution should be optimal"
                }
        ]
