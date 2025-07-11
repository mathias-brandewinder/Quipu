namespace Quipu.Tests

module TestFunctions =

    open Quipu
    open Quipu.Tests.Functions
    open Expecto

    [<Tests>]
    let tests =

        let tolerance = 0.000_001
        let solverConfiguration =
            { Configuration.defaultValue with
                MaximumIterations = None
                Termination = Termination.tolerance tolerance
            }

        testList "standard optimization test functions" [

            test "beale function" {

                let solverResult =
                    beale
                    |> NelderMead.objective
                    |> NelderMead.withConfiguration solverConfiguration
                    |> NelderMead.startFrom (Start.around [ 4.5; 4.5 ])
                    |> NelderMead.solve

                let solution = solverResult.Solution
                Expect.equal solution.Status Status.Optimal "optimal solution"

                let value, point = solution.Candidate.Value, solution.Candidate.Arguments
                Expect.isWithin tolerance value 0.0 "function value"

                Expect.isWithin tolerance point[0] 3.0 "function x0"
                Expect.isWithin tolerance point[1] 0.5 "function x1"
                }

            test "booth function" {

                let solverResult =
                    booth
                    |> NelderMead.objective
                    |> NelderMead.withConfiguration solverConfiguration
                    |> NelderMead.startFrom (Start.around [ 10.0; 10.0 ])
                    |> NelderMead.solve

                let solution = solverResult.Solution
                Expect.equal solution.Status Status.Optimal "optimal solution"

                let value, point = solution.Candidate.Value, solution.Candidate.Arguments

                Expect.isWithin tolerance value 0.0 "function value"

                Expect.isWithin tolerance point[0] 1.0 "function x0"
                Expect.isWithin tolerance point[1] 3.0 "function x1"
                }

            ptest "bukin function" {
                let solverResult =
                    bukin
                    |> NelderMead.objective
                    |> NelderMead.withConfiguration solverConfiguration
                    |> NelderMead.solve

                let solution = solverResult.Solution
                Expect.equal solution.Status Status.Optimal "optimal solution"

                let value, point = solution.Candidate.Value, solution.Candidate.Arguments

                Expect.isWithin tolerance value 0.0 "function value"

                Expect.isWithin tolerance point[0] -10.0 "function x0"
                Expect.isWithin tolerance point[1] 1.0 "function x1"
                }

            test "himmelblau function" {
                let solverResult =
                    himmelblau
                    |> NelderMead.objective
                    |> NelderMead.withConfiguration solverConfiguration
                    |> NelderMead.solve

                let solution = solverResult.Solution
                Expect.equal solution.Status Status.Optimal "optimal solution"

                let value, point = solution.Candidate.Value, solution.Candidate.Arguments

                Expect.isWithin tolerance value 0.0 "function value"

                // 4 possible minima
                [
                    (3.0, 2.0)
                    (-2.805118, 3.131312)
                    (-3.779310, -3.283186)
                    (3.584428, -1.848126)
                ]
                |> List.exists (fun (x0, x1) ->
                    Value.isWithin tolerance point[0] x0
                    &&
                    Value.isWithin tolerance point[1] x1
                    )
                |> fun check -> Expect.isTrue check "function x0, y0"
                }
            ]
