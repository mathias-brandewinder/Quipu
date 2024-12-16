namespace Quipu.Tests

module TestFunctions =

    open Quipu
    open Expecto

    // See https://en.wikipedia.org/wiki/Test_functions_for_optimization
    let beale (x, y) =
        pown (1.5 - x + (x * y)) 2
        +
        pown (2.25 - x + (x * pown y 2)) 2
        +
        pown (2.625 - x + x * pown y 3) 2

    let booth (x, y) =
        pown (x + 2.0 * y - 7.0) 2
        +
        pown (2.0 * x + y - 5.0) 2

    [<Tests>]
    let tests =

        let tolerance = 0.000_001
        let solverConfiguration =
            { Configuration.defaultValue with
                Termination = {
                    MaximumIterations = None
                    Termination = Termination.tolerance tolerance
                    }
            }

        testList "standard optimization test functions" [

            test "beale function" {

                let solverResult =
                    beale
                    |> NelderMead.objective
                    |> NelderMead.withConfiguration solverConfiguration
                    |> NelderMead.startFrom (Start.around [ 4.5; 4.5 ])
                    |> NelderMead.solve

                let solution =
                    match solverResult with
                    | Solution solution -> solution
                    | _ -> failwith "unexpected"

                Expect.equal solution.Status Status.Optimal "optimal solution"

                let value, point = solution.Candidate.Value, solution.Candidate.Point
                Expect.floatClose { absolute = tolerance; relative = tolerance } value 0.0 "function value"

                Expect.floatClose { absolute = tolerance; relative = tolerance } point[0] 3.0 "function x0"
                Expect.floatClose { absolute = tolerance; relative = tolerance } point[1] 0.5 "function x1"
                }

            test "booth function" {

                let solverResult =
                    booth
                    |> NelderMead.objective
                    |> NelderMead.withConfiguration solverConfiguration
                    |> NelderMead.startFrom (Start.around [ 10.0; 10.0 ])
                    |> NelderMead.solve

                let solution =
                    match solverResult with
                    | Solution solution -> solution
                    | _ -> failwith "unexpected"

                Expect.equal solution.Status Status.Optimal "optimal solution"

                let value, point = solution.Candidate.Value, solution.Candidate.Point


                Expect.floatClose { absolute = tolerance; relative = tolerance } value 0.0 "function value"

                Expect.floatClose { absolute = tolerance; relative = tolerance } point[0] 1.0 "function x0"
                Expect.floatClose { absolute = tolerance; relative = tolerance } point[1] 3.0 "function x1"
                }
            ]
