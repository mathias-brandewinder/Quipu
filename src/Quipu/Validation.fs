namespace Quipu

module Validation =

    // does the objective throw
    let private evaluateObjective (objective: IVectorFunction) (simplex: float[][]) =
        try
            simplex
            |> Array.map (fun args ->
                {
                    Arguments = args
                    Value = objective.Value args
                }
                )
            |> Ok
        with
        | ex ->
            {
                Message = $"Invalid initial Simplex: exception thrown {ex.Message}"
                Simplex = simplex
            }
            |> Abnormal
            |> Error

    // If the objective value is -infinity, the problem has no minimum and is
    // unbounded: we exit early.
    let private checkUnboundedObjective (evaluations: Evaluation []) =
        evaluations
        |> Array.tryFind (fun evaluation ->
            evaluation.Value = System.Double.NegativeInfinity
            )
        |> function
            | None -> Ok evaluations
            | Some unbounded ->
                {
                    Status = Unbounded
                    Candidate = unbounded
                    Iterations = 0
                    Simplex =
                        evaluations
                        |> Array.map (fun e -> e.Arguments)
                }
                |> Successful
                |> Error

    // Are all arguments finite?
    let private checkArgumentsFinite (evaluations: Evaluation []) =
        evaluations
        // are all the arguments finite?
        |> Array.forall (fun evaluation ->
            evaluation.Arguments
            |> Array.forall System.Double.IsFinite
            )
        |> function
            | false ->
                {
                    Message = "Invalid initial Simplex: all arguments must be finite"
                    Simplex =
                        evaluations
                        |> Array.map (fun e -> e.Arguments)
                }
                |> Abnormal
                |> Error
            | true ->
                evaluations
                |> Ok

    let private checkFiniteObjective (evaluations: Evaluation []) =
        evaluations
        |> Array.forall (fun evaluation ->
            System.Double.IsFinite evaluation.Value
            )
        |> function
            | false ->
                {
                    Message = "Invalid initial Simplex: all objective values must be finite"
                    Simplex =
                        evaluations
                        |> Array.map (fun e -> e.Arguments)
                }
                |> Abnormal
                |> Error
            | true ->
                evaluations
                |> Ok

    // Verify that the initial simplex is well-formed
    let preCheck (objective: IVectorFunction) simplex: Result<Evaluation [], SolverResult> =

        simplex
        |> evaluateObjective objective
        |> Result.bind checkUnboundedObjective
        |> Result.bind checkArgumentsFinite
        |> Result.bind checkFiniteObjective