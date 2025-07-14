namespace Quipu

module Validation =

    // Verify that the initial simplex is well-formed
    let preCheck (objective: IVectorFunction) simplex: Result<Evaluation [], SolverResult> =

        // are all arguments finite?
        let checkArgumentsFinite (simplex: float[][]) =
            simplex
            // are all the arguments finite?
            |> Array.forall (fun args ->
                args
                |> Array.forall System.Double.IsFinite
                )
            |> function
                | false ->
                    {
                        Message = "Invalid initial Simplex: all arguments must be finite"
                        Simplex = simplex
                    }
                    |> Abnormal
                    |> Error
                | true ->
                    simplex
                    |> Ok

        // does the objective throw
        let checkFunctionEvaluation (simplex: float[][]) =
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

        let checkUnboundedObjective (evaluations: Evaluation []) =
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
                        Simplex = simplex
                    }
                    |> Successful
                    |> Error

        let checkFiniteObjective (evaluations: Evaluation []) =
            evaluations
            |> Array.forall (fun evaluation ->
                System.Double.IsFinite evaluation.Value
                )
            |> function
                | false ->
                    {
                        Message = "Invalid initial Simplex: all objective values must be finite"
                        Simplex = simplex
                    }
                    |> Abnormal
                    |> Error
                | true ->
                    evaluations
                    |> Ok

        simplex
        |> checkArgumentsFinite
        |> Result.bind checkFunctionEvaluation
        |> Result.bind checkUnboundedObjective
        |> Result.bind checkFiniteObjective