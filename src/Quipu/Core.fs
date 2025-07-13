namespace Quipu

type Status =
    /// The algorithm terminated the search, the solution is a local optimum.
    | Optimal
    /// The algorithm did not find an optimal solution within the specified
    /// number of iterations. The best current solution
    /// does not satisfy the termination criteria.
    | Suboptimal
    /// The algorithm found a candidate where the function is unbounded. The
    /// objective function does not have a minimum.
    | Unbounded

type Solution = {
    /// Indicates whether the best candidate is (locally) optimal, sub-optimal
    /// (early search termination) or unbounded.
    Status: Status
    /// Number of search iterations performed.
    Iterations: int
    /// Best combination of arguments found for the target function.
    Candidate: Evaluation
    /// Final Simplex.
    Simplex: float [][]
    }

type AbnormalSimplex = {
    Message: string
    Simplex: float [][]
    }

/// <summary>
/// Result of the solver search for arguments that minimize or maximize a
/// function.
/// </summary>
type SolverResult =
    | Successful of Solution
    | Abnormal of AbnormalSimplex
    with
    /// Indicates whether or not the Solver successfully completed its search.
    member this.HasSolution =
        match this with
        | Successful _ -> true
        | Abnormal _ -> false
    /// Best solution found by the solver, if the search terminated
    /// successfully.
    member this.Solution =
        match this with
        | Successful solution -> solution
        | Abnormal _ -> failwith "No solution found."

/// <summary>
/// Description of the function to be minimized, and configuration of the
/// search parameters.
/// </summary>
type Problem = {
    Objective: IVectorFunction
    Configuration: Configuration
    StartingPoint: IStartingPoint
    }
    with
    member this.Dimension =
        this.Objective.Dimension
    static member defaultCreate(objective: IVectorFunction) =
        {
            Objective = objective
            StartingPoint = Start.zero
            Configuration = Configuration.defaultValue
        }