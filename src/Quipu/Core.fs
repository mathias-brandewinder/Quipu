namespace Quipu

type Status =
    | Optimal
    | Suboptimal
    | Unbounded

type Solution = {
    Status: Status
    Candidate: Evaluation
    Simplex: float [][]
    }

type SolverResult =
    | Successful of Solution
    | Abnormal of (float [][])
    with
    member this.HasSolution =
        match this with
        | Successful _ -> true
        | Abnormal _ -> false
    member this.Solution =
        match this with
        | Successful solution -> solution
        | Abnormal _ -> failwith "No solution found."

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