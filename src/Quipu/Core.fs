namespace Quipu

type Status =
    | Optimal
    | Suboptimal
    | Unbounded

type Solution = {
    Status: Status
    Candidate: Candidate
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
    Objective: IObjective
    Configuration: Configuration
    StartingPoint: IStartingPoint
    }
    with
    member this.Dimension =
        this.Objective.Dimension
    static member defaultCreate(objective: IObjective) =
        {
            Objective = objective
            StartingPoint = Start.zero
            Configuration = Configuration.defaultValue
        }