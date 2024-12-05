namespace Quipu

type IObjective =
    abstract member Dimension: int
    abstract member Value: float [] -> float

type Objective () =
    static member from (f: float -> float) =
        { new IObjective with
            member this.Dimension = 1
            member this.Value x = f(x.[0])
        }
    static member from (f: (float * float) -> float) =
        { new IObjective with
            member this.Dimension = 2
            member this.Value x = f (x.[0], x.[1])
        }
    static member from (f: (float * float * float) -> float) =
        { new IObjective with
            member this.Dimension = 3
            member this.Value x = f (x.[0], x.[1], x.[2])
        }
    static member from (dim: int, f: float[] -> float) =
        { new IObjective with
            member this.Dimension = dim
            member this.Value x = f x
        }