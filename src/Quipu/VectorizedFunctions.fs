namespace Quipu

/// Represents a function that accepts an array of System.Double as arguments,
/// of known size (Dimension), and returns a System.Double.
type IVectorFunction =
    abstract member Dimension: int
    abstract member Value: float [] -> float

/// Value of a function, evaluated for an array of Arguments.
type Evaluation = {
    Arguments: float []
    Value: float
    }
    with
    member this.IsInfeasible =
        System.Double.IsNaN (this.Value)
    member this.IsFeasible =
        not (this.IsInfeasible)

/// Utilities to convert methods or functions into an IVectorFunction.
type Vectorize () =

    static member from (f: float -> float) =
        { new IVectorFunction with
            member this.Dimension = 1
            member this.Value x = f(x.[0])
        }
    static member from (f: (float * float) -> float) =
        { new IVectorFunction with
            member this.Dimension = 2
            member this.Value x = f (x.[0], x.[1])
        }
    static member from (f: (float * float * float) -> float) =
        { new IVectorFunction with
            member this.Dimension = 3
            member this.Value x = f (x.[0], x.[1], x.[2])
        }
    static member from (f: float -> float -> float) =
        { new IVectorFunction with
            member this.Dimension = 2
            member this.Value x = f x.[0] x.[1]
        }
    static member from (f: float -> float -> float -> float) =
        { new IVectorFunction with
            member this.Dimension = 3
            member this.Value x = f x.[0] x.[1] x.[2]
        }
    static member from (dim: int, f: float[] -> float) =
        { new IVectorFunction with
            member this.Dimension = dim
            member this.Value x = f x
        }
    static member negate (objective: IVectorFunction) =
        { new IVectorFunction with
            member this.Dimension: int =
                objective.Dimension
            member this.Value(x: float array): float =
                - (objective.Value x)
        }