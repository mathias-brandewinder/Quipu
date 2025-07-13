namespace Quipu

/// <summary>
/// Strategy to create a starting point for the search algorithm. To optimize
/// a n-dimensional function (a function that takes n arguments), the search
/// algorithm requires at least n+1 distinct sets of arguments as starting
/// values.
/// <para>
/// Start has multiple built-in implementations covering typical scenarios such
/// as starting around a given value.
/// </para>
/// </summary>
type IStartingPoint =
    /// Given a dimension n, create an array of n+1 arrays of n floats.
    abstract member create: int -> float[][]

/// <summary>
/// Contains functions to help create starting points for the search.
/// </summary>
type Start =

    static member zero =
        { new IStartingPoint with
            member this.create(dim: int): float array array =
                Simplex.create dim
                |> Simplex.vertices
        }

    [<CompiledName("Around")>]
    static member around (startingPoint: seq<float>) =
        { new IStartingPoint with
            member this.create (dim: int): float[][] =
                let startingPoint = startingPoint |> Array.ofSeq
                if startingPoint.Length <> dim
                then failwith $"Invalid starting point dimension: {startingPoint.Length}, expected {dim}."
                Simplex.create startingPoint
                |> Simplex.vertices
        }

    [<CompiledName("Around")>]
    static member around (startingPoint: seq<float>, radius: float) =
        { new IStartingPoint with
            member this.create (dim: int): float[][] =
                let startingPoint = startingPoint |> Array.ofSeq
                if startingPoint.Length <> dim
                then failwith $"Invalid starting point dimension: {startingPoint.Length}, expected {dim}."
                Simplex.create (startingPoint, radius)
                |> Simplex.vertices
        }

    [<CompiledName("Around")>]
    static member around (startingPoint: float) =
        { new IStartingPoint with
            member this.create (dim: int): float[][] =
                let startingPoint = Array.singleton startingPoint
                if startingPoint.Length <> dim
                then failwith $"Invalid starting point dimension: {startingPoint.Length}, expected {dim}."
                Simplex.create startingPoint
                |> Simplex.vertices
        }

    [<CompiledName("Around")>]
    static member around (x: float, y: float) =
        { new IStartingPoint with
            member this.create (dim: int): float[][] =
                let startingPoint = [| x; y |]
                if startingPoint.Length <> dim
                then failwith $"Invalid starting point dimension: {startingPoint.Length}, expected {dim}."
                Simplex.create startingPoint
                |> Simplex.vertices
        }

    [<CompiledName("Around")>]
    static member around (x: float, y: float, z: float) =
        { new IStartingPoint with
            member this.create (dim: int): float[][] =
                let startingPoint = [| x; y; z |]
                if startingPoint.Length <> dim
                then failwith $"Invalid starting point dimension: {startingPoint.Length}, expected {dim}."
                Simplex.create startingPoint
                |> Simplex.vertices
        }

    [<CompiledName("At")>]
    static member at (startingPoint: #seq<#seq<float>>) =
        { new IStartingPoint with
            member this.create (dim: int): float[][] =
                let startingPoint =
                    startingPoint
                    |> Seq.map (Array.ofSeq)
                    |> Array.ofSeq
                    |> Array.distinct
                startingPoint
                |> Array.iter (fun point ->
                    if point.Length <> dim
                    then
                        failwith $"Invalid starting point dimension: {point.Length}, expected {dim}."
                    )
                if startingPoint.Length <= dim
                then failwith $"Invalid starting simplex size: {startingPoint.Length}, expected {dim + 1}."
                startingPoint
        }
