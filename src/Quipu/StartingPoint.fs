namespace Quipu

type IStartingPoint =
    abstract member create: int -> float[][]

type Start =

    static member zero =
        { new IStartingPoint with
            member this.create(dim: int): float array array =
                Simplex.create dim
                |> Simplex.vertices
        }

    static member around (startingPoint: seq<float>) =
        { new IStartingPoint with
            member this.create (dim: int): float[][] =
                let startingPoint = startingPoint |> Array.ofSeq
                if startingPoint.Length <> dim
                then failwith $"Invalid starting point dimension: {startingPoint.Length}, expected {dim}."
                Simplex.create startingPoint
                |> Simplex.vertices
        }

    static member around (startingPoint: seq<float>, radius: float) =
        { new IStartingPoint with
            member this.create (dim: int): float[][] =
                let startingPoint = startingPoint |> Array.ofSeq
                if startingPoint.Length <> dim
                then failwith $"Invalid starting point dimension: {startingPoint.Length}, expected {dim}."
                Simplex.create (startingPoint, radius)
                |> Simplex.vertices
        }

    static member around (startingPoint: float) =
        { new IStartingPoint with
            member this.create (dim: int): float[][] =
                let startingPoint = Array.singleton startingPoint
                if startingPoint.Length <> dim
                then failwith $"Invalid starting point dimension: {startingPoint.Length}, expected {dim}."
                Simplex.create startingPoint
                |> Simplex.vertices
        }

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
