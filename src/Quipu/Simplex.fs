namespace Quipu

// https://en.wikipedia.org/wiki/Simplex
type Simplex =
    private | Vectors of dim: int * vectors: float [][]
    with

    member this.dimension =
        match this with
        | Vectors (dim, _) -> dim

    member this.size =
        match this with
        | Vectors (_, vectors) -> vectors.Length

    static member vertices (this: Simplex) =
        match this with
        | Vectors (_, vectors) ->
            vectors
            |> Array.copy

    /// Create a Simplex centered on the origin point, with a set outer radius.
    static member create (origin: float[], radius: float) =
        // Source:
        // https://en.wikipedia.org/wiki/Simplex#Cartesian_coordinates_for_a_regular_n-dimensional_simplex_in_Rn
        let dim = origin.Length
        // Coordinates of the last, non-basis vertex
        let lastValue = (1.0 / (float dim)) * (1.0 + (sqrt (float (dim + 1))))
        // Barycenter of the original simplex
        let barycenter = (1.0 + lastValue) / (float (dim + 1))
        // Radius of the original simplex
        let initialRadius = sqrt (float dim / float (2 * (dim + 1))) * sqrt 2.0
        // Scaling factor to desired radius
        let scale = radius / initialRadius
        let vectors =
            Array.init (dim + 1) (
                fun i ->
                    if i < dim
                    then
                        // n first basis vectors
                        Array.init dim (fun j ->
                            if j = i
                            then 1.0
                            else 0.0
                            )
                    else
                        // last, non-basis vector
                        Array.init dim (fun _ -> lastValue)
                    |> Array.mapi (fun i x ->
                        // scale and translate every coordinate
                        scale * (x - barycenter) + origin.[i]
                        )
                    )
        Vectors (dim, vectors)

    /// Create a Simplex centered on the origin point, with a radius of 1.0
    static member create (origin: float[]) =
        Simplex.create (origin, 1.0)

    /// Create a Simplex centered on 0.0, with a radius of 1.0
    static member create (dim: int) =
        let origin = Array.init dim (fun _ -> 0.0)
        Simplex.create origin

    /// Create a Simplex centered on 0.0, with a set outer radius.
    static member create (dim: int, radius: float) =
        let origin = Array.init dim (fun _ -> 0.0)
        Simplex.create (origin, radius)