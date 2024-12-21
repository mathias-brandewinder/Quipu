namespace Quipu.Tests

module Simplex =

    open Expecto
    open Quipu

    [<Tests>]
    let tests =
        testList "simplex tests" [
            test "simplex should be centered on origin" {

                let origin = [| 10.0; 5.0; -20.0 |]
                let radius = 3.0
                let simplex = Simplex.create(origin, radius)

                let dims = [| 0 .. simplex.dimension - 1 |]
                let expected =
                    dims
                    |> Array.map (fun dim ->
                        simplex
                        |> Simplex.vertices
                        |> Array.averageBy (fun vertex -> vertex.[dim])
                        )

                dims
                |> Array.iter (fun dim ->
                    Expect.equal expected.[dim] origin.[dim] ""
                    )
                }

            test "simplex should have the requested radius" {

                let origin = [| 10.0; 5.0; -20.0 |]
                let radius = 3.0
                let simplex = Simplex.create(origin, radius)

                let distance xs ys =
                    (xs, ys)
                    ||> Array.map2 (fun x y -> pown (x - y) 2)
                    |> Array.sum
                    |> sqrt

                simplex
                |> Simplex.vertices
                |> Array.iter (fun vertex ->
                    let d = distance vertex origin
                    Expect.isWithin 0.001 radius d $"simplex vertices should be {radius} away from center, actual: {d}"
                    )
                }
            ]