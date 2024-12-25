namespace Quipu.Tests

module PartialSort =

    open Expecto
    open Quipu

    // We generate an array of Candidate [], of size 2 to 20, with random Value.
    let testCases () =
        let rng = System.Random 0
        Array.init 50 (fun i ->
            let size = rng.Next(2, 20)
            i,
            Array.init size (fun _ ->
                { Value = rng.NextDouble(); Arguments = Array.empty}
                )
            )

    [<Tests>]
    let tests =
        testList "partial sort candidates" [

            // This should probably be turned into a property based test.
            // Good enough for now :)
            testTheory "test cases" (testCases ()) <|
                fun (i, candidates) ->
                    let manual = candidates |> Array.sortBy (fun c -> c.Value)
                    let sorted = Algorithm.sortInPlace candidates

                    let last = candidates.Length - 1

                    Expect.equal sorted[0].Value manual[0].Value "same best"
                    Expect.equal sorted[last].Value manual[last].Value "same worst"
                    Expect.equal sorted[last - 1].Value manual[last - 1].Value "same second worst"
            ]