namespace Quipu.Tests

module PreCheck =

    open Expecto
    open Quipu
    open Quipu.Algorithm

    [<Tests>]
    let tests =

        // better tests would check the type of exception with throwsT,
        // but this requires making the exceptions public, which I do not like.
        // Good enough for now.
        testList "validation of initial simplex" [

            test "an initial simplex containing nan should produce Abnormal solution" {

                let objective =
                    fun (x, y) -> x + y
                    |> Vectorize.from

                let simplex =
                    [|
                        [| 0.0; 1.0 |]
                        [| 1.0; nan |]
                    |]

                let actual = preCheck objective simplex
                match actual with
                | Error result ->
                    Expect.isTrue (result.IsAbnormal) "Invalid initial Simplex"
                | Ok _ -> failwith "simplex should not pass pre-check"
                }

            test "an initial simplex evaluating to -infinity should throw" {

                let objective =
                    fun (x, y) -> - infinity
                    |> Vectorize.from

                let simplex =
                    [|
                        [| 0.0; 1.0 |]
                        [| 1.0; 0.0 |]
                    |]

                let actual = preCheck objective simplex
                match actual with
                | Error result ->
                    Expect.isTrue (result.Solution.Status = Status.Unbounded) "Early exit: unbounded problem"
                | Ok _ -> failwith "simplex should not pass pre-check"
                }

            test "an initial simplex evaluating to infinity should throw" {

                let objective =
                    fun (x, y) -> infinity
                    |> Vectorize.from

                let simplex =
                    [|
                        [| 0.0; 1.0 |]
                        [| 1.0; 0.0 |]
                    |]

                let actual = preCheck objective simplex
                match actual with
                | Error result ->
                    Expect.isTrue (result.IsAbnormal) "Invalid initial Simplex"
                | Ok _ -> failwith "simplex should not pass pre-check"
                }

            test "an initial simplex evaluating to nan should throw" {

                let objective =
                    fun (x, y) -> nan
                    |> Vectorize.from

                let simplex =
                    [|
                        [| 0.0; 1.0 |]
                        [| 1.0; 0.0 |]
                    |]

                let actual = preCheck objective simplex
                match actual with
                | Error result ->
                    Expect.isTrue (result.IsAbnormal) "Invalid initial Simplex"
                | Ok _ -> failwith "simplex should not pass pre-check"
                }
            ]