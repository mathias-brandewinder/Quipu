namespace Quipu.Tests

module Termination =

    open Expecto
    open Quipu

    [<Tests>]
    let tests =
        testList "termination tests" [
            test "when function values are not within bounds termination should not occur" {

                // simplex within bounds, but function values are not
                let tolerance = 0.5
                let terminate = Termination.tolerance tolerance
                let simplex =
                    [|
                        { Point = [| 0.0; 0.0 |]; Value = 0.0 }
                        { Point = [| 1.0; 0.0 |]; Value = tolerance + 0.1 }
                    |]

                let shouldTerminate = terminate.HasTerminated simplex
                Expect.isFalse(shouldTerminate) ""
                }

            test "when argument values are not within bounds termination should not occur" {

                // function values within bounds, but simplex values are not
                let tolerance = 0.5
                let terminate = Termination.tolerance tolerance
                let simplex =
                    [|
                        { Point = [| 0.0; 0.0 |]; Value = 0.0 }
                        { Point = [| tolerance + 0.1; 0.0 |]; Value = 0.1 }
                    |]

                let shouldTerminate = terminate.HasTerminated simplex
                Expect.isFalse(shouldTerminate) ""
                }

            test "when function and argument values are within bounds termination should occur" {

                // function values and simplex values are within bounds
                let tolerance = 0.5
                let terminate = Termination.tolerance tolerance
                let simplex =
                    [|
                        { Point = [| 0.0; 0.0 |]; Value = 0.0 }
                        { Point = [| 0.0 + tolerance / 2.0; 0.0 + tolerance / 2.0 |]; Value = 0.0 + tolerance / 2.0 }
                    |]

                let shouldTerminate = terminate.HasTerminated simplex
                Expect.isTrue(shouldTerminate) ""
                }
        ]