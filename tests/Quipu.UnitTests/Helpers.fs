namespace Quipu.Tests

open Expecto

module Expect =

    /// Replacement for Expect.floatClose, where I find Accuracy confusing.
    let isWithin (margin: float) (actual: float) (expected: float) (msg: string) =
        let difference = abs (actual - expected)
        Expect.isTrue (difference <= margin) msg