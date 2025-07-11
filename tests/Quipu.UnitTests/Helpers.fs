namespace Quipu.Tests

module Value =

    let isWithin (margin: float) (actual: float) (expected: float) =
        let difference = abs (actual - expected)
        difference <= margin

module Expect =

    open Expecto

    /// Replacement for Expect.floatClose, where I find Accuracy confusing.
    let isWithin (margin: float) (actual: float) (expected: float) (msg: string) =
        Expect.isTrue (Value.isWithin margin actual expected) msg