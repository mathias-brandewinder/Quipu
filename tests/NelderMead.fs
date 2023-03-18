namespace Quipu.Tests

module NelderMead =

    open Xunit
    open Quipu
    open Quipu.NelderMead

    module BasicConvergence =

        // We test variations of minimize f(x) = x ^ 2,
        // which has a minimum value of 0.0 for x = 0.0.
        let expected = 0.0
        let tolerance = 0.01

        type TestClass() =
            member this.OneParameter(x: float) = pown x 2
            member this.TwoParameters(x: float, y: float) = pown x 2 + pown y 2
            member this.ThreeParameters(x: float, y: float, z: float) = pown x 2 + pown y 2 + pown z 2
            static member StaticOne(x: float) = pown x 2

        [<Fact>]
        let ``function, 1 argument`` () =

            let f x = pown x 2
            let (_, actual) = NelderMead.solve Configuration.defaultValue tolerance (Objective.from f) [ 100.0 ]
            Assert.InRange(actual, 0.0 - tolerance, 0.0 + tolerance)

        [<Fact>]
        let ``function, tuple`` () =

            let f (x, y) = pown x 2 + pown y 2
            let (_, actual) = NelderMead.solve Configuration.defaultValue tolerance (Objective.from f) [ 100.0; 100.0 ]
            Assert.InRange(actual, 0.0 - tolerance, 0.0 + tolerance)

        [<Fact>]
        let ``function, truple`` () =

            let f (x, y, z) = pown x 2 + pown y 2 + pown z 2
            let (_, actual) = NelderMead.solve Configuration.defaultValue tolerance (Objective.from f) [ 100.0; 100.0; 100.0 ]
            Assert.InRange(actual, 0.0 - tolerance, 0.0 + tolerance)

        [<Fact>]
        let ``function, array of arguments`` () =

            let f (x: float []) = pown x.[0] 2 + pown x.[1] 2
            let (_, actual) = NelderMead.solve Configuration.defaultValue tolerance (Objective.from (2, f)) [ 100.0; 100.0 ]
            Assert.InRange(actual, 0.0 - tolerance, 0.0 + tolerance)

        [<Fact>]
        let ``method, 1 argument`` () =

            let testClass = TestClass()
            let (_, actual) = NelderMead.solve Configuration.defaultValue tolerance (Objective.from testClass.OneParameter) [ 100.0 ]
            Assert.InRange(actual, 0.0 - tolerance, 0.0 + tolerance)

        [<Fact>]
        let ``method, 2 arguments`` () =

            let testClass = TestClass()
            let (_, actual) = NelderMead.solve Configuration.defaultValue tolerance (Objective.from testClass.TwoParameters) [ 100.0; 100.0 ]
            Assert.InRange(actual, 0.0 - tolerance, 0.0 + tolerance)

        [<Fact>]
        let ``method, 3 arguments`` () =

            let testClass = TestClass()
            let (_, actual) = NelderMead.solve Configuration.defaultValue tolerance (Objective.from testClass.ThreeParameters) [ 100.0; 100.0; 100.0 ]
            Assert.InRange(actual, 0.0 - tolerance, 0.0 + tolerance)

        [<Fact>]
        let ``static method, 1 argument`` () =

            let (_, actual) = NelderMead.solve Configuration.defaultValue tolerance (Objective.from TestClass.StaticOne) [ 100.0 ]
            Assert.InRange(actual, 0.0 - tolerance, 0.0 + tolerance)

    module Unbounded =

        [<Fact(Skip="TODO")>]
        let ``function, 1 argument`` () =

            let f x = x
            let (_, actual) = NelderMead.solve Configuration.defaultValue 0.01 (Objective.from f) [ 100.0 ]
            ignore ()