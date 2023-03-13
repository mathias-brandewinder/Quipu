module Quipu.Tests

open System
open Xunit
open Quipu.NelderMead

let isWithin (target: float, tolerance: float) (value: float) =
    value <= target + tolerance
    &&
    value >= target - tolerance

module BasicConvergence =

    type TestClass() =
        member this.OneParameter(x: float) = pown x 2
        member this.TwoParameters(x: float, y: float) = pown x 2 + pown y 2
        member this.ThreeParameters(x: float, y: float, z: float) = pown x 2 + pown y 2 + pown z 2
        static member StaticOne(x: float) = pown x 2

    [<Fact>]
    let ``function, 1 argument`` () =

        let f x = pown x 2
        let (_, value) = NelderMead.solve Configuration.defaultValue 0.01 (Objective.from f) [ 100.0 ]
        Assert.True(value |> isWithin (0.0, 0.01))

    [<Fact>]
    let ``function, tuple`` () =

        let f (x, y) = pown x 2 + pown y 2
        let (_, value) = NelderMead.solve Configuration.defaultValue 0.01 (Objective.from f) [ 100.0; 100.0 ]
        Assert.True(value |> isWithin (0.0, 0.01))

    [<Fact>]
    let ``function, truple`` () =

        let f (x, y, z) = pown x 2 + pown y 2 + pown z 2
        let (_, value) = NelderMead.solve Configuration.defaultValue 0.01 (Objective.from f) [ 100.0; 100.0; 100.0 ]
        Assert.True(value |> isWithin (0.0, 0.01))

    [<Fact>]
    let ``function, array of arguments`` () =

        let f (x: float []) = pown x.[0] 2 + pown x.[1] 2
        let (_, value) = NelderMead.solve Configuration.defaultValue 0.01 (Objective.from (2, f)) [ 100.0; 100.0 ]
        Assert.True(value |> isWithin (0.0, 0.01))

    [<Fact>]
    let ``method, 1 argument`` () =

        let testClass = TestClass()
        let (_, value) = NelderMead.solve Configuration.defaultValue 0.01 (Objective.from testClass.OneParameter) [ 100.0 ]
        Assert.True(value |> isWithin (0.0, 0.01))

    [<Fact>]
    let ``method, 2 arguments`` () =

        let testClass = TestClass()
        let (_, value) = NelderMead.solve Configuration.defaultValue 0.01 (Objective.from testClass.TwoParameters) [ 100.0; 100.0 ]
        Assert.True(value |> isWithin (0.0, 0.01))

    [<Fact>]
    let ``method, 3 arguments`` () =

        let testClass = TestClass()
        let (_, value) = NelderMead.solve Configuration.defaultValue 0.01 (Objective.from testClass.ThreeParameters) [ 100.0; 100.0; 100.0 ]
        Assert.True(value |> isWithin (0.0, 0.01))

    [<Fact>]
    let ``static method, 1 argument`` () =

        let (_, value) = NelderMead.solve Configuration.defaultValue 0.01 (Objective.from TestClass.StaticOne) [ 100.0 ]
        Assert.True(value |> isWithin (0.0, 0.01))