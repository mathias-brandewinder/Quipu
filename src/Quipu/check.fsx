#load "NelderMead.fs"
open Quipu
open Quipu.NelderMead

let objective = {
    new IObjective with
        member this.Dimension = 1
        member this.Value x = pown x.[0] 2
    }

let solution = NelderMead.solve Configuration.defaultValue 0.01 objective [| 100.0 |]