#load "NelderMead.fs"
open Quipu
open Quipu.NelderMead

let solution = NelderMead.solve Configuration.defaultValue 0.01 (1, fun x -> pown x.[0] 2) [| 100.0 |]