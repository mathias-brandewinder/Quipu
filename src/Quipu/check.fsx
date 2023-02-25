#load "NelderMead.fs"
open Quipu

NelderMead.solve 0.01 (1, fun x -> pown x.[0] 2) [| 100.0 |]