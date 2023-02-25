#load "NelderMead.fs"
open Quipu
open Quipu.NelderMead

let f x = pown x 2
let solution_f = NelderMead.solve Configuration.defaultValue 0.01 (Objective.from f) [| 100.0 |]

let g (x, y) = pown x 2 + pown y 2
let solution_g = NelderMead.solve Configuration.defaultValue 0.01 (Objective.from g) [| 100.0; 100.0 |]

let h (x, y, z) = pown x 2 + pown y 2 + pown z 2
let solution_h = NelderMead.solve Configuration.defaultValue 0.01 (Objective.from h) [| 100.0; 100.0; 100.0 |]
