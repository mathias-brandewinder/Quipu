# Quipu

## Nelder Mead

Implementation of a [Nelder-Mead function minimizer](https://en.wikipedia.org/wiki/Nelder%E2%80%93Mead_method).

Basic usage: find x and y that minimize sin(x) + cos(y)

``` fsharp
open Quipu.NelderMead

let f (x, y) = sin x + cos y

NelderMead.minimize f
|> NelderMead.solve
```

Solution:

```
val it: Quipu.NelderMead.Solution =
  Optimal (-1.99999997, [|-1.570753552; 3.141352485|])
```

The solver provides more fine grained control if needed, see the test suite 
for more examples:

``` fsharp
open Quipu
open Quipu.NelderMead

let g (x, y) = pown x 2 + pown y 2

let solution =
    NelderMead.minimize g
    |> NelderMead.withConfiguration Configuration.defaultValue
    |> NelderMead.startFrom (StartingPoint.fromValue [ 100.0; 100.0 ])
    |> NelderMead.solve
```

```
val solution: Solution =
  Optimal (5.420777773e-08, [|-9.201898552e-05; 0.0002138697829|])
```