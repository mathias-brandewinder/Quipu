# Quipu

**Quipu** is a dotnet implementation of the 
[Nelder–Mead method](https://en.wikipedia.org/wiki/Nelder%E2%80%93Mead_method). 
It is a numerical solver used to find the minimum or maximum of a function. It 
is particularly useful for nonlinear optimization problems for which 
derivatives may not be known.  

## Example usage

Basic usage: find `x` and `y` that minimize `sin(x) + cos(y)`:  

``` fsharp
open Quipu

let f (x, y) = sin x + cos y

NelderMead.objective f
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

let g (x, y) = pown x 2 + pown y 2

let solution =
    NelderMead.objective g
    // modify termination criteria
    |> NelderMead.withConfiguration
        { Configuration.defaultValue with
            Termination = {
                Termination = Termination.tolerance 0.0001
                MaximumIterations = Some 100
                }
        }
    // start search around 100.0, 100.0
    |> NelderMead.startFrom (Start.around [ 100.0; 100.0 ])
    |> NelderMead.solve
```

```
val solution: Solution =
  Optimal (5.420777773e-08, [|-9.201898552e-05; 0.0002138697829|])
```