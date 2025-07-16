# Quipu

**Quipu** is a dotnet implementation of the 
[Nelder–Mead method](https://en.wikipedia.org/wiki/Nelder%E2%80%93Mead_method). 
It is a numerical solver used to find the minimum or maximum of a function. It 
is particularly useful for nonlinear optimization problems for which 
derivatives may not be known.  

[![NuGet Version](https://img.shields.io/nuget/v/Quipu)](https://www.nuget.org/packages/Quipu)
![GitHub Actions Workflow Status](https://img.shields.io/github/actions/workflow/status/mathias-brandewinder/Quipu/build-and-test.yml)
[![F#](https://img.shields.io/badge/F%23-378BBA?logo=fsharp&logoColor=fff)](#)

## Example usage

The solver can be used from F# and C#, with similar APIs.  

Let's use Quipu to find the minimum of `f(x,y) = (x-1)^2 + (y-2)^2 + 42`.  

This function has a unique global minimum, for `x=1,y=2`.  

### Basic usage, F# pipeline

``` fsharp
#r "nuget: Quipu, 1.0.0"
open Quipu

let f (x, y) = pown (x - 1.0) 2 + pown (y - 2.0) 2 + 42.0

let solverResult =
    NelderMead.objective f
    |> NelderMead.minimize

if solverResult.HasSolution
then
    let solution = solverResult.Solution
    printfn $"Solution: {solution.Status}"
    let candidate = solution.Candidate
    let args = candidate.Arguments
    let value = candidate.Value
    printfn $"f(%.3f{args[0]}, %.3f{args[1]}) = %.3f{value}"
```

```
Solution: Optimal
f(1.000, 2.000) = 42.000
```

### Basic usage, C# fluent interface  

``` csharp
#r "nuget: Quipu, 1.0.0"
using Quipu.CSharp;
using System;

Func<Double,Double,Double> f =
    (x, y) => Math.Pow(x - 1.0, 2) + Math.Pow(y - 2.0, 2) + 42.0;

var solverResult =
    NelderMead
        .Objective(f)
        .Minimize();

if (solverResult.HasSolution)
{
    var solution = solverResult.Solution;
    Console.WriteLine($"Solution: {solution.Status}");
    var candidate = solution.Candidate;
    var args = candidate.Arguments;
    var value = candidate.Value;
    Console.WriteLine($"f({args[0]:N3}, {args[1]:N3}) = {value:N3}");
}
```

```
Solution: Optimal
f(1.000, 2.000) = 42.000
```

## Advanced usage

The solver provides more fine grained control if needed, see the test suite 
for more examples:  

``` fsharp
open Quipu

let f (x, y) = pown (x - 1.0) 2 + pown (y - 2.0) 2 + 42.0

let tolerance = 0.000_0001

let solverResult =
    NelderMead.objective f
    |> NelderMead.withTolerance 0.000_0001
    |> NelderMead.startFrom (Start.around [ 100.0; 100.0])
    |> NelderMead.minimize
```
