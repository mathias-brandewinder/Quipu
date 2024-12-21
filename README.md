# Quipu

**Quipu** is a dotnet implementation of the 
[Nelder–Mead method](https://en.wikipedia.org/wiki/Nelder%E2%80%93Mead_method). 
It is a numerical solver used to find the minimum or maximum of a function. It 
is particularly useful for nonlinear optimization problems for which 
derivatives may not be known.  

## Example usage

The solver can be used from F# and C#, with similar APIs.  

Let's use Quipu to find the minimum of `f(x,y) = (x-1)^2 + (y-2)^2 + 42`.  

This function has a unique global minimum, for `x=1,y=2`.  

### Basic usage, F# pipeline

``` fsharp
open Quipu

let f (x, y) = pown (x - 1.0) 2 + pown (y - 2.0) 2 + 42.0

let solverResult =
    NelderMead.objective f
    |> NelderMead.minimize

Expect.isTrue (solverResult.HasSolution) "should have a solution"
let solution = solverResult.Solution
Expect.isTrue (solution.Status = Status.Optimal) "should be optimal"
Expect.isWithin 0.001 solution.Candidate.Value 42.0 "minimum should be at 42"
Expect.isWithin 0.001 solution.Candidate.Arguments[0] 1.0 "x should be at 1"
Expect.isWithin 0.001 solution.Candidate.Arguments[1] 2.0 "y should be at 2"
```

### Basic usage, C# fluent interface  

``` csharp
using Quipu.CSharp;

Func<Double,Double,Double> f =
    (x, y) => Math.Pow(x - 1.0, 2) + Math.Pow(y - 2.0, 2) + 42.0;

var result =
    NelderMead
        .Objective(f)
        .Minimize();

Assert.True(result.HasSolution);
var solution = result.Solution;
Assert.Equal(Status.Optimal, solution.Status);

Assert.Equal(42.0, solution.Candidate.Value, 0.001);
Assert.Equal(1.0, solution.Candidate.Arguments[0], 0.001);
Assert.Equal(2.0, solution.Candidate.Arguments[1], 0.001);
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
    |> NelderMead.withTolerance tolerance
    |> NelderMead.startFrom (Start.around [ 100.0; 100.0])
    |> NelderMead.minimize

Expect.isTrue (solverResult.HasSolution) "should have a solution"
let solution = solverResult.Solution
Expect.isTrue (solution.Status = Status.Optimal) "should be optimal"
Expect.isWithin tolerance solution.Candidate.Value 42.0 "minimum should be at 42"
Expect.isWithin tolerance solution.Candidate.Arguments[0] 1.0 "x should be at 1"
Expect.isWithin tolerance solution.Candidate.Arguments[1] 2.0 "y should be at 2"
```
