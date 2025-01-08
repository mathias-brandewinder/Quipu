#r "nuget: Quipu, 0.5.2"
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
