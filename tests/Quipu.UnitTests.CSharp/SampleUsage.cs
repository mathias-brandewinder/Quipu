namespace Quipu.Tests.CSharp;

using Quipu.CSharp;

public class CSharpFluentInterfaceDemo
{
    [Fact]
    public void Demo_SimpleSetup()
    {
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
    }

    [Fact]
    public void Demo_AdvancedSetup()
    {
        Func<Double,Double,Double> f =
            (x, y) => Math.Pow(x - 1.0, 2) + Math.Pow(y - 2.0, 2);

        var tolerance = 0.001;

        var result =
            NelderMead
                .Objective(f)
                .WithMaximumIterations(100)
                .WithTolerance(tolerance)
                .StartFrom(Start.Around(new double[] { 100.0, 100.0 }))
                .Minimize();

        Assert.True(result.HasSolution);
        var solution = result.Solution;
        Assert.Equal(Status.Optimal, solution.Status);

        Assert.Equal(0.0, solution.Candidate.Value, tolerance);
        Assert.Equal(1.0, solution.Candidate.Arguments[0], tolerance);
        Assert.Equal(2.0, solution.Candidate.Arguments[1], tolerance);
    }
}