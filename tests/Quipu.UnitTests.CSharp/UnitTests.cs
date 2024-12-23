namespace Quipu.Tests.CSharp;

using Quipu.CSharp;

public class UnitTests
{
    public double OneParameter(double x)
    {
        return
            System.Math.Pow(x - 1.0, 2)
            +
            10.0;
    }

    public double TwoParameters(double x0, double x1)
    {
        return
            System.Math.Pow(x0 - 1.0, 2)
            +
            System.Math.Pow(x1 - 2.0, 2)
            +
            10.0;
    }

    public double ThreeParameters(double x0, double x1, double x2)
    {
        return
            System.Math.Pow(x0 - 1.0, 2)
            +
            System.Math.Pow(x1 - 2.0, 2)
            +
            System.Math.Pow(x1 - 3.0, 2)
            +
            10.0;
    }

    [Fact]
    public void Test_One_Parameter()
    {
        var result =
            NelderMead
                .Objective(this.OneParameter)
                .WithMaximumIterations(100)
                .WithTolerance(0.001)
                .StartFrom(Start.Around(100.0))
                .Minimize();

        Assert.True(result.HasSolution);
        var solution = result.Solution;
        Assert.Equal(Status.Optimal, solution.Status);
    }

    [Fact]
    public void Test_Two_Parameters()
    {
        var result =
            NelderMead
                .Objective(this.TwoParameters)
                .WithMaximumIterations(100)
                .WithTolerance(0.001)
                .StartFrom(Start.Around(new double[] { 100.0, 100.0 }))
                .Minimize();

        Assert.True(result.HasSolution);
        var solution = result.Solution;
        Assert.Equal(Status.Optimal, solution.Status);
    }

    [Fact]
    public void Test_Two_Parameters_Start_From_Pair()
    {
        var result =
            NelderMead
                .Objective(this.TwoParameters)
                .WithMaximumIterations(100)
                .WithTolerance(0.001)
                .StartFrom(Start.Around(100.0, 100.0))
                .Minimize();

        Assert.True(result.HasSolution);
        var solution = result.Solution;
        Assert.Equal(Status.Optimal, solution.Status);
    }

    [Fact]
    public void Test_Three_Parameters()
    {
        var result =
            NelderMead
                .Objective(this.ThreeParameters)
                .WithMaximumIterations(100)
                .WithTolerance(0.001)
                .StartFrom(Start.Around(new double[] { 100.0, 100.0, 100.0 }))
                .Minimize();

        Assert.True(result.HasSolution);
        var solution = result.Solution;
        Assert.Equal(Status.Optimal, solution.Status);
    }

    [Fact]
    public void Test_Three_Parameters_Start_From_Triplet()
    {
        var result =
            NelderMead
                .Objective(this.ThreeParameters)
                .WithMaximumIterations(100)
                .WithTolerance(0.001)
                .StartFrom(Start.Around(100.0, 100.0, 100.0))
                .Minimize();

        Assert.True(result.HasSolution);
        var solution = result.Solution;
        Assert.Equal(Status.Optimal, solution.Status);
    }
}
