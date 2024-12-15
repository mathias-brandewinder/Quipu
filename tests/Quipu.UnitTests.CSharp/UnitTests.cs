namespace Quipu.Tests.CSharp;

using Quipu;

public class CSharpFluentInterface
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
        var solution =
            NelderMead
                .Objective(this.OneParameter)
                .WithMaximumIterations(10)
                .WithTolerance(0.001)
                .StartFrom(Start.around(100.0))
                .Minimize();

        Assert.True(true);
    }

    [Fact]
    public void Test_Two_Parameters()
    {
        var solution =
            NelderMead
                .Objective(this.TwoParameters)
                .WithMaximumIterations(10)
                .WithTolerance(0.001)
                .StartFrom(Start.around(new double[] { 100.0, 100.0 }))
                .Minimize();

        Assert.True(true);
    }

    [Fact]
    public void Test_Three_Parameters()
    {
        var solution =
            NelderMead
                .Objective(this.ThreeParameters)
                .WithMaximumIterations(10)
                .WithTolerance(0.001)
                .StartFrom(Start.around(new double[] { 100.0, 100.0, 100.0 }))
                .Minimize();

        Assert.True(true);
    }
}
