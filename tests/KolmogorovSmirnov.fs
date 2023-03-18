namespace Quipu.Tests

module KolmogorovSmirnov =

    open Xunit
    open Quipu.KolmogorovSmirnov

    module Samples =

        [<Theory>]
        [<InlineData(0.20, 1.073)>]
        [<InlineData(0.15, 1.138)>]
        [<InlineData(0.10, 1.224)>]
        [<InlineData(0.05, 1.358)>]
        [<InlineData(0.025, 1.48)>]
        [<InlineData(0.01, 1.628)>]
        [<InlineData(0.005, 1.731)>]
        [<InlineData(0.001, 1.949)>]
        // See https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test#Two-sample_Kolmogorov%E2%80%93Smirnov_test
        let ``c(alpha)`` (alpha, expected) =

            let actual = Samples.c alpha
            Assert.InRange(actual, expected - 0.001, expected + 0.001)

        [<Fact>]
        let ``identical samples should have a difference of 0.0`` () =

            let sample = [| 0.0; 1.0; 2.0 |]
            Assert.Equal(0.0, Samples.maximumDifference(sample, sample))

        [<Fact>]
        let ``max difference should return largest observed CDF difference`` () =

            let sample1 = [| 0.0; 1.0; 2.0; 3.0 |]
            let sample2 = [| 0.0; 1.0; 1.0; 3.0 |]
            Assert.Equal(0.25, Samples.maximumDifference(sample1, sample2))

        [<Fact>]
        let ``max difference of non overlapping functions should be 1.0`` () =

            let sample1 = [| 0.0; 1.0; 2.0; 3.0 |]
            let sample2 = [| 4.0; 5.0; 6.0; 7.0 |]
            Assert.Equal(1.0, Samples.maximumDifference(sample1, sample2))

        [<Fact>]
        let ``critical should match`` () =

            let alpha = 0.01
            let c = Samples.c alpha
            let n1 = 100
            let n2 = 50
            let correction = Samples.sampleCorrection (n1, n2)
            let critical = c * correction
            let estimated = Samples.findAlpha critical (n1, n2)
            Assert.InRange(alpha, estimated - 0.001, estimated + 0.001)