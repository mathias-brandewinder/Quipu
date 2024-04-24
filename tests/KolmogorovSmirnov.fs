namespace Quipu.Tests

// module KolmogorovSmirnov =

//     open System
//     open Xunit
//     open FsCheck
//     open FsCheck.Xunit
//     open Quipu.KolmogorovSmirnov

//     type IDistribution =
//         abstract member Simulate : unit -> float
//         abstract member Cumulative : float -> float

//     let uniform (rng: Random) (lower, upper) =
//         let diff = upper - lower
//         { new IDistribution with
//             member this.Cumulative(arg1: float): float =
//                 (arg1 - lower) / diff
//                 |> min 1.0
//                 |> max 0.0
//             member this.Simulate(): float =
//                 diff * rng.NextDouble() + lower
//         }

//     let sample n (density: unit -> float) =
//         Array.init n (fun _ -> density())

//     module Samples =

//         [<Theory>]
//         [<InlineData(0.20, 1.073)>]
//         [<InlineData(0.15, 1.138)>]
//         [<InlineData(0.10, 1.224)>]
//         [<InlineData(0.05, 1.358)>]
//         [<InlineData(0.025, 1.48)>]
//         [<InlineData(0.01, 1.628)>]
//         [<InlineData(0.005, 1.731)>]
//         [<InlineData(0.001, 1.949)>]
//         // See https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test#Two-sample_Kolmogorov%E2%80%93Smirnov_test
//         let ``c(alpha)`` (alpha, expected) =

//             let actual = Samples.c alpha
//             Assert.InRange(actual, expected - 0.001, expected + 0.001)

//         [<Fact>]
//         let ``identical samples should have a difference of 0.0`` () =

//             let sample = [| 0.0; 1.0; 2.0 |]
//             Assert.Equal(0.0, Samples.maximumDifference(sample, sample))

//         [<Fact>]
//         let ``max difference should return largest observed CDF difference`` () =

//             let sample1 = [| 0.0; 1.0; 2.0; 3.0 |]
//             let sample2 = [| 0.0; 1.0; 1.0; 3.0 |]
//             Assert.Equal(0.25, Samples.maximumDifference(sample1, sample2))

//         [<Fact>]
//         let ``max difference of non overlapping functions should be 1.0`` () =

//             let sample1 = [| 0.0; 1.0; 2.0; 3.0 |]
//             let sample2 = [| 4.0; 5.0; 6.0; 7.0 |]
//             Assert.Equal(1.0, Samples.maximumDifference(sample1, sample2))

//         [<Fact>]
//         let ``difference with repeats, case 1`` () =

//             let sample1 = [| 0.0; 1.0; 2.0; 3.0; 4.0 |]
//             let sample2 = [| 0.0; 1.0; 1.0; 1.0; 4.0 |]
//             Assert.Equal(0.4, Samples.maximumDifference(sample1, sample2), 4)

//         [<Fact>]
//         let ``difference with repeats, case 2`` () =

//             let sample1 = [| 0.0; 1.0; 1.0; 3.0; 4.0 |]
//             let sample2 = [| 0.0; 1.0; 1.0; 1.0; 4.0 |]
//             Assert.Equal(0.2, Samples.maximumDifference(sample1, sample2), 4)

//         [<Fact>]
//         let ``difference with repeats, case 3`` () =

//             let sample1 = [| 0.0; 2.0; 3.0; 4.0; 5.0 |]
//             let sample2 = [| 0.0; 1.0; 1.0; 1.0; 4.0 |]
//             Assert.Equal(0.6, Samples.maximumDifference(sample1, sample2), 4)

//         [<Property>]
//         let ``estimated alpha for critical error should match alpha used to generate error `` (alpha: NormalFloat, size1: PositiveInt, size2: PositiveInt) =

//             let alpha = alpha.Get
//             let size1 = size1.Get
//             let size2 = size2.Get

//             let alpha =
//                 alpha
//                 |> abs
//                 |> fun x -> x - floor x

//             // compute the critical value for a level alpha
//             let c = Samples.c alpha
//             let correction = Samples.sampleCorrection (size1, size2)
//             let critical = c * correction
//             // compute estimated alpha for the observed difference
//             let estimated = Samples.findAlpha critical (size1, size2)

//             estimated < alpha + 0.001 && alpha - 0.001 < estimated

//         [<Fact>]
//         let ``test accuracy`` () =

//             let tests = 100
//             let sampleSize = 100

//             let samples =
//                 [ 1 .. tests ]
//                 |> List.map (fun seed ->
//                     let uniform1 = uniform (Random seed) (0.0, 100.0)
//                     let uniform2 = uniform (Random (seed + 1)) (0.0, 100.0)
//                     let sample1 = sample sampleSize (uniform1.Simulate)
//                     let sample2 = sample sampleSize (uniform2.Simulate)
//                     Samples.compare (sample1, sample2)
//                     )

//             let alphas = [ 0.1; 0.2; 0.3; 0.4; 0.5 ]

//             alphas
//             |> List.iter (fun alpha ->
//                 let failures =
//                     samples
//                     |> List.filter (fun test -> test.ProbaDifferenceIfEqual < alpha)
//                     |> List.length
//                 let limit = alpha + 0.01
//                 Assert.True(failures <= int (limit * float sampleSize))
//                 )