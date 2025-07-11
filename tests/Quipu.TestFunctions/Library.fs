namespace Quipu.Tests

module Functions =

    // See https://en.wikipedia.org/wiki/Test_functions_for_optimization

    let beale (x, y) =
        pown (1.5 - x + (x * y)) 2
        +
        pown (2.25 - x + (x * pown y 2)) 2
        +
        pown (2.625 - x + x * pown y 3) 2

    let booth (x, y) =
        pown (x + 2.0 * y - 7.0) 2
        +
        pown (2.0 * x + y - 5.0) 2

    let bukin (x, y) =
        100.0 * (sqrt (abs (y - 0.01 * pown x 2)))
        +
        0.01 * (abs (x + 10.0))

    // https://en.wikipedia.org/wiki/Himmelblau%27s_function
    let himmelblau (x, y) =
        pown (pown x 2 + y - 11.0) 2
        +
        pown (x + pown y 2 - 7.0) 2