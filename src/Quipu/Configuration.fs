namespace Quipu

module Default =

    let tolerance = 0.001
    let alpha = 1.0
    let gamma = 2.0
    let rho = 0.5
    let sigma = 0.5

type UpdateParameters = {
    /// Reflection parameter, Alpha > 0.0
    Alpha: float
    /// Expansion parameter, Gamma > 1.0
    Gamma: float
    /// Contraction parameter, 0.0 < Rho <= 1.0
    Rho: float
    /// Shrink parameter, 0.0 < Sigma < 1.0
    Sigma: float
    }
    with
    static member defaultValue = {
        Alpha = Default.alpha
        Gamma = Default.gamma
        Rho = Default.rho
        Sigma = Default.sigma
        }

type Configuration = {
    Updates: UpdateParameters
    Tolerance: float
    MaximumIterations: Option<int>
    }
    with
    static member defaultValue = {
        Updates = UpdateParameters.defaultValue
        Tolerance = Default.tolerance
        MaximumIterations = None
        }