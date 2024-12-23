namespace Quipu

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
        Alpha = 1.0
        Gamma = 2.0
        Rho = 0.5
        Sigma = 0.5
        }

type Configuration = {
    Updates: UpdateParameters
    Termination: ITerminator
    MaximumIterations: Option<int>
    }
    with
    static member defaultValue = {
        Updates = UpdateParameters.defaultValue
        Termination = Termination.tolerance 0.001
        MaximumIterations = None
        }