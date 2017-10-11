#' Create a Dirichlet Mixture of the Weibull distribution
#'
#' The likelihood is parameterised as \eqn{\mathrm{Weibull} (y | a, b) = \frac{a}{b} y ^{a-1}  \exp \left( -  \frac{x^a}{b}  \right)}.
#' The base measure is a Uniform Inverse Gamma Distribution.
#' \eqn{G_0 (a, b | \phi, \alpha _0 , \beta _0) = U(a | 0, \phi ) \mathrm{Inv-Gamma} ( b | \alpha _0, \beta _0)}
#' \eqn{\phi \sim \mathrm{Pareto}(x_m , k)}
#' \eqn{\beta \sim \mathrm{Gamma} (\alpha _0 , \beta _0)}
#' This is a semi-conjugate distribution. The cluster parameter a is updated using the Metropolis Hastings algorithm an analytical posterior exists for b.
#'
#' @param y Data.
#' @param g0Priors Base Distribution Priors.
#' @param alphaPriors Prior for the concentration parameter.
#' @param mhStepSize Step size for the new parameter in the Metropolis Hastings algorithm.
#' @param hyperPriorParameters Hyper prior parameters.
#' @param verbose Set the level of screen output.
#' @return Dirichlet process object
#'
#' @references Kottas, A. (2006). Nonparametric Bayesian survival analysis using mixtures of Weibull distributions. Journal of Statistical Planning and Inference, 136(3), 578-596.
#'
#'
#' @export
DirichletProcessWeibull <- function(y, g0Priors, alphaPriors = c(2, 4),
                                    mhStepSize = c(1, 1),
                                    hyperPriorParameters = c(6, 2, 1, 0.5),
                                    verbose=FALSE) {

  mdobj <- WeibullMixtureCreate(g0Priors, mhStepSize, hyperPriorParameters)
  dpobj <- DirichletProcessCreate(y, mdobj, alphaPriors)
  dpobj <- Initialise(dpobj, verbose = verbose)
  return(dpobj)
}
