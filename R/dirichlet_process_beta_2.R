#' Dirichlet process mixture of Beta distributions with a Uniform Pareto base measure.
#'
#' Create a Dirichlet process object using the mean and scale parameterisation of the Beta distribution bounded on \eqn{(0, maxY)}.
#' The Pareto distribution is used as a prior on the scale parameter to ensure that the likelihood is 0 at the boundaries.
#'
#' \eqn{G_0 (\mu , \nu | maxY, \alpha ) = U(\mu | 0, maxY) \mathrm{Pareto} (\nu | x_m, \gamma)}.
#' @param y Data for which to be modelled.
#' @param maxY End point of the data
#' @param g0Priors Prior parameters of the base measure \eqn{(\gamma}.
#' @param alphaPrior Prior parameters for the concentration parameter. See also \code{\link{UpdateAlpha}}.
#' @param mhStep Step size for Metropolis Hastings sampling algorithm.
#' @param verbose Logical, control the level of on screen output.
#' @param mhDraws Number of Metropolis-Hastings samples to perform for each cluster update.
#' @return Dirichlet process object
#' @export
DirichletProcessBeta2 <- function(y, maxY, g0Priors = 2, alphaPrior = c(2, 4),
                                  mhStep = c(1, 1), verbose=TRUE, mhDraws=250) {

  mdObj <- BetaMixture2Create(priorParameters = g0Priors, mhStepSize = mhStep, maxT = maxY)
  dpObj <- DirichletProcessCreate(y, mdObj, alphaPrior, mhDraws)
  dpObj <- Initialise(dpObj, verbose=verbose)
  return(dpObj)
}
