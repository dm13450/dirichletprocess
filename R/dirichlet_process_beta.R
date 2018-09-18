#' Dirichlet process mixture of the Beta distribution.
#'
#' Create a Dirichlet process object using the mean and scale parameterisation of the Beta distribution bounded on \eqn{(0, maxY)}.
#'
#' \eqn{G_0 (\mu , \nu | maxY, \alpha _0 , \beta _0) = U(\mu | 0, maxY) \mathrm{Inv-Gamma} (\nu | \alpha _0, \beta _0)}.
#'
#' The parameter \eqn{\beta _0} also has a prior distribution \eqn{\beta _0 \sim \mathrm{Gamma} (a, b)} if the user selects \code{Fit(...,updatePrior=TRUE)}.
#'
#' @param y Data for which to be modelled.
#' @param maxY End point of the data
#' @param g0Priors Prior parameters of the base measure \eqn{(\alpha _0, \beta _0)}.
#' @param alphaPrior Prior parameters for the concentration parameter. See also \code{\link{UpdateAlpha}}.
#' @param mhStep Step size for Metropolis Hastings sampling algorithm.
#' @param hyperPriorParameters Hyper-prior parameters for the prior distributions of the base measure parameters \eqn{(a, b)}.
#' @param verbose Logical, control the level of on screen output.
#' @param mhDraws Number of Metropolis-Hastings samples to perform for each cluster update.
#' @return Dirichlet process object
#' @export
DirichletProcessBeta <- function(y, maxY, g0Priors = c(2, 8), alphaPrior = c(2, 4),
  mhStep = c(1, 1), hyperPriorParameters = c(1, 0.125), verbose=TRUE, mhDraws=250) {

  mdObj <- BetaMixtureCreate(priorParameters = g0Priors, mhStepSize = mhStep, maxT = maxY,
    hyperPriorParameters)
  dpObj <- DirichletProcessCreate(y, mdObj, alphaPrior, mhDraws)
  dpObj <- Initialise(dpObj, verbose=verbose)
  return(dpObj)
}
