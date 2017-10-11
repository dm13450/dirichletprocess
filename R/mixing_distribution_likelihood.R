#' Mixing Distribution Likelihood
#'
#' Evaluate the Likelihood of some data \eqn{x} for some parameter \eqn{\theta}.
#'
#' @param mdObj Mixing Distribution
#' @param x Data
#' @param theta Parameters of distribution
#' @return Likelihood of the data
#' @export
Likelihood <- function(mdObj, x, theta) UseMethod("Likelihood", mdObj)
