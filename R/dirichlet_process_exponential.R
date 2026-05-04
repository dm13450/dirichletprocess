#'Create a Dirichlet Mixture of Exponentials
#'
#' This is the constructor function to produce a \code{dirichletprocess} object with a Exponential mixture kernel with unknown rate.
#' The base measure is a  Gamma distribution that is conjugate to the posterior distribution.
#'
#' \eqn{G_0(\theta | \alpha _0, \beta_0) =  \mathrm{Gamma} \left(\theta | \alpha_0, \beta_0 \right)}
#'
#'
#'@param y Data
#'@param g0Priors Base Distribution Priors \eqn{\alpha _0 , \beta _0)}
#'@param alphaPriors Alpha prior parameters. See \code{\link{UpdateAlpha}}.
#'@param cpp Logical compatibility argument. Constructors no longer toggle the
#'  package-wide C++ implementation flag; `Fit()` now selects the validated C++
#'  path automatically when supported.
#'@return Dirichlet process object
#'@export
DirichletProcessExponential <- function(y, g0Priors=c(0.01,0.01), alphaPriors=c(2,4),
                                        cpp = FALSE){

  mdObj <- ExponentialMixtureCreate(g0Priors)
  dpObj <- DirichletProcessCreate(y, mdObj, alphaPriors)
  dpObj <- Initialise(dpObj)

  return(dpObj)
}
