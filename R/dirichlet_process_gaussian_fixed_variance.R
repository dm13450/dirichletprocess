#' Create a Dirichlet Mixture of the Gaussian Distribution with fixed variance.
#'
#'
#' @param y Data.
#' @param sigma The fixed variance
#' @param g0Priors Base Distribution Priors.
#' @param alphaPriors Prior parameter distributions for the alpha concentration parameter.
#' @return Dirichlet process object
#'
#' @export
DirichletProcessGaussianFixedVariance <- function(y,
                                                  sigma,
                                                  g0Priors = c(0, 1),
                                                  alphaPriors = c(2, 4)) {

  mdobj <- GaussianFixedVarianceMixtureCreate(g0Priors, sigma)
  dpobj <- DirichletProcessCreate(y, mdobj, alphaPriors)
  dpobj <- Initialise(dpobj)
  return(dpobj)
}
