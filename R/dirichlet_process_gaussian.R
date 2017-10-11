#'Create a Dirichlet Mixture of Gaussians
#'
#' This is the constructor function to produce a \code{dirichletprocess} object with a Gaussian mixture kernel with unknown mean and variance.
#' The base measure is a Normal Inverse Gamma distribution that is conjugate to the posterior distribution.
#'
#' \eqn{G_0(\theta | \gamma) =  N \left(\mu | \mu_0, \frac{\sigma^2}{k_0} \right) \mathrm{Inv-Gamma} \left(\sigma^2 | \alpha_0, \beta_0 \right)}
#'
#' We recommend scaling your data to zero mean and unit variance for quicker convergence.
#'
#'@param y Data
#'@param g0Priors Base Distribution Priors \eqn{\gamma = (\mu _0, k_0 , \alpha _0 , \beta _0)}
#'@param alphaPriors Alpha prior parameters. See \code{\link{UpdateAlpha}}.
#'@return Dirichlet process object
#'@export
DirichletProcessGaussian <- function(y, g0Priors = c(0, 1, 1, 1),
                                     alphaPriors = c(2, 4)) {

  mdobj <- GaussianMixtureCreate(g0Priors)
  dpobj <- DirichletProcessCreate(y, mdobj, alphaPriors)
  dpobj <- Initialise(dpobj)
  return(dpobj)
}
