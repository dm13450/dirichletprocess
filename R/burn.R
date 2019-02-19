#' Add burn-in to a dirichletprocess object
#'
#' @param dpobj A dirichletprocess object.
#' @param niter Number of iterations to burn.
#'
#' @return A dirichletprocess object where all chain objects have the first
#'   \code{niter} iterations are removed.
#' @export
#'
#' @examples
#' dp <- Fit(DirichletProcessGaussian(rnorm(10)), 100)
#' DiagnosticPlots(dp)
#' burned_dp <- Burn(dp, 50)
#' DiagnosticPlots(burned_dp)
Burn <- function(dpobj, niter) {

  if (niter >= length(dpobj$likelihoodChain)) {
    stop("Can't burn more than total iterations.")
  }
  if (niter < 1) stop("Can't burn less than 1 iteration.")

  burned_dpobj <- dpobj

  # Names of all chains in the dp object.
  chain_names <- grep("Chain", names(dpobj), value = TRUE)

  for (nm in chain_names) {
    burned_dpobj[[nm]] <- dpobj[[nm]][-(1:niter)]
  }

  # Save the number of burned iterations.
  prevburn <- ifelse(is.null(burned_dpobj$n_burned), 0,  burned_dpobj$n_burned)
  burned_dpobj$n_burned <- niter + prevburn

  burned_dpobj
}
