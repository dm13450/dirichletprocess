#' Create a Mixing Object for a hierarchical Beta Dirichlet process object.
#'
#' @param n Number of data sets
#' @param priorParameters The prior parameters for the top level base distribution.
#' @param hyperPriorParameters Hyper prior parameters for the top level base distribution.
#' @param gammaPrior Concentration parameter for the top level priors.
#' @param alphaPrior Individual level concentration parameter priors.
#' @param maxT Bounding value of the data.
#' @param mhStepSize Metropolis Hastings step size for the posterior drawing.
#' @param num_sticks Number of stick breaking values to use.
#' @return A mixing distribution object.
#' @export
HierarchicalBetaCreate <- function(n, priorParameters, hyperPriorParameters,
                                   alphaPrior, maxT, gammaPrior,
                                   mhStepSize, num_sticks) {
  # Phase 5 route lock:
  # hierarchical construction stays on the rewritten R path until a coherent
  # hierarchical C++ implementation exists.

  # Original R implementation
  mdobj_beta_base <- BetaMixtureCreate(priorParameters, mhStepSize = mhStepSize,
                                       maxT = maxT, hyperPriorParameters = hyperPriorParameters)

  class(mdobj_beta_base) <- c("hierarchical", "beta", "nonconjugate")

  gammaParam <- rgamma(1, gammaPrior[1], gammaPrior[2])

  theta_k <- PriorDraw.beta(mdobj_beta_base, num_sticks)
  beta_k <- StickBreaking(gammaParam, num_sticks)

  mdobj_list <- vector("list", n)

  for (i in seq_len(n)) {
    # Create a fresh copy of the base object for each group
    mdobj_beta <- BetaMixtureCreate(priorParameters, mhStepSize = mhStepSize,
                                    maxT = maxT, hyperPriorParameters = hyperPriorParameters)

    class(mdobj_beta) <- c("hierarchical", "beta", "nonconjugate")

    # Set the shared global parameters
    mdobj_beta$theta_k <- theta_k
    mdobj_beta$beta_k <- beta_k
    mdobj_beta$gamma <- gammaParam

    # Set the individual alpha
    mdobj_beta$alpha <- rgamma(1, alphaPrior[1], alphaPrior[2])
    mdobj_beta$pi_k <- draw_gj(mdobj_beta$alpha, beta_k)

    mdobj_list[[i]] <- mdobj_beta
  }

  return(mdobj_list)
}
