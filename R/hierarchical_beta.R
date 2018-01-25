#' Create a Mixing Object for a hierarchical Beta Dirichlet process object.
#'
#' @param n Number of data sets
#' @param priorParameters The prior parameters for the top level base distribution.
#' @param hyperPriorParameters Hyper prior parameters for the top level base distribution.
#' @param gammaParam Concentration parameter for the top level.
#' @param alpha0 Individual level concentration parameter.
#' @param maxT Bounding value of the data.
#' @param mhStepSize Metropolis Hastings step size for the posterior drawing.
#' @param num_sticks Number of stick breaking values to use.
#' @return A mixing distribution object.
#' @export
HierarchicalBetaCreate <- function(n, priorParameters, hyperPriorParameters,
                                   alpha0, maxT, gammaParam,
                                   mhStepSize, num_sticks) {

  mdobj_beta <- BetaMixtureCreate(priorParameters, mhStepSize = mhStepSize,
    maxT = maxT, hyperPriorParameters = hyperPriorParameters)

  class(mdobj_beta) <- c("hierarchical", "beta", "nonconjugate")

  theta_k <- PriorDraw.beta(mdobj_beta, num_sticks)
  beta_k <- StickBreaking(gammaParam, num_sticks)

  mdobj_beta$theta_k <- theta_k
  mdobj_beta$beta_k <- beta_k

  mdobj_beta$pi_k <- draw_gj(alpha0, beta_k)

  mdobj_list <- vector("list", n)

  for (i in seq_len(n)) {
    mdobj_beta$alpha <- alpha0[i]
    mdobj_beta$pi_k <- draw_gj(alpha0[i], beta_k)

    mdobj_list[[i]] <- mdobj_beta
  }
  return(mdobj_list)
}
