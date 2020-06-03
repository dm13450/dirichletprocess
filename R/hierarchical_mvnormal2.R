#' Create a Mixing Object for a hierarchical semi-conjugate
#' Multivariate Normal Dirichlet process object.
#'
#' @param n Number of data sets
#' @param priorParameters The prior parameters for the top level base distribution.
#' @param alphaPrior Individual level concentration parameter priors.
#' @param gammaPrior Concentration parameter for the top level priors.
#' @param num_sticks Number of stick breaking values to use.
#' @return A mixing distribution object.
#' @export
#'

HierarchicalMvnormal2Create <- function(n, priorParameters,
                                        alphaPrior, gammaPrior,
                                        num_sticks) {

  mdobj_mvnormal2 <- Mvnormal2Create(priorParameters)

  class(mdobj_mvnormal2) <- c("hierarchical", "mvnormal2", "nonconjugate")


  gammaParam <- rgamma(1, gammaPrior[1], gammaPrior[2])


  theta_k <- PriorDraw.mvnormal2(mdobj_mvnormal2, num_sticks)
  beta_k <- StickBreaking(gammaParam, num_sticks)

  mdobj_mvnormal2$theta_k <- theta_k
  mdobj_mvnormal2$beta_k <- beta_k
  mdobj_mvnormal2$gamma <- gammaParam

  #mdobj_beta$pi_k <- draw_gj(alpha0, beta_k)

  mdobj_list <- vector("list", n)

  for (i in seq_len(n)) {
    mdobj_mvnormal2$alpha <- rgamma(1, alphaPrior[1], alphaPrior[2])
    mdobj_mvnormal2$pi_k <- draw_gj(mdobj_mvnormal2$alpha, beta_k)

    mdobj_list[[i]] <- mdobj_mvnormal2
  }
  return(mdobj_list)
}
