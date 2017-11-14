#' Create a Dirichlet mixture of multivariate normal distributions.
#'
#' \eqn{G_0 (\boldsymbol{\mu} , \Lambda |  \boldsymbol{\mu _0} , \kappa _0, \nu _0, T_0)  = N ( \boldsymbol{\mu} | \boldsymbol{\mu _0} , (\kappa _0 \Lambda )^{-1} ) \mathrm{Wi} _{\nu _0} (\Lambda | T_0)}
#'
#' @param y Data
#' @param g0Priors Prior parameters for the base distribution.
#' @param alphaPriors Alpha prior parameters. See \code{\link{UpdateAlpha}}.
#' @export
DirichletProcessMvnormal <- function(y,
                                     g0Priors,
                                     alphaPriors = c(2, 4)) {

  if(!is.matrix(y)){
    y <- matrix(y, ncol=length(y))
  }

  if(missing(g0Priors)){
    g0Priors <- list(mu0 = rep_len(0, length.out = ncol(y)),
                     Lambda = diag(ncol(y)),
                     kappa0 = ncol(y), nu = ncol(y))
  }


  mdobj <- MvnormalCreate(g0Priors)
  dpobj <- DirichletProcessCreate(y, mdobj, alphaPriors)
  dpobj <- Initialise(dpobj)

  return(dpobj)
}
