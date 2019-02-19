#' Create a Dirichlet mixture of multivariate normal distributions with semi-conjugate prior.
#'
#'
#'
#' @param y Data
#' @param g0Priors Prior parameters for the base distribution.
#' @param alphaPriors Alpha prior parameters. See \code{\link{UpdateAlpha}}.
#' @export
DirichletProcessMvnormal2 <- function(y,
                                     g0Priors,
                                     alphaPriors = c(2, 4)) {

  if (!is.matrix(y)){
    y <- matrix(y, ncol=length(y))
  }

  if(missing(g0Priors)){
    g0Priors <- list(nu0 = 2,
                     phi0 = diag(ncol(y)),
                     mu0 = numeric(ncol(y)),
                     sigma0 = diag(ncol(y)))
  }


  mdobj <- Mvnormal2Create(g0Priors)
  dpobj <- DirichletProcessCreate(y, mdobj, alphaPriors)
  dpobj <- Initialise(dpobj)

  return(dpobj)
}
