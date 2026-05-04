#' Create a Dirichlet mixture of multivariate normal distributions.
#'
#' \eqn{G_0 (\boldsymbol{\mu} , \Lambda |  \boldsymbol{\mu _0} , \kappa _0, \nu _0, T_0)  = N ( \boldsymbol{\mu} | \boldsymbol{\mu _0} , (\kappa _0 \Lambda )^{-1} ) \mathrm{Wi} _{\nu _0} (\Lambda | T_0)}
#'
#' Non-\code{"FULL"} covariance models are rejected in the live package.
#'
#' @param y Data
#' @param g0Priors Prior parameters for the base distribution. The live package
#'   currently supports only \code{covModel = "FULL"} for
#'   \code{DirichletProcessMvnormal()}.
#' @param alphaPriors Alpha prior parameters. See \code{\link{UpdateAlpha}}.
#' @param numInitialClusters Number of clusters to initialise with.
#' @param cpp Logical compatibility argument. Constructors no longer toggle the
#'   package-wide C++ implementation flag. \code{Fit()} now selects the
#'   validated FULL-model C++ path automatically when supported.
#' @export
DirichletProcessMvnormal <- function(y,
                                     g0Priors,
                                     alphaPriors = c(2, 4),
                                     numInitialClusters=1,
                                     cpp = FALSE) {

  if(!is.matrix(y)){
    y <- matrix(y, ncol=length(y))
  }

  if(missing(g0Priors)){
    g0Priors <- list(mu0 = rep_len(0, length.out = ncol(y)),
                     Lambda = diag(ncol(y)),
                     kappa0 = ncol(y),
                     nu = ncol(y))
  }

  covModel <- if (is.null(g0Priors$covModel)) "FULL" else as.character(g0Priors$covModel)
  if (!identical(covModel, "FULL")) {
    stop("DirichletProcessMvnormal() currently supports only covModel = 'FULL'.",
         call. = FALSE)
  }

  mdobj <- MvnormalCreate(g0Priors)
  dpobj <- DirichletProcessCreate(y, mdobj, alphaPriors)
  dpobj <- Initialise(dpobj, numInitialClusters=numInitialClusters)

  return(dpobj)
}
