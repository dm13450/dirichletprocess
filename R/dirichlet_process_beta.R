#' Dirichlet process mixture of the Beta distribution.
#'
#' Create a Dirichlet process object using the mean and precision
#' parameterisation of the Beta distribution bounded on \eqn{(0, maxY)}.
#' This is not the ordinary Beta \eqn{(\alpha, \beta)} shape-parameter
#' parameterisation.
#'
#' \eqn{G_0 (\mu , \nu | maxY, \alpha _0 , \beta _0) = U(\mu | 0, maxY)
#' \mathrm{Inv-Gamma} (\nu | \alpha _0, \beta _0)}.
#'
#' The parameter \eqn{\beta _0} also has a prior distribution
#' \eqn{\beta _0 \sim \mathrm{Gamma} (a, b)} if the user selects
#' \code{Fit(..., updatePrior = TRUE)}.
#'
#' @param y Data for which to be modelled.
#' @param maxY End point of the data. Defaults to 1 for backward compatibility.
#' @param g0Priors Prior parameters of the base measure for the precision
#'   component \eqn{(\alpha _0, \beta _0)}.
#' @param alphaPrior Prior parameters for the concentration parameter.
#' @param mhStep Step size for the Metropolis-Hastings sampler.
#' @param hyperPriorParameters Hyper-prior parameters for the prior distributions
#'   of the base measure parameters \eqn{(a, b)}.
#' @param verbose Logical, control the level of on-screen output.
#' @param mhDraws Number of Metropolis-Hastings samples to perform for each cluster update.
#' @param cpp Logical compatibility argument. Constructors no longer toggle the
#'   package-wide C++ implementation flag; `Fit()` now selects the validated
#'   C++ path automatically when supported.
#' @param ... Backward-compatible aliases such as \code{alphaPriors} or \code{mhStepSize}.
#' @return Dirichlet process object
#' @export
DirichletProcessBeta <- function(y, maxY = 1, g0Priors = c(2, 8),
                                 alphaPrior = c(2, 4), mhStep = c(1, 1),
                                 hyperPriorParameters = c(1, 0.125),
                                 verbose = TRUE, mhDraws = 250,
                                 cpp = FALSE, ...) {
  dots <- list(...)

  if (!is.null(dots$alphaPriors)) {
    alphaPrior <- dots$alphaPriors
  }
  if (!is.null(dots$mhStepSize)) {
    mhStep <- dots$mhStepSize
  }
  if (!is.null(dots$maxT)) {
    maxY <- dots$maxT
  }

  mdObj <- BetaMixtureCreate(priorParameters = g0Priors,
                             mhStepSize = mhStep,
                             maxT = maxY,
                             hyperPriorParameters = hyperPriorParameters)

  dpObj <- DirichletProcessCreate(y, mdObj, alphaPrior, mhDraws)
  dpObj <- Initialise(dpObj, verbose = verbose)

  # Ensure cluster accounting is correct
  dpObj$pointsPerCluster <- as.numeric(table(factor(dpObj$clusterLabels,
                                                    levels = 1:dpObj$numberClusters)))

  return(dpObj)
}

#' @export
#' @rdname Initialise
Initialise.beta <- function(dpObj, posterior = TRUE, m = 3, verbose = TRUE, numInitialClusters = 1, ...) {

  dpObj$m <- m
  dpObj$clusterLabels <- rep_len(seq_len(numInitialClusters), length.out = dpObj$n)
  dpObj$numberClusters <- numInitialClusters
  dpObj$pointsPerCluster <- vapply(seq_len(numInitialClusters),
                                   function(x) sum(dpObj$clusterLabels == x),
                                   numeric(1))

  if (posterior && numInitialClusters == 1) {
    post_draws <- PosteriorDraw(dpObj$mixingDistribution, dpObj$data, 1000)

    if (verbose) {
      cat(paste("Accept Ratio: ",
                length(unique(c(post_draws[[1]]))) / 1000,
                "\n"))
    }

    dpObj$clusterParameters <- lapply(post_draws, function(x) x[, , 1000, drop = FALSE])
  } else {
    dpObj$clusterParameters <- PriorDraw(dpObj$mixingDistribution, numInitialClusters)
  }

  # Generate auxiliary parameters with proper structure
  dpObj$aux <- vector("list", m)
  for(i in seq_len(m)) {
    aux_draw <- PriorDraw(dpObj$mixingDistribution, 1)
    dpObj$aux[[i]] <- list(
      mu = array(aux_draw$mu, dim = c(1, 1, 1)),
      nu = array(aux_draw$nu, dim = c(1, 1, 1))
    )
  }

  if (verbose) {
    cat("Dirichlet process initialised.\n")
  }

  return(dpObj)
}
