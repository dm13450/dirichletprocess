# R/dirichlet_process_hierarchical_mvnormal2.R
#' Create a Hierarchical Dirichlet Mixture of
#' semi-conjugate Multivariate Normal Distributions
#'
#' The top-level hierarchical object is fitted through the rewritten R-only
#' live path. Top-level \code{PosteriorSummary()}, \code{PosteriorFrame()},
#' \code{PosteriorFunction()}, \code{PosteriorClusters()}, and \code{plot()}
#' calls are intentionally unsupported. For a local restaurant object, use
#' \code{dplist$indDP[[j]]}, which can be summarised with
#' \code{\link{PosteriorSummary}}.
#'
#' @param dataList List of data for each separate Dirichlet mixture object
#' @param g0Priors Prior Parameters for the top level base distribution.
#' @param gammaPriors Prior parameters for the top level concentration parameter.
#' @param alphaPriors Prior parameters for the individual parameters.
#' @param numSticks Truncation level for the Stick Breaking formulation.
#' @param numInitialClusters Number of clusters to initialise with.
#' @param mhDraws Number of Metropolis-Hastings samples to perform for each cluster update.
#' @param cpp Logical compatibility argument. The live hierarchical HDP path is
#'   locked to the rewritten R implementation; this argument is accepted for
#'   backward compatibility only and does not enable live hierarchical C++
#'   fitting or mutate ordinary C++ routing state.
#' @return A hierarchical Dirichlet process object. Top-level posterior summary
#'   and plotting helpers are intentionally unsupported; use local restaurant
#'   objects in \code{indDP} for those tasks.
#' @export
DirichletProcessHierarchicalMvnormal2 <- function(dataList,
                                                  g0Priors,
                                                  gammaPriors = c(2,4), alphaPriors = c(2, 4),
                                                  numSticks = 50,
                                                  numInitialClusters = 1,
                                                  mhDraws=250, cpp = FALSE) {

  if(missing(g0Priors)){
    g0Priors <- list(nu0 = 2,
                     phi0 = diag(ncol(dataList[[1]])),
                     mu0 = numeric(ncol(dataList[[1]])),
                     sigma0 = diag(ncol(dataList[[1]])))
  }

  # Add this block to ensure mu0 is a matrix:
  if (!is.matrix(g0Priors$mu0) || nrow(g0Priors$mu0) != 1) {
    if (is.numeric(g0Priors$mu0) && (is.vector(g0Priors$mu0) || is.array(g0Priors$mu0))) {
      g0Priors$mu0 <- matrix(g0Priors$mu0, nrow = 1)
    } else {
      stop("g0Priors$mu0 must be a numeric vector or a 1xN matrix.")
    }
  }

  mdobj_list <- HierarchicalMvnormal2Create(n=length(dataList), priorParameters=g0Priors,
                                            gammaPrior=gammaPriors,
                                            alphaPrior = alphaPriors, num_sticks=numSticks)

  dpobjlist <- list()
  dpobjlist$indDP <- lapply(seq_along(dataList),
                            function(x) DirichletProcessCreate(dataList[[x]], mdobj_list[[x]], alphaPriors, mhDraws))

  dpobjlist$indDP <- lapply(dpobjlist$indDP,
                            Initialise,
                            posterior = FALSE,
                            numInitialClusters = numInitialClusters)

  for(i in seq_along(dpobjlist$indDP)){
    dpobjlist$indDP[[i]]$alpha <- dpobjlist$indDP[[i]]$mixingDistribution$alpha
  }

  dpobjlist$globalStick <- mdobj_list[[1]]$beta_k
  dpobjlist$gamma <- mdobj_list[[1]]$gamma
  dpobjlist$gammaPriors <- gammaPriors
  dpobjlist <- hdp_initialise_phase1_state(dpobjlist)

  # CRITICAL: Set class with hierarchical FIRST to ensure proper method dispatch
  class(dpobjlist) <- c("hierarchical", "dirichletprocess", "list")

  # Compatibility only: live hierarchical routing stays on the rewritten R path.
  force(cpp)

  return(dpobjlist)
}
