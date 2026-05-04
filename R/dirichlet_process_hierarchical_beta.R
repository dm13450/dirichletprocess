#' Create a Hierarchical Dirichlet Mixture of Beta Distributions
#'
#' The top-level hierarchical object is fitted through the rewritten R-only
#' live path. Top-level \code{PosteriorSummary()}, \code{PosteriorFrame()},
#' \code{PosteriorFunction()}, \code{PosteriorClusters()}, and \code{plot()}
#' calls are intentionally unsupported. For a local restaurant object, use
#' \code{dplist$indDP[[j]]}, which can be summarised with
#' \code{\link{PosteriorSummary}}.
#'
#' @param dataList List of data for each separate Dirichlet mixture object
#' @param maxY Maximum value for the Beta distribution.
#' @param priorParameters Prior Parameters for the top level base distribution.
#' @param hyperPriorParameters Hyper prior parameters for the top level base distribution.
#' @param gammaPriors Prior parameters for the top level concentration parameter.
#' @param alphaPriors Prior parameters for the individual parameters.
#' @param mhStepSize Metropolis Hastings jump size.
#' @param numSticks Truncation level for the Stick Breaking formulation.
#' @param mhDraws Number of Metropolis-Hastings samples to perform for each cluster update.
#' @param cpp Logical compatibility argument. The live hierarchical HDP path is
#'   locked to the rewritten R implementation; this argument is accepted for
#'   backward compatibility only and does not enable live hierarchical C++
#'   fitting or mutate ordinary C++ routing state.
#' @return A hierarchical Dirichlet process object. Top-level posterior summary
#'   and plotting helpers are intentionally unsupported; use local restaurant
#'   objects in \code{indDP} for those tasks.
#' @export
DirichletProcessHierarchicalBeta <- function(dataList, maxY,
                                             priorParameters = c(2,8),
                                             hyperPriorParameters = c(1,0.125),
                                             gammaPriors = c(2,4), alphaPriors = c(2, 4),
                                             mhStepSize = c(0.1,0.1), numSticks = 50, mhDraws=250,
                                             cpp = FALSE) {

  mdobj_list <- HierarchicalBetaCreate(n=length(dataList), priorParameters=priorParameters,
                                       hyperPriorParameters=hyperPriorParameters, gammaPrior=gammaPriors,
                                       alphaPrior = alphaPriors, maxT=maxY, mhStepSize=mhStepSize, num_sticks=numSticks)

  dpobjlist <- list()
  dpobjlist$indDP <- lapply(seq_along(dataList),
                            function(x) DirichletProcessCreate(dataList[[x]], mdobj_list[[x]], alphaPriors, mhDraws))

  dpobjlist$indDP <- lapply(dpobjlist$indDP, Initialise, posterior=FALSE, m=20)

  # Ensure alpha is initialized from the mixing distribution
  for(i in seq_along(dpobjlist$indDP)){
    dpobjlist$indDP[[i]]$alpha <- dpobjlist$indDP[[i]]$mixingDistribution$alpha
  }

  dpobjlist$globalStick <- mdobj_list[[1]]$beta_k
  dpobjlist$gamma <- mdobj_list[[1]]$gamma
  dpobjlist$gammaPriors <- gammaPriors
  dpobjlist <- hdp_initialise_phase1_state(dpobjlist)

  # CRITICAL FIX: Put "hierarchical" before "dirichletprocess" for proper S3 dispatch
  class(dpobjlist) <- c("hierarchical", "dirichletprocess", "list")

  # Compatibility only: live hierarchical routing stays on the rewritten R path.
  force(cpp)

  return(dpobjlist)
}
