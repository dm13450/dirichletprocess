# R/cpp_hierarchical_mvnormal2_wrappers.R

#' C++ Implementation Wrappers for Hierarchical MVNormal2 Distribution
#'
#' These functions provide access to the C++ implementations of the
#' hierarchical MVNormal2 Dirichlet process algorithms.
#'
#' @name cpp_hierarchical_mvnormal2_wrappers
#' @keywords internal
NULL

#' @rdname cpp_hierarchical_mvnormal2_wrappers
#' @param dpObj Hierarchical Dirichlet process object
#' @param its Number of iterations
#' @param updatePrior Whether to update prior parameters
#' @param progressBar Whether to show progress bar
#' @export
Fit.hierarchical.mvnormal2.cpp <- function(dpObj, its, updatePrior = FALSE, progressBar = TRUE, ...) {
  if (!inherits(dpObj, "hierarchical")) {
    stop("This C++ implementation is only for hierarchical Dirichlet processes")
  }

  # Check if all individual DPs are MVNormal2 type
  all_mvnormal2 <- all(sapply(dpObj$indDP, function(x) inherits(x, "mvnormal2")))

  if (!all_mvnormal2) {
    stop("C++ implementation currently only supports hierarchical MVNormal2 DPs")
  }

  # Convert 1-indexed R labels to 0-indexed C++ labels
  for (i in seq_along(dpObj$indDP)) {
    dpObj$indDP[[i]]$clusterLabels <- dpObj$indDP[[i]]$clusterLabels - 1
  }

  # Call C++ implementation
  result <- hierarchical_mvnormal2_fit_cpp(dpObj, its, updatePrior, progressBar)

  # Convert back to 1-indexed
  for (i in seq_along(result$indDP)) {
    result$indDP[[i]]$clusterLabels <- result$indDP[[i]]$clusterLabels + 1
  }

  # Add chain values
  result$gammaValues <- result$gamma  # This should be updated to store full chain

  return(result)
}

#' @rdname cpp_hierarchical_mvnormal2_wrappers
#' @export
HierarchicalMvnormal2Create.cpp <- function(n, priorParameters, alphaPrior,
                                            gammaPrior, num_sticks) {
  hierarchical_mvnormal2_mixing_create_cpp(
    n = n,
    priorParameters = priorParameters,
    alphaPrior = alphaPrior,
    gammaPrior = gammaPrior,
    num_sticks = num_sticks
  )
}

#' @rdname cpp_hierarchical_mvnormal2_wrappers
#' @export
ClusterComponentUpdate.mvnormal2.cpp <- function(dpObj) {
  if (!inherits(dpObj, "mvnormal2")) {
    stop("This C++ implementation is only for MVNormal2 distributions")
  }

  # Store original class structure
  original_class <- class(dpObj)
  
  # Convert labels
  dpObj$clusterLabels <- dpObj$clusterLabels - 1

  # Call C++ implementation
  result <- nonconjugate_mvnormal2_cluster_component_update_cpp(dpObj)

  # Convert back
  result$clusterLabels <- result$clusterLabels + 1

  # Update dpObj while preserving its structure
  dpObj$clusterLabels <- result$clusterLabels
  dpObj$pointsPerCluster <- result$pointsPerCluster
  dpObj$numberClusters <- result$numberClusters
  dpObj$clusterParameters <- result$clusterParameters

  # Ensure class structure is preserved
  class(dpObj) <- original_class

  return(dpObj)
}

#' @rdname cpp_hierarchical_mvnormal2_wrappers
#' @export
ClusterParameterUpdate.mvnormal2.cpp <- function(dpObj) {
  if (!inherits(dpObj, "mvnormal2")) {
    stop("This C++ implementation is only for MVNormal2 distributions")
  }

  # Store original class structure
  original_class <- class(dpObj)
  
  # Convert labels
  dpObj$clusterLabels <- dpObj$clusterLabels - 1

  # Call C++ implementation
  dpObj$clusterParameters <- nonconjugate_mvnormal2_cluster_parameter_update_cpp(dpObj)

  # Convert back
  dpObj$clusterLabels <- dpObj$clusterLabels + 1

  # Ensure class structure is preserved
  class(dpObj) <- original_class

  return(dpObj)
}

#' @rdname cpp_hierarchical_mvnormal2_wrappers
#' @export
PriorDraw.mvnormal2.cpp <- function(mdObj, n = 1, ...) {
  mvnormal2_prior_draw_cpp(mdObj$priorParameters, n)
}

#' @rdname cpp_hierarchical_mvnormal2_wrappers
#' @export
PosteriorDraw.mvnormal2.cpp <- function(mdObj, x, n = 1, ...) {
  if (!is.matrix(x)) {
    x <- matrix(x, nrow = 1)
  }
  mvnormal2_posterior_draw_cpp(mdObj$priorParameters, x, n)
}

#' @rdname cpp_hierarchical_mvnormal2_wrappers
#' @export
Likelihood.mvnormal2.cpp <- function(mdObj, x, theta) {
  if (!is.matrix(x)) {
    x <- matrix(x, nrow = 1)
  }

  # The C++ function now handles the full matrix
  return(mvnormal2_likelihood_cpp(x, theta))
}

#' @rdname cpp_hierarchical_mvnormal2_wrappers
#' @param dpObj Dirichlet process object
#' @param its Number of iterations
#' @param updatePrior Whether to update prior parameters
#' @param progressBar Whether to show progress bar
#' @export
fit_mvnormal2_cpp <- function(dpObj, its, updatePrior = FALSE, progressBar = TRUE, ...) {
  # DEPRECATED: MVNormal2 now uses unified CppMCMCRunner interface
  warning("fit_mvnormal2_cpp is deprecated. MVNormal2 now uses the unified interface through Fit(). ",
          "Please use Fit() instead, which will automatically use the unified C++ implementation.")
  
  # Redirect to unified interface
  return(Fit(dpObj, its, updatePrior, progressBar, ...))
}
