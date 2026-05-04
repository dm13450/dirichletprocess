# Declare global variables for R CMD check
utils::globalVariables(c("conjugate_mvnormal_cluster_component_update_cpp", "conjugate_mvnormal_cluster_parameter_update_cpp"))

#' C++ Implementation Wrappers for MVNormal Distribution
#'
#' These functions provide access to the C++ implementations of the
#' core sampling algorithms for the MVNormal distribution.
#'
#' @name cpp_mvnormal_wrappers
#' @keywords internal
NULL

#' @rdname cpp_mvnormal_wrappers
#' @param dpObj Dirichlet process object
#' @export
ClusterComponentUpdate.mvnormal.cpp <- function(dpObj) {
  # Ensure we're working with a conjugate MVNormal DP
  if (!inherits(dpObj, "conjugate") || !inherits(dpObj$mixingDistribution, "mvnormal")) {
    stop("This C++ implementation is only for conjugate MVNormal distributions")
  }

  # Check if C++ functions are available
  if (!exists("conjugate_mvnormal_cluster_component_update_cpp")) {
    stop("MVNormal C++ functions not available")
  }

  # Ensure predictiveArray exists
  if (is.null(dpObj$predictiveArray)) {
    dpObj$predictiveArray <- numeric(dpObj$n)
  }

  # The C++ implementation expects 0-indexed cluster labels
  dpObj_cpp <- dpObj
  dpObj_cpp$clusterLabels <- as.integer(dpObj$clusterLabels - 1)

  # Call C++ implementation
  result <- conjugate_mvnormal_cluster_component_update_cpp(dpObj_cpp)

  # Convert back to 1-indexed
  result$clusterLabels <- result$clusterLabels + 1

  # Update the dpObj with results
  dpObj$clusterLabels <- result$clusterLabels
  dpObj$pointsPerCluster <- result$pointsPerCluster
  dpObj$numberClusters <- result$numberClusters
  dpObj$clusterParameters <- result$clusterParameters

  return(dpObj)
}

#' @rdname cpp_mvnormal_wrappers
#' @export
ClusterParameterUpdate.mvnormal.cpp <- function(dpObj) {
  # Ensure we're working with a conjugate MVNormal DP
  if (!inherits(dpObj, "conjugate") || !inherits(dpObj$mixingDistribution, "mvnormal")) {
    stop("This C++ implementation is only for conjugate MVNormal distributions")
  }

  # Check if C++ functions are available
  if (!exists("conjugate_mvnormal_cluster_parameter_update_cpp")) {
    stop("MVNormal C++ functions not available")
  }

  # The C++ implementation expects 0-indexed cluster labels
  dpObj_cpp <- dpObj
  dpObj_cpp$clusterLabels <- as.integer(dpObj$clusterLabels - 1)

  # Call C++ implementation
  dpObj$clusterParameters <- conjugate_mvnormal_cluster_parameter_update_cpp(dpObj_cpp)

  return(dpObj)
}

#' @rdname cpp_mvnormal_wrappers
#' @param n Number of draws
#' @export
PriorDraw.mvnormal.cpp <- function(mdObj, n = 1, ...) {
  if (!exists("mvnormal_prior_draw_cpp")) {
    stop("MVNormal C++ functions not available")
  }
  mvnormal_prior_draw_cpp(mdObj$priorParameters, n)
}

#' @rdname cpp_mvnormal_wrappers
#' @param x Data matrix
#' @export
PosteriorDraw.mvnormal.cpp <- function(mdObj, x, n = 1, ...) {
  if (!exists("mvnormal_posterior_draw_cpp")) {
    stop("MVNormal C++ functions not available")
  }
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  mvnormal_posterior_draw_cpp(mdObj$priorParameters, x, n)
}

#' @rdname cpp_mvnormal_wrappers
#' @export
Likelihood.mvnormal.cpp <- function(mdObj, x, theta) {
  if (!exists("mvnormal_likelihood_cpp")) {
    stop("MVNormal C++ functions not available")
  }
  if (!is.matrix(x)) {
    x <- matrix(x, nrow = 1)
  }

  # Extract mu and sigma from theta
  mu <- as.numeric(theta$mu)
  sig <- theta$sig

  if (is.array(sig) && length(dim(sig)) == 3) {
    # If sig is a 3D array, we need to handle multiple parameter sets
    n_params <- dim(sig)[3]
    result <- numeric(nrow(x) * n_params)

    idx <- 1
    for (i in 1:nrow(x)) {
      for (j in 1:n_params) {
        result[idx] <- mvnormal_likelihood_cpp(
          matrix(x[i,], nrow = 1),
          theta$mu[, , j],
          theta$sig[, , j]
        )[1]
        idx <- idx + 1
      }
    }
    return(result)
  } else {
    # Single parameter set
    return(mvnormal_likelihood_cpp(x, mu, sig))
  }
}
