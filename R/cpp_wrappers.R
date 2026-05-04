#' C++ Implementation Wrappers for Normal Distribution
#'
#' These functions provide access to the C++ implementations of the
#' core sampling algorithms for the Normal distribution.
#'
#' @name cpp_normal_wrappers
#' @keywords internal
NULL

#' @rdname cpp_normal_wrappers
#' @param priorParams Prior parameters (mu0, kappa0, alpha0, beta0)
#' @param n Number of draws
#' @export
normal_prior_draw_cpp_wrapper <- function(priorParams, n = 1) {
  if (!is.numeric(priorParams) || length(priorParams) != 4) {
    stop("priorParams must be a numeric vector of length 4")
  }
  if (n < 1) {
    stop("n must be at least 1")
  }

  normal_prior_draw_cpp(priorParams, n)
}

#' @rdname cpp_normal_wrappers
#' @param x Data matrix
#' @export
normal_posterior_draw_cpp_wrapper <- function(priorParams, x, n = 1) {
  if (!is.numeric(priorParams) || length(priorParams) != 4) {
    stop("priorParams must be a numeric vector of length 4")
  }
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  if (n < 1) {
    stop("n must be at least 1")
  }

  normal_posterior_draw_cpp(priorParams, x, n)
}

#' @rdname cpp_normal_wrappers
#' @param dpObj Dirichlet process object
#' @export
ClusterComponentUpdate.conjugate.cpp <- function(dpObj) {
  # Ensure we're working with a conjugate normal DP
  if (!inherits(dpObj, "conjugate") || !inherits(dpObj, "normal")) {
    stop("This C++ implementation is only for conjugate normal distributions")
  }

  # The C++ implementation expects 0-indexed cluster labels
  dpObj_cpp <- dpObj
  dpObj_cpp$clusterLabels <- dpObj$clusterLabels - 1

  # Call C++ implementation
  result <- conjugate_cluster_component_update_cpp(dpObj_cpp)

  # Convert back to 1-indexed
  result$clusterLabels <- result$clusterLabels + 1

  # Update the dpObj with results
  dpObj$clusterLabels <- result$clusterLabels
  dpObj$pointsPerCluster <- result$pointsPerCluster
  dpObj$numberClusters <- result$numberClusters
  dpObj$clusterParameters <- result$clusterParameters

  return(dpObj)
}

#' @rdname cpp_normal_wrappers
#' @export
ClusterParameterUpdate.conjugate.cpp <- function(dpObj) {
  # Ensure we're working with a conjugate normal DP
  if (!inherits(dpObj, "conjugate") || !inherits(dpObj, "normal")) {
    stop("This C++ implementation is only for conjugate normal distributions")
  }

  # The C++ implementation expects 0-indexed cluster labels
  dpObj_cpp <- dpObj
  dpObj_cpp$clusterLabels <- dpObj$clusterLabels - 1

  # Call C++ implementation
  dpObj$clusterParameters <- conjugate_cluster_parameter_update_cpp(dpObj_cpp)

  return(dpObj)
}

#' Enable C++ implementations for Normal samplers
#'
#' This function enables the use of C++ implementations for the Normal
#' distribution sampling algorithms when available.
#'
#' @param use_cpp Logical indicating whether to use C++ implementations
#' @export
enable_cpp_normal_samplers <- function(use_cpp = TRUE) {
  options(dirichletprocesscpp.use_cpp_samplers = use_cpp)

  if (use_cpp) {
    message("C++ samplers enabled for conjugate Normal distribution")
  } else {
    message("Using R implementations for all samplers")
  }

  invisible(use_cpp)
}

# Note: using_cpp_samplers() function is now defined in cpp_interface.R
# to properly handle the unified MCMC runner detection
