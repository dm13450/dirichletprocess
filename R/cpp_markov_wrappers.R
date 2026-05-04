#' C++ Implementation Wrappers for Markov DP (HMM)
#'
#' These functions provide access to the C++ implementations of the
#' Markov Dirichlet process (Hidden Markov Model) algorithms.
#'
#' @name cpp_markov_wrappers
#' @keywords internal
NULL

#' @rdname cpp_markov_wrappers
#' @param dpObj Markov Dirichlet process object
#' @param its Number of iterations
#' @param progressBar Whether to show progress bar
#' @export
Fit.markov.cpp <- function(dpObj, its, updatePrior = FALSE, progressBar = TRUE, ...) {
  if (!inherits(dpObj, "markov")) {
    stop("This C++ implementation is only for Markov Dirichlet processes")
  }

  # Convert 1-indexed R states to 0-indexed C++ states
  dpObj$states <- dpObj$states - 1

  # Call C++ implementation
  result <- markov_dp_fit_cpp(dpObj, its, updatePrior, progressBar)

  # Convert back to 1-indexed
  result$states <- result$states + 1
  if (!is.null(result$statesChain)) {
    result$statesChain <- lapply(result$statesChain, function(x) x + 1)
  }

  return(result)
}

#' @rdname cpp_markov_wrappers
#' @export
UpdateStates.cpp <- function(dpObj) {
  if (!inherits(dpObj, "markov")) {
    stop("This C++ implementation is only for Markov Dirichlet processes")
  }

  # Convert states
  dpObj$states <- dpObj$states - 1

  # Call C++ implementation
  result <- markov_dp_update_states_cpp(dpObj)

  # Convert back
  result$states <- result$states + 1

  # Update the params list based on new states
  new_states <- list(result$states, result$params)

  return(new_states)
}

#' @rdname cpp_markov_wrappers
#' @export
UpdateAlphaBeta.cpp <- function(dpObj) {
  if (!inherits(dpObj, "markov")) {
    stop("This C++ implementation is only for Markov Dirichlet processes")
  }

  # Convert states
  dpObj$states <- dpObj$states - 1

  # Call C++ implementation
  result <- markov_dp_update_alpha_beta_cpp(dpObj)

  # Convert back
  result$states <- result$states + 1

  # Extract alpha and beta
  dpObj$alpha <- result$alpha
  dpObj$beta <- result$beta

  return(dpObj)
}

#' @rdname cpp_markov_wrappers
#' @export
param_update.cpp <- function(dpObj) {
  if (!inherits(dpObj, "markov")) {
    stop("This C++ implementation is only for Markov Dirichlet processes")
  }

  # Convert states
  dpObj$states <- dpObj$states - 1

  # Call C++ implementation
  result <- markov_dp_param_update_cpp(dpObj)

  # Convert back
  result$states <- result$states + 1

  # Update dp with new parameters
  dpObj$uniqueParams <- result$uniqueParams
  dpObj$params <- result$params

  return(dpObj)
}

#' Enable C++ implementations for Markov DP samplers
#'
#' This function enables the use of C++ implementations for the Markov
#' DP (HMM) sampling algorithms when available.
#'
#' @param use_cpp Logical indicating whether to use C++ implementations
#' @export
enable_cpp_markov_samplers <- function(use_cpp = TRUE) {
  options(dirichletprocesscpp.use_cpp_markov = use_cpp)

  if (use_cpp) {
    message("C++ samplers enabled for Markov Dirichlet processes (HMM)")
  } else {
    message("Using R implementations for Markov samplers")
  }

  invisible(use_cpp)
}

#' Check if C++ Markov samplers are enabled
#'
#' @return Logical indicating if C++ Markov samplers are enabled
#' @export
using_cpp_markov_samplers <- function() {
  getOption("dirichletprocesscpp.use_cpp_markov", FALSE)
}
