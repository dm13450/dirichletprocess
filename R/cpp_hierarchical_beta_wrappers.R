#' C++ Implementation Wrappers for Hierarchical Beta Distribution
#'
#' These functions provide access to the C++ implementations of the
#' hierarchical Beta Dirichlet process algorithms.
#'
#' @name cpp_hierarchical_beta_wrappers
#' @keywords internal
NULL

#' @rdname cpp_hierarchical_beta_wrappers
#' @param dpobjlist Hierarchical Dirichlet process object
#' @param its Number of iterations
#' @param updatePrior Whether to update prior parameters
#' @param progressBar Whether to show progress bar
#' @export
Fit.hierarchical.cpp <- function(dpObj, its, updatePrior = FALSE, progressBar = TRUE) {
  if (!inherits(dpObj, "hierarchical")) {
    stop("This C++ implementation is only for hierarchical Dirichlet processes")
  }

  # Check if all individual DPs are Beta type
  all_beta <- all(sapply(dpObj$indDP, function(x) inherits(x, "beta")))

  if (!all_beta) {
    stop("C++ implementation currently only supports hierarchical Beta DPs")
  }

  # Validate inputs
  if (its <= 0) {
    stop("Number of iterations must be positive")
  }

  # Deep copy to avoid modifying original object
  dpObj_copy <- dpObj

  # Convert 1-indexed R labels to 0-indexed C++ labels
  for (i in seq_along(dpObj_copy$indDP)) {
    # Check if labels exist and are valid
    if (!is.null(dpObj_copy$indDP[[i]]$clusterLabels) &&
        length(dpObj_copy$indDP[[i]]$clusterLabels) > 0) {
      labels <- dpObj_copy$indDP[[i]]$clusterLabels

      # Validate labels
      if (any(is.na(labels))) {
        stop(paste("Invalid cluster labels in DP", i, ": labels cannot be NA"))
      }
      if (min(labels) < 1) {
        stop(paste("Invalid cluster labels in DP", i, ": labels must be >= 1"))
      }

      # Convert to 0-indexed
      dpObj_copy$indDP[[i]]$clusterLabels <- as.integer(labels - 1)
    }
  }

  # Call C++ implementation with error handling
  result <- tryCatch({
    hierarchical_beta_fit_cpp(dpObj_copy, its, updatePrior, progressBar)
  }, error = function(e) {
    stop(paste("C++ fitting failed:", e$message))
  })

  # Convert back to 1-indexed
  for (i in seq_along(result$indDP)) {
    if (!is.null(result$indDP[[i]]$clusterLabels) &&
        length(result$indDP[[i]]$clusterLabels) > 0) {
      labels <- result$indDP[[i]]$clusterLabels
      result$indDP[[i]]$clusterLabels <- as.integer(labels + 1)
    }
  }

  return(result)
}

#' @rdname cpp_hierarchical_beta_wrappers
#' @export
ClusterComponentUpdate.hierarchical.cpp <- function(dpObj) {
  if (!inherits(dpObj, "hierarchical")) {
    stop("This C++ implementation is only for hierarchical Dirichlet processes")
  }

  # Deep copy
  dpObj_copy <- dpObj

  # Convert labels with validation
  for (i in seq_along(dpObj_copy$indDP)) {
    if (!is.null(dpObj_copy$indDP[[i]]$clusterLabels) &&
        length(dpObj_copy$indDP[[i]]$clusterLabels) > 0) {
      labels <- dpObj_copy$indDP[[i]]$clusterLabels

      # Validate
      if (any(is.na(labels))) {
        stop(paste("Invalid cluster labels in DP", i, ": labels cannot be NA"))
      }
      if (min(labels) < 1) {
        stop(paste("Invalid cluster labels in DP", i, ": labels must be >= 1"))
      }

      # Convert to 0-indexed
      dpObj_copy$indDP[[i]]$clusterLabels <- as.integer(labels - 1)
    }
  }

  # Call C++ implementation
  result <- tryCatch({
    hierarchical_beta_cluster_component_update_cpp(dpObj_copy)
  }, error = function(e) {
    stop(paste("C++ cluster component update failed:", e$message))
  })

  # Convert back
  for (i in seq_along(result$indDP)) {
    if (!is.null(result$indDP[[i]]$clusterLabels) &&
        length(result$indDP[[i]]$clusterLabels) > 0) {
      labels <- result$indDP[[i]]$clusterLabels
      result$indDP[[i]]$clusterLabels <- as.integer(labels + 1)
    }
  }

  return(result)
}

#' @rdname cpp_hierarchical_beta_wrappers
#' @export
GlobalParameterUpdate.hierarchical.cpp <- function(dpobjlist) {
  if (!inherits(dpobjlist, "hierarchical")) {
    stop("This C++ implementation is only for hierarchical Dirichlet processes")
  }

  # Deep copy
  dpObj_copy <- dpobjlist

  # Convert labels
  for (i in seq_along(dpObj_copy$indDP)) {
    if (!is.null(dpObj_copy$indDP[[i]]$clusterLabels) &&
        length(dpObj_copy$indDP[[i]]$clusterLabels) > 0) {
      labels <- dpObj_copy$indDP[[i]]$clusterLabels

      # Validate
      if (any(is.na(labels))) {
        stop(paste("Invalid cluster labels in DP", i, ": labels cannot be NA"))
      }
      if (min(labels) < 1) {
        stop(paste("Invalid cluster labels in DP", i, ": labels must be >= 1"))
      }

      # Convert to 0-indexed
      dpObj_copy$indDP[[i]]$clusterLabels <- as.integer(labels - 1)
    }
  }

  # Call C++ implementation
  result <- tryCatch({
    hierarchical_beta_global_parameter_update_cpp(dpObj_copy)
  }, error = function(e) {
    stop(paste("C++ global parameter update failed:", e$message))
  })

  # Convert back
  for (i in seq_along(result$indDP)) {
    if (!is.null(result$indDP[[i]]$clusterLabels) &&
        length(result$indDP[[i]]$clusterLabels) > 0) {
      labels <- result$indDP[[i]]$clusterLabels
      result$indDP[[i]]$clusterLabels <- as.integer(labels + 1)
    }
  }

  return(result)
}

#' @rdname cpp_hierarchical_beta_wrappers
#' @export
UpdateG0.cpp <- function(dpObj) {
  if (!inherits(dpObj, "hierarchical")) {
    stop("This C++ implementation is only for hierarchical Dirichlet processes")
  }

  # Deep copy
  dpObj_copy <- dpObj

  # Convert labels
  for (i in seq_along(dpObj_copy$indDP)) {
    if (!is.null(dpObj_copy$indDP[[i]]$clusterLabels) &&
        length(dpObj_copy$indDP[[i]]$clusterLabels) > 0) {
      labels <- dpObj_copy$indDP[[i]]$clusterLabels

      # Validate
      if (any(is.na(labels))) {
        stop(paste("Invalid cluster labels in DP", i, ": labels cannot be NA"))
      }
      if (min(labels) < 1) {
        stop(paste("Invalid cluster labels in DP", i, ": labels must be >= 1"))
      }

      # Convert to 0-indexed
      dpObj_copy$indDP[[i]]$clusterLabels <- as.integer(labels - 1)
    }
  }

  # Call C++ implementation
  result <- tryCatch({
    hierarchical_beta_update_g0_cpp(dpObj_copy)
  }, error = function(e) {
    stop(paste("C++ G0 update failed:", e$message))
  })

  # Convert back
  for (i in seq_along(result$indDP)) {
    if (!is.null(result$indDP[[i]]$clusterLabels) &&
        length(result$indDP[[i]]$clusterLabels) > 0) {
      labels <- result$indDP[[i]]$clusterLabels
      result$indDP[[i]]$clusterLabels <- as.integer(labels + 1)
    }
  }

  return(result)
}

#' @rdname cpp_hierarchical_beta_wrappers
#' @export
UpdateGamma.cpp <- function(dpObj) {
  if (!inherits(dpObj, "hierarchical")) {
    stop("This C++ implementation is only for hierarchical Dirichlet processes")
  }

  # Deep copy
  dpObj_copy <- dpObj

  # Convert labels
  for (i in seq_along(dpObj_copy$indDP)) {
    if (!is.null(dpObj_copy$indDP[[i]]$clusterLabels) &&
        length(dpObj_copy$indDP[[i]]$clusterLabels) > 0) {
      labels <- dpObj_copy$indDP[[i]]$clusterLabels

      # Validate
      if (any(is.na(labels))) {
        stop(paste("Invalid cluster labels in DP", i, ": labels cannot be NA"))
      }
      if (min(labels) < 1) {
        stop(paste("Invalid cluster labels in DP", i, ": labels must be >= 1"))
      }

      # Convert to 0-indexed
      dpObj_copy$indDP[[i]]$clusterLabels <- as.integer(labels - 1)
    }
  }

  # Call C++ implementation
  result <- tryCatch({
    hierarchical_beta_update_gamma_cpp(dpObj_copy)
  }, error = function(e) {
    stop(paste("C++ gamma update failed:", e$message))
  })

  # Convert back
  for (i in seq_along(result$indDP)) {
    if (!is.null(result$indDP[[i]]$clusterLabels) &&
        length(result$indDP[[i]]$clusterLabels) > 0) {
      labels <- result$indDP[[i]]$clusterLabels
      result$indDP[[i]]$clusterLabels <- as.integer(labels + 1)
    }
  }

  return(result)
}

#' @rdname cpp_hierarchical_beta_wrappers
#' @export
HierarchicalBetaCreate.cpp <- function(n, priorParameters, hyperPriorParameters,
                                       alphaPrior, maxT, gammaPrior,
                                       mhStepSize, num_sticks) {
  # Validate inputs
  if (n <= 0) stop("Number of datasets must be positive")
  if (num_sticks <= 0) stop("Number of sticks must be positive")
  if (maxT <= 0) stop("maxT must be positive")

  tryCatch({
    hierarchical_beta_mixing_create_cpp(
      n = n,
      priorParameters = priorParameters,
      hyperPriorParameters = hyperPriorParameters,
      alphaPrior = alphaPrior,
      maxT = maxT,
      gammaPrior = gammaPrior,
      mhStepSize = mhStepSize,
      num_sticks = num_sticks
    )
  }, error = function(e) {
    stop(paste("C++ mixing distribution creation failed:", e$message))
  })
}

# Note: enable_cpp_hierarchical_samplers and using_cpp_hierarchical_samplers 
# are now defined in cpp_interface.R to avoid duplicate definitions
