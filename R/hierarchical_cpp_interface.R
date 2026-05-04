#' Run Hierarchical Beta MCMC using C++ implementation
#'
#' @param dp_list Hierarchical DP object (not a list of DirichletProcessBeta objects)
#' @param n_iter Number of MCMC iterations
#' @param n_burn Number of burn-in iterations
#' @param thin Thinning parameter
#' @param update_prior Whether to update prior parameters
#' @param progress_bar Show progress bar
#'
#' @return Updated hierarchical DP object
#' @export
run_hierarchical_mcmc_cpp <- function(dp_list, n_iter = 1000, n_burn = 100,
                                      thin = 1, update_prior = FALSE,
                                      progress_bar = TRUE) {

  # Validate inputs - dp_list should be a hierarchical DP object
  if (!inherits(dp_list, "hierarchical")) {
    stop("dp_list must be a hierarchical Dirichlet process object")
  }

  if (!all(sapply(dp_list$indDP, function(x) inherits(x, "beta")))) {
    stop("All individual DPs must be Beta type")
  }

  # Extract datasets
  datasets <- lapply(dp_list$indDP, function(dp) as.matrix(dp$data))

  # Prepare mixing distribution parameters
  first_dp <- dp_list$indDP[[1]]
  mixing_params <- list(
    type = "hierarchical_beta",
    alpha0 = first_dp$mixingDistribution$priorParameters[1],
    beta0 = first_dp$mixingDistribution$priorParameters[2],
    maxT = first_dp$mixingDistribution$maxT,
    gamma_prior_shape = dp_list$gammaPriors[1],
    gamma_prior_rate = dp_list$gammaPriors[2]
  )

  # MCMC parameters - use the first alpha value as default
  # The C++ code expects a single 'alpha' parameter
  mcmc_params <- list(
    n_iter = as.integer(n_iter),
    n_burn = as.integer(n_burn),
    thin = as.integer(thin),
    update_prior = update_prior,
    update_concentration = TRUE,
    m_auxiliary = 3L,  # For Algorithm 8
    alpha = as.numeric(dp_list$indDP[[1]]$alpha)  # Use first DP's alpha as default
  )

  # Call C++ implementation
  result <- .Call("_dirichletprocess_run_hierarchical_mcmc_cpp",
                  datasets, mixing_params, mcmc_params,
                  PACKAGE = "dirichletprocess")

  # Debug: Check what fields are in the result
  # cat("Result fields from C++:", names(result), "\n")

  # The C++ returns 'indDP' not 'individual_dps'
  # Update dp_list with results
  if (!is.null(result$indDP)) {
    for (i in seq_along(result$indDP)) {
      # Each indDP[i] should be a list with the DP fields
      if (is.list(result$indDP[[i]])) {
        # Copy all fields from result to dp_list
        for (field in names(result$indDP[[i]])) {
          dp_list$indDP[[i]][[field]] <- result$indDP[[i]][[field]]
        }

        # Ensure numberClusters is scalar
        if (!is.null(dp_list$indDP[[i]]$clusterLabels)) {
          dp_list$indDP[[i]]$numberClusters <- as.integer(
            length(unique(dp_list$indDP[[i]]$clusterLabels))
          )
        }

        # Calculate weights if needed
        if (!is.null(dp_list$indDP[[i]]$pointsPerCluster) &&
            !is.null(dp_list$indDP[[i]]$n)) {
          dp_list$indDP[[i]]$weights <- dp_list$indDP[[i]]$pointsPerCluster / dp_list$indDP[[i]]$n
        }
      }
    }
  }

  # Update global fields
  if (!is.null(result$globalParameters)) {
    dp_list$globalParameters <- result$globalParameters
  }

  if (!is.null(result$globalStick)) {
    dp_list$globalStick <- result$globalStick
  }

  if (!is.null(result$gammaValues)) {
    dp_list$gammaValues <- result$gammaValues
    # Update gamma to the last value
    if (length(result$gammaValues) > 0) {
      dp_list$gamma <- result$gammaValues[length(result$gammaValues)]
    }
  } else if (!is.null(result$gamma)) {
    dp_list$gamma <- result$gamma
  }

  return(dp_list)
}

#' Check if hierarchical C++ implementation is available
#'
#' @param dp_list Hierarchical DP object
#' @return Logical indicating availability
#' @export
can_use_hierarchical_cpp <- function(dp_list) {
  if (!inherits(dp_list, "hierarchical")) {
    return(FALSE)
  }

  # Phase 5 route lock:
  # the live hierarchical HDP path is now the rewritten R implementation.
  # Legacy hierarchical C++ samplers remain in the package but are disabled
  # from automatic live routing until they are rewritten coherently.
  FALSE
}

#' Update DP object from MCMC results
#' @param dp Original DP object
#' @param mcmc_result MCMC results from C++
#' @return Updated DP object
#' @keywords internal
update_dp_from_mcmc <- function(dp, mcmc_result) {
  # Handle the case where mcmc_result might be NULL or not a list
  if (is.null(mcmc_result) || !is.list(mcmc_result)) {
    return(dp)
  }

  if (!is.null(mcmc_result$cluster_labels)) {
    dp$clusterLabels <- mcmc_result$cluster_labels
  }

  if (!is.null(mcmc_result$cluster_params)) {
    dp$clusterParameters <- mcmc_result$cluster_params
  }

  if (!is.null(mcmc_result$n_clusters)) {
    # Ensure it's a scalar - handle both numeric and list inputs
    if (is.list(mcmc_result$n_clusters)) {
      dp$numberClusters <- as.integer(mcmc_result$n_clusters[[1]][1])
    } else {
      dp$numberClusters <- as.integer(mcmc_result$n_clusters[1])
    }
  }

  if (!is.null(mcmc_result$alpha)) {
    # Ensure it's a scalar - handle both numeric and list inputs
    if (is.list(mcmc_result$alpha)) {
      dp$alpha <- as.numeric(mcmc_result$alpha[[1]][1])
    } else {
      dp$alpha <- as.numeric(mcmc_result$alpha[1])
    }
  }

  if (!is.null(mcmc_result$weights)) {
    dp$weights <- mcmc_result$weights
  }

  # Copy any other fields that might be present
  other_fields <- setdiff(names(mcmc_result),
                          c("cluster_labels", "cluster_params", "n_clusters", "alpha", "weights"))
  for (field in other_fields) {
    dp[[field]] <- mcmc_result[[field]]
  }

  return(dp)
}
