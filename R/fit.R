#' Fit the Dirichlet process object
#'
#' Using Neal's algorithm 4 or 8 depending on conjugacy, the sampling
#' procedure for a Dirichlet process is carried out.
#'
#' Ordinary `Fit()` methods always update the current fitted state. When
#' `storeSamples = TRUE`, retained iterations generated during the current
#' `Fit()` call are appended to any existing retained history on the object.
#' `thinning` applies only to newly generated iterations from the current call
#' and keeps iterations `1, 1 + thinning, 1 + 2 * thinning, ...` from that
#' call. When `storeSamples = FALSE`, the sampler still runs and updates the
#' current fitted state, but no newly retained history is appended. Previously
#' retained samples are never re-thinned or deleted by later `Fit()` calls.
#'
#' When `updatePrior = TRUE` the parameters of the base measure are also
#' updated when supported by the mixing distribution.
#'
#' @param dpObj Initialised Dirichlet Process object
#' @param its Number of iterations to use
#' @param updatePrior Logical flag, defaults to FALSE. Set whether the parameters
#'        of the base measure are updated.
#' @param progressBar Logical flag indicating whether to display a progress bar.
#' @param storeSamples Logical flag indicating whether to retain MCMC samples in
#'        the fitted object. Defaults to `TRUE`. Retained samples are stored in
#'        the fitted object, not written to disk.
#' @param thinning Integer thinning interval applied only to iterations
#'        generated in the current `Fit()` call. Defaults to `1`.
#' @param ... Additional arguments
#' @return A fitted Dirichlet process-related object with updated current state
#'   and, when requested, appended retained sample history.
#'
#' @references Neal, R. M. (2000). Markov chain sampling methods for Dirichlet
#'             process mixture models. Journal of computational and graphical
#'             statistics, 9(2), 249-265.
#'
#' @examples
#' dp <- DirichletProcessGaussian(rnorm(20))
#' dp <- Fit(dp, 10, progressBar = FALSE)
#' dp <- Fit(dp, 6, progressBar = FALSE, thinning = 2)
#'
#' @export
Fit <- function(dpObj, its, updatePrior = FALSE, progressBar = TRUE,
                storeSamples = TRUE, thinning = 1, ...) {
  UseMethod("Fit", dpObj)
}

validate_fit_storage_args <- function(storeSamples, thinning) {
  if (!is.logical(storeSamples) || length(storeSamples) != 1L || is.na(storeSamples)) {
    stop("'storeSamples' must be TRUE or FALSE.", call. = FALSE)
  }

  if (length(thinning) != 1L || is.na(thinning) || !is.numeric(thinning) ||
      thinning < 1 || thinning != as.integer(thinning)) {
    stop("'thinning' must be a positive integer.", call. = FALSE)
  }

  list(
    storeSamples = isTRUE(storeSamples),
    thinning = as.integer(thinning)
  )
}

empty_dp_sample_storage <- function() {
  list(
    alphaChain = numeric(0),
    likelihoodChain = numeric(0),
    weightsChain = list(),
    clusterParametersChain = list(),
    priorParametersChain = list(),
    labelsChain = list()
  )
}

dp_sample_storage_from_object <- function(dpObj) {
  storage <- empty_dp_sample_storage()

  for (name in names(storage)) {
    if (!is.null(dpObj[[name]])) {
      storage[[name]] <- dpObj[[name]]
    }
  }

  storage$alphaChain <- as.numeric(storage$alphaChain)
  storage$likelihoodChain <- as.numeric(storage$likelihoodChain)
  storage
}

dp_existing_sample_storage <- function(dpObj) {
  storage <- dp_sample_storage_from_object(dpObj)

  stored_iterations <- dpObj$storedIterations
  if (is.null(stored_iterations)) {
    if (length(storage$alphaChain) > 0L) {
      stored_iterations <- seq_len(length(storage$alphaChain))
    } else {
      stored_iterations <- integer(0)
    }
  } else {
    stored_iterations <- as.integer(stored_iterations)
  }

  total_iterations <- dpObj$totalIterations
  if (is.null(total_iterations) || length(total_iterations) != 1L || is.na(total_iterations)) {
    if (length(stored_iterations) > 0L) {
      total_iterations <- max(stored_iterations)
    } else {
      total_iterations <- 0L
    }
  }

  storage$storedIterations <- as.integer(stored_iterations)
  storage$totalIterations <- as.integer(total_iterations)
  storage
}

dp_subset_sample_storage <- function(storage, keep_idx) {
  out <- empty_dp_sample_storage()

  if (length(keep_idx) == 0L) {
    return(out)
  }

  out$alphaChain <- as.numeric(storage$alphaChain[keep_idx])
  out$likelihoodChain <- as.numeric(storage$likelihoodChain[keep_idx])
  out$weightsChain <- storage$weightsChain[keep_idx]
  out$clusterParametersChain <- storage$clusterParametersChain[keep_idx]
  out$priorParametersChain <- storage$priorParametersChain[keep_idx]
  out$labelsChain <- storage$labelsChain[keep_idx]
  out
}

dp_apply_sample_storage <- function(dpObj, storage) {
  dpObj$alphaChain <- as.numeric(storage$alphaChain)
  dpObj$likelihoodChain <- as.numeric(storage$likelihoodChain)
  dpObj$weightsChain <- storage$weightsChain
  dpObj$clusterParametersChain <- storage$clusterParametersChain
  dpObj$priorParametersChain <- storage$priorParametersChain
  dpObj$labelsChain <- storage$labelsChain
  dpObj$storedIterations <- as.integer(storage$storedIterations)
  dpObj$totalIterations <- as.integer(storage$totalIterations)
  dpObj
}

dp_finalize_fit_sample_storage <- function(dpObj, storage_before, its,
                                           storeSamples, thinning) {
  keep_idx <- if (storeSamples) {
    seq.int(1L, as.integer(its), by = thinning)
  } else {
    integer(0)
  }

  new_storage <- dp_subset_sample_storage(
    dp_sample_storage_from_object(dpObj),
    keep_idx
  )

  total_before <- as.integer(storage_before$totalIterations)
  merged_storage <- list(
    alphaChain = c(storage_before$alphaChain, new_storage$alphaChain),
    likelihoodChain = c(storage_before$likelihoodChain, new_storage$likelihoodChain),
    weightsChain = c(storage_before$weightsChain, new_storage$weightsChain),
    clusterParametersChain = c(storage_before$clusterParametersChain,
                               new_storage$clusterParametersChain),
    priorParametersChain = c(storage_before$priorParametersChain,
                             new_storage$priorParametersChain),
    labelsChain = c(storage_before$labelsChain, new_storage$labelsChain),
    storedIterations = c(storage_before$storedIterations, total_before + keep_idx),
    totalIterations = total_before + as.integer(its)
  )

  dp_apply_sample_storage(dpObj, merged_storage)
}

#' @export
Fit.default <- function(dpObj, its, updatePrior = FALSE, progressBar = interactive(),
                        storeSamples = TRUE, thinning = 1, ...) {
  storage_args <- validate_fit_storage_args(storeSamples, thinning)
  storeSamples <- storage_args$storeSamples
  thinning <- storage_args$thinning
  storage_before <- dp_existing_sample_storage(dpObj)

  if (updatePrior) {
    AssertPriorUpdateSupported(dpObj$mixingDistribution)
  }

  if (progressBar) {
    pb <- txtProgressBar(min = 0, max = its, width = 50, char = "-", style = 3)
  }

  alphaChain <- numeric(its)
  likelihoodChain <- numeric(its)
  weightsChain <- vector("list", length = its)
  clusterParametersChain <- vector("list", length = its)
  priorParametersChain <- vector("list", length = its)
  labelsChain <- vector("list", length = its)

  for (i in seq_len(its)) {

    alphaChain[i] <- dpObj$alpha
    weightsChain[[i]] <- dpObj$pointsPerCluster / dpObj$n
    clusterParametersChain[[i]] <- dpObj$clusterParameters
    priorParametersChain[[i]] <- dpObj$mixingDistribution$priorParameters
    labelsChain[[i]] <- dpObj$clusterLabels

    likelihoodChain[i] <- sum(log(LikelihoodDP(dpObj)))

    dpObj <- ClusterComponentUpdate(dpObj)
    dpObj <- ClusterParameterUpdate(dpObj)
    dpObj <- UpdateAlpha(dpObj)

    if (updatePrior) {
      dpObj$mixingDistribution <- PriorParametersUpdate(dpObj$mixingDistribution,
                                                        dpObj$clusterParameters)
      if (inherits(dpObj, "conjugate")) {
        dpObj <- InitialisePredictive(dpObj)
      }
    }

    if (progressBar) {
      setTxtProgressBar(pb, i)
    }
  }

  dpObj$weights <- dpObj$pointsPerCluster / dpObj$n
  dpObj$alphaChain <- alphaChain
  dpObj$likelihoodChain <- likelihoodChain
  dpObj$weightsChain <- weightsChain
  dpObj$clusterParametersChain <- clusterParametersChain
  dpObj$priorParametersChain <- priorParametersChain
  dpObj$labelsChain <- labelsChain

  if (progressBar) {
    close(pb)
  }

  dp_finalize_fit_sample_storage(dpObj, storage_before, its, storeSamples, thinning)
}

#' @export
Fit.conjugate <- function(dpObj, its, updatePrior = FALSE, progressBar = interactive(),
                          storeSamples = TRUE, thinning = 1, ...) {
  # Use the preferred live C++ implementation automatically when supported,
  # unless explicitly forced back to R.
  if (should_use_cpp_fit(dpObj)) {
    return(Fit.dirichletprocess(dpObj, its, updatePrior, progressBar,
                                storeSamples = storeSamples,
                                thinning = thinning, ...))
  }

  # Otherwise use default R implementation
  return(Fit.default(dpObj, its, updatePrior, progressBar,
                     storeSamples = storeSamples,
                     thinning = thinning, ...))
}

#' @export
Fit.nonconjugate <- function(dpObj, its, updatePrior = FALSE, progressBar = interactive(),
                             storeSamples = TRUE, thinning = 1, ...) {
  # Use the preferred live C++ implementation automatically when supported,
  # unless explicitly forced back to R.
  if (should_use_cpp_fit(dpObj)) {
    return(Fit.dirichletprocess(dpObj, its, updatePrior, progressBar,
                                storeSamples = storeSamples,
                                thinning = thinning, ...))
  }

  # Otherwise use default R implementation
  return(Fit.default(dpObj, its, updatePrior, progressBar,
                     storeSamples = storeSamples,
                     thinning = thinning, ...))
}

#' @export
Fit.dirichletprocess <- function(dpObj, its, updatePrior = FALSE, progressBar = TRUE,
                                 storeSamples = TRUE, thinning = 1, ...) {
  if (!inherits(dpObj, "dirichletprocess")) {
    stop("dpObj must be a dirichletprocess object")
  }
  if (its <= 0) {
    stop("Number of iterations must be positive")
  }

  storage_args <- validate_fit_storage_args(storeSamples, thinning)
  storeSamples <- storage_args$storeSamples
  thinning <- storage_args$thinning
  storage_before <- dp_existing_sample_storage(dpObj)

  dots <- list(...)
  n_burn <- ifelse(is.null(dots$n_burn), 0, dots$n_burn)
  thin <- ifelse(is.null(dots$thin), 1, dots$thin)

  if (n_burn != 0 || thin != 1) {
    warning("The C++ Fit() path ignores 'n_burn' and 'thin' and stores every iteration to match the repaired R implementation.",
            call. = FALSE)
  }

  use_cpp <- should_use_cpp_fit(dpObj)

  if (use_cpp) {
    if (updatePrior) {
      AssertPriorUpdateSupported(dpObj$mixingDistribution)
    }

    tryCatch({
      if (is.null(dpObj$data) || is.null(dpObj$alpha)) {
        stop("Invalid dirichletprocess object: missing data or alpha")
      }

      if (is.null(dpObj$clusterLabels)) {
        dpObj$clusterLabels <- rep(1L, nrow(dpObj$data))
      }

      if (inherits(dpObj$mixingDistribution, c("normal_inverse_gamma", "normal"))) {
        if (progressBar) {
          pb <- txtProgressBar(min = 0, max = its, width = 50, char = "-", style = 3)
        }

        mixing_params <- prepare_mixing_dist_params(dpObj)
        mcmc_params <- prepare_mcmc_params(dpObj, its, updatePrior, 0L, 1L,
                                           store_history = FALSE)

        results <- run_gaussian_fit_cpp_batch(
          data = as.matrix(dpObj$data),
          mixing_dist_params = mixing_params,
          mcmc_params = mcmc_params
        )

        dpObj <- update_gaussian_dpobj_from_cpp_batch_result(dpObj, results)

        if (progressBar) {
          setTxtProgressBar(pb, its)
          close(pb)
        }

        return(dp_finalize_fit_sample_storage(dpObj, storage_before, its,
                                              storeSamples, thinning))
      }

      if (inherits(dpObj$mixingDistribution, "normalFixedVariance")) {
        if (progressBar) {
          pb <- txtProgressBar(min = 0, max = its, width = 50, char = "-", style = 3)
        }

        mixing_params <- prepare_mixing_dist_params(dpObj)
        mcmc_params <- prepare_mcmc_params(dpObj, its, updatePrior, 0L, 1L,
                                           store_history = FALSE)

        results <- run_normal_fixed_variance_fit_cpp_batch(
          data = as.matrix(dpObj$data),
          mixing_dist_params = mixing_params,
          mcmc_params = mcmc_params
        )

        dpObj <- update_normal_fixed_variance_dpobj_from_cpp_batch_result(dpObj, results)

        if (progressBar) {
          setTxtProgressBar(pb, its)
          close(pb)
        }

        return(dp_finalize_fit_sample_storage(dpObj, storage_before, its,
                                              storeSamples, thinning))
      }

      if (inherits(dpObj$mixingDistribution, "exponential")) {
        if (progressBar) {
          pb <- txtProgressBar(min = 0, max = its, width = 50, char = "-", style = 3)
        }

        mixing_params <- prepare_mixing_dist_params(dpObj)
        mcmc_params <- prepare_mcmc_params(dpObj, its, updatePrior, 0L, 1L,
                                           store_history = FALSE)

        results <- run_exponential_fit_cpp_batch(
          data = as.matrix(dpObj$data),
          mixing_dist_params = mixing_params,
          mcmc_params = mcmc_params
        )

        dpObj <- update_exponential_dpobj_from_cpp_batch_result(dpObj, results)

        if (progressBar) {
          setTxtProgressBar(pb, its)
          close(pb)
        }

        return(dp_finalize_fit_sample_storage(dpObj, storage_before, its,
                                              storeSamples, thinning))
      }

      if (inherits(dpObj$mixingDistribution, "mvnormal")) {
        if (progressBar) {
          pb <- txtProgressBar(min = 0, max = its, width = 50, char = "-", style = 3)
        }

        mixing_params <- prepare_mixing_dist_params(dpObj)
        mcmc_params <- prepare_mcmc_params(dpObj, its, updatePrior, 0L, 1L,
                                           store_history = FALSE)

        results <- run_mvnormal_fit_cpp_batch(
          data = as.matrix(dpObj$data),
          mixing_dist_params = mixing_params,
          mcmc_params = mcmc_params
        )

        dpObj <- update_mvnormal_dpobj_from_cpp_batch_result(dpObj, results)

        if (progressBar) {
          setTxtProgressBar(pb, its)
          close(pb)
        }

        return(dp_finalize_fit_sample_storage(dpObj, storage_before, its,
                                              storeSamples, thinning))
      }

      alphaChain <- numeric(its)
      likelihoodChain <- numeric(its)
      weightsChain <- vector("list", length = its)
      clusterParametersChain <- vector("list", length = its)
      priorParametersChain <- vector("list", length = its)
      labelsChain <- vector("list", length = its)

      if (progressBar) {
        pb <- txtProgressBar(min = 0, max = its, width = 50, char = "-", style = 3)
      }

      for (i in seq_len(its)) {
        alphaChain[i] <- dpObj$alpha
        weightsChain[[i]] <- dpObj$pointsPerCluster / dpObj$n
        clusterParametersChain[[i]] <- dpObj$clusterParameters
        priorParametersChain[[i]] <- dpObj$mixingDistribution$priorParameters
        labelsChain[[i]] <- dpObj$clusterLabels

        mixing_params <- prepare_mixing_dist_params(dpObj)
        mcmc_params <- prepare_mcmc_params(dpObj, 1L, updatePrior, 0L, 1L,
                                           store_history = FALSE)

        results <- run_mcmc_cpp(
          data = as.matrix(dpObj$data),
          mixing_dist_params = mixing_params,
          mcmc_params = mcmc_params
        )

        if (!is.null(results$likelihoodChain) && length(results$likelihoodChain) >= 1L) {
          likelihoodChain[i] <- as.numeric(results$likelihoodChain[[1]])
        } else {
          likelihoodChain[i] <- sum(log(LikelihoodDP(dpObj)))
        }

        dpObj <- update_dpobj_from_cpp_result(dpObj, results)

        if (updatePrior) {
          dpObj$mixingDistribution <- PriorParametersUpdate(dpObj$mixingDistribution,
                                                            dpObj$clusterParameters)
          if (inherits(dpObj, "conjugate")) {
            dpObj <- InitialisePredictive(dpObj)
          }
        }

        if (progressBar) {
          setTxtProgressBar(pb, i)
        }
      }

      dpObj$weights <- dpObj$pointsPerCluster / dpObj$n
      dpObj$alphaChain <- alphaChain
      dpObj$likelihoodChain <- likelihoodChain
      dpObj$weightsChain <- weightsChain
      dpObj$clusterParametersChain <- clusterParametersChain
      dpObj$priorParametersChain <- priorParametersChain
      dpObj$labelsChain <- labelsChain

      if (progressBar) {
        close(pb)
      }

      return(dp_finalize_fit_sample_storage(dpObj, storage_before, its,
                                            storeSamples, thinning))

    }, error = function(e) {
      warning("C++ implementation failed: ", e$message,
              "\nFalling back to R implementation")
      return(Fit.default(dpObj, its, updatePrior, progressBar,
                         storeSamples = storeSamples,
                         thinning = thinning, ...))
    })
  }

  # Use R implementation
  return(Fit.default(dpObj, its, updatePrior, progressBar,
                     storeSamples = storeSamples,
                     thinning = thinning, ...))
}

#' @export
Fit.hierarchical <- function(dpObj, its, updatePrior = FALSE, progressBar = interactive(), ...) {
  # Use C++ implementation if enabled and available
  if (using_cpp_hierarchical_samplers() && can_use_hierarchical_cpp(dpObj)) {
    return(Fit.hierarchical.cpp(dpObj, its, updatePrior, progressBar))
  }

  # Original R implementation
  if (progressBar) {
    pb <- txtProgressBar(min = 0, max = its, width = 50, char = "-", style = 3)
  }

  # Initialize storage arrays
  gammaValues <- numeric(its)
  gammaChain <- numeric(its)

  # Initialize alpha chains for each individual DP
  for (j in seq_along(dpObj$indDP)) {
    dpObj$indDP[[j]]$alphaChain <- numeric(its)
    dpObj$indDP[[j]]$likelihoodChain <- numeric(its)
    dpObj$indDP[[j]]$weightsChain <- vector("list", length = its)
    dpObj$indDP[[j]]$clusterParametersChain <- vector("list", length = its)
    dpObj$indDP[[j]]$labelsChain <- vector("list", length = its)
  }

  # Initialize global parameter storage
  globalParametersChain <- vector("list", length = its)
  globalStickChain <- vector("list", length = its)

  for (i in seq_len(its)) {
    # Update cluster components for each individual DP
    dpObj <- ClusterComponentUpdate(dpObj)

    # Update cluster parameters for each individual DP
    dpObj <- ClusterParameterUpdate(dpObj)

    # Update alpha for each individual DP
    dpObj <- UpdateAlpha(dpObj)

    # Update global parameters using all data
    dpObj <- GlobalParameterUpdate(dpObj)

    # Update G0 (the base distribution)
    dpObj <- UpdateG0(dpObj)

    # Update gamma (concentration parameter for G0)
    dpObj <- UpdateGamma(dpObj)

    # Store values for this iteration
    gammaValues[i] <- dpObj$gamma
    gammaChain[i] <- dpObj$gamma
    globalParametersChain[[i]] <- dpObj$globalParameters
    globalStickChain[[i]] <- dpObj$globalStick

    # Store individual DP values
    for (j in seq_along(dpObj$indDP)) {
      # Store alpha
      dpObj$indDP[[j]]$alphaChain[i] <- dpObj$indDP[[j]]$alpha

      # Calculate and store likelihood
      if (!is.null(dpObj$indDP[[j]]$data) && !is.null(dpObj$indDP[[j]]$clusterLabels)) {
        dpObj$indDP[[j]]$likelihoodChain[i] <- sum(log(LikelihoodDP(dpObj$indDP[[j]])))
      }

      # Store weights
      dpObj$indDP[[j]]$weightsChain[[i]] <- dpObj$indDP[[j]]$pointsPerCluster / dpObj$indDP[[j]]$n

      # Store cluster parameters
      dpObj$indDP[[j]]$clusterParametersChain[[i]] <- dpObj$indDP[[j]]$clusterParameters

      # Store labels
      dpObj$indDP[[j]]$labelsChain[[i]] <- dpObj$indDP[[j]]$clusterLabels

      # Update weights
      dpObj$indDP[[j]]$weights <- dpObj$indDP[[j]]$pointsPerCluster / dpObj$indDP[[j]]$n
    }

    # Update prior parameters if requested
    if (updatePrior) {
      # Get unique cluster parameters across all DPs
      allClusterParams <- list()
      for (j in seq_along(dpObj$indDP)) {
        if (!is.null(dpObj$indDP[[j]]$clusterParameters)) {
          allClusterParams <- c(allClusterParams,
                                list(dpObj$indDP[[j]]$clusterParameters))
        }
      }

      # Find unique parameters
      if (length(allClusterParams) > 0) {
        uniqueParams <- unique(unlist(lapply(allClusterParams, function(x) {
          if (is.list(x)) x[[1]] else x
        }), recursive = FALSE))

        clustParamLen <- length(uniqueParams)

        if (clustParamLen > 0) {
          # Extract global parameters up to the number of unique clusters
          clustParam <- lapply(dpObj$globalParameters, function(x) {
            if (is.array(x) && length(dim(x)) >= 3) {
              x[, , 1:min(clustParamLen, dim(x)[3]), drop = FALSE]
            } else {
              x
            }
          })

          # Update prior parameters using the first DP's mixing distribution
          tempMD <- PriorParametersUpdate(dpObj$indDP[[1]]$mixingDistribution, clustParam)

          # Apply updated prior parameters to all individual DPs
          for (j in seq_along(dpObj$indDP)) {
            dpObj$indDP[[j]]$mixingDistribution$priorParameters <- tempMD$priorParameters
          }
        }
      }
    }

    if (progressBar) {
      setTxtProgressBar(pb, i)
    }
  }

  # Store all chains in the dpObj
  dpObj$gammaValues <- gammaValues
  dpObj$gammaChain <- gammaChain
  dpObj$globalParametersChain <- globalParametersChain
  dpObj$globalStickChain <- globalStickChain

  # Ensure each individual DP has the correct numberClusters as a scalar
  for (j in seq_along(dpObj$indDP)) {
    if (!is.null(dpObj$indDP[[j]]$clusterLabels)) {
      dpObj$indDP[[j]]$numberClusters <- length(unique(dpObj$indDP[[j]]$clusterLabels))
    }
  }

  if (progressBar) {
    close(pb)
  }

  return(dpObj)
}

#' @export
Fit.hierarchical.cpp <- function(dpObj, its, updatePrior = FALSE, progressBar = interactive(), ...) {
  if (!can_use_hierarchical_cpp(dpObj)) {
    stop("C++ implementation not available for this hierarchical DP type")
  }

  # Use the appropriate C++ implementation based on distribution type
  if (all(sapply(dpObj$indDP, function(x) inherits(x, "beta")))) {
    # Use hierarchical Beta C++ implementation
    result <- run_hierarchical_mcmc_cpp(
      dpObj,
      n_iter = its,
      n_burn = 0,  # No burn-in for regular Fit
      thin = 1,
      update_prior = updatePrior,
      progress_bar = progressBar
    )
  } else if (all(sapply(dpObj$indDP, function(x) inherits(x, "mvnormal2")))) {
    # Use hierarchical MVNormal2 C++ implementation (disable progress bar to avoid R implementation)
    result <- hierarchical_mvnormal2_fit_cpp(
      dpObj,
      iterations = its,
      updatePrior = updatePrior,
      progressBar = FALSE  # Disable to ensure C++ is used
    )
  } else {
    stop("Mixed distribution types not supported in hierarchical C++ implementation")
  }

  # The result from run_hierarchical_mcmc_cpp should already have the updated dpObj
  # Ensure all fields are properly set

  # Make sure numberClusters is scalar for each individual DP
  for (j in seq_along(result$indDP)) {
    if (!is.null(result$indDP[[j]]$clusterLabels)) {
      result$indDP[[j]]$numberClusters <- as.integer(length(unique(result$indDP[[j]]$clusterLabels)))
    }

    # Ensure weights are calculated
    if (!is.null(result$indDP[[j]]$pointsPerCluster) && !is.null(result$indDP[[j]]$n)) {
      result$indDP[[j]]$weights <- result$indDP[[j]]$pointsPerCluster / result$indDP[[j]]$n
    }
  }

  # Ensure gamma is set to the last value if we have samples
  if (!is.null(result$gammaValues) && length(result$gammaValues) > 0) {
    result$gamma <- result$gammaValues[length(result$gammaValues)]
  }

  # Set gammaChain as alias for gammaValues for compatibility
  if (!is.null(result$gammaValues)) {
    result$gammaChain <- result$gammaValues
  }

  return(result)
}

#' @export
Fit.markov <- function(dpObj, its, updatePrior = FALSE, progressBar = interactive(), ...) {
  # Similar pattern - check for C++ then fall back to R
  if (using_cpp() && exists("_dirichletprocess_markov_dp_fit_cpp")) {
    return(Fit.markov.cpp(dpObj, its, updatePrior, progressBar))
  }

  # R implementation would go here
  return(dpObj)
}
