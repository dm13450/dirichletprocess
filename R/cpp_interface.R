#' @title C++ Backend Interface Functions
#' @description Functions for interfacing with C++ implementations
#' @name cpp_interface
NULL

cpp_use_override <- function() {
  getOption("dirichletprocesscpp.use_cpp", NULL)
}

#' Set whether to use C++ implementations
#' @param use_cpp Logical indicating whether to force C++ (`TRUE`) or R (`FALSE`)
#'   implementations. Use `NULL` to restore automatic selection for supported
#'   non-hierarchical `Fit()` paths.
#' @details `set_use_cpp()` controls the ordinary non-hierarchical
#'   \code{Fit()} auto-selection logic between validated R and C++ sampler
#'   paths. It does not make hierarchical or HMM constructor \code{cpp}
#'   arguments live routing selectors.
#'
#'   Use \code{TRUE} to force validated ordinary C++ paths where supported,
#'   \code{FALSE} to force the R paths, or \code{NULL} to restore automatic
#'   selection.
#' @return Previous setting (invisibly).
#' @examples
#' old_setting <- set_use_cpp(TRUE)
#' set_use_cpp(old_setting)
#' @export
set_use_cpp <- function(use_cpp = NULL) {
  restore_override <- attr(use_cpp, "dirichletprocesscpp.use_cpp.override", exact = TRUE)

  if (!is.null(restore_override) && length(use_cpp) == 1L && is.logical(use_cpp)) {
    use_cpp <- restore_override
  }

  if (!is.null(use_cpp)) {
    if (!is.logical(use_cpp) || length(use_cpp) != 1L || is.na(use_cpp)) {
      stop("'use_cpp' must be TRUE, FALSE, or NULL.", call. = FALSE)
    }
  }

  old <- cpp_use_override()
  options(dirichletprocesscpp.use_cpp = use_cpp)
  invisible(old)
}

#' Check if using C++ implementations
#' @return Logical indicating whether the package is explicitly forced to use the
#'   C++ implementation. The raw override value is attached so
#'   `set_use_cpp(using_cpp())` restores automatic mode correctly.
#' @export
using_cpp <- function() {
  override <- cpp_use_override()
  out <- isTRUE(override)
  attr(out, "dirichletprocesscpp.use_cpp.override") <- override
  out
}

#' Get C++ implementation status
#' @return List showing which C++ implementations are available
#' @export
get_cpp_status <- function() {
  has_cpp <- exists("_dirichletprocess_run_mcmc_cpp")

  status <- list(
    mcmc_runner = has_cpp,
    gaussian_likelihood = has_cpp,
    exponential_likelihood = exists("_dirichletprocess_run_mcmc_cpp"),
    beta_likelihood = has_cpp,
    mvnormal_likelihood = exists("conjugate_mvnormal_cluster_component_update_cpp"),
    weibull_likelihood = has_cpp,
    hierarchical_beta = exists("_dirichletprocess_hierarchical_beta_fit_cpp"),
    markov = exists("_dirichletprocess_markov_dp_fit_cpp"),
    available = has_cpp
  )

  attr(status, "message") <- if (has_cpp) {
    "C++ backend is available with all distributions"
  } else {
    "C++ backend is not available - using R implementation"
  }

  return(status)
}

#' Check if C++ can be used for a given DP object
#' @param dp_obj Dirichlet process object
#' @return Logical indicating whether C++ implementation is available
#' @export
can_use_cpp <- function(dp_obj = NULL) {
  ns <- getNamespace("dirichletprocess")

  if (is.null(dp_obj)) {
    # If no dp_obj provided, just check if C++ is available
    return(exists("_dirichletprocess_run_mcmc_cpp", where = ns))
  }

  if (!exists("_dirichletprocess_run_mcmc_cpp", where = ns)) {
    return(FALSE)
  }

  !is.null(mixing_distribution_supported_cpp_class(dp_obj$mixingDistribution))
}

should_use_cpp_fit <- function(dp_obj) {
  if (!can_use_cpp(dp_obj)) {
    return(FALSE)
  }

  override <- cpp_use_override()

  if (identical(override, FALSE)) {
    return(FALSE)
  }

  TRUE
}

#' Run MCMC using C++ implementation
#' @param data Data matrix
#' @param mixing_dist_params Mixing distribution parameters
#' @param mcmc_params MCMC parameters
#' @return List with MCMC results
#' @keywords internal
run_mcmc_cpp <- function(data, mixing_dist_params, mcmc_params) {
  # Ensure required parameters
  if (!"m_auxiliary" %in% names(mcmc_params)) {
    mcmc_params$m_auxiliary <- 3  # Default for Algorithm 8
  }

  if (!"alpha" %in% names(mcmc_params)) {
    mcmc_params$alpha <- 1.0
  }

  if (!"update_concentration" %in% names(mcmc_params)) {
    mcmc_params$update_concentration <- TRUE
  }

  # Call C++ implementation
  result <- .Call("_dirichletprocess_run_mcmc_cpp",
                  data, mixing_dist_params, mcmc_params,
                  PACKAGE = "dirichletprocess")

  # Convert results to match R format when history is present.
  if ("labels_chain" %in% names(result)) {
    result$labelsChain <- lapply(seq_len(nrow(result$labels_chain)), function(i) {
      result$labels_chain[i, ]
    })
  }

  if ("alpha_chain" %in% names(result)) {
    result$alphaChain <- as.numeric(result$alpha_chain)
  }

  if ("likelihood_chain" %in% names(result)) {
    result$likelihoodChain <- as.numeric(result$likelihood_chain)
  }

  return(result)
}

run_gaussian_fit_cpp_batch <- function(data, mixing_dist_params, mcmc_params) {
  result <- .Call("_dirichletprocess_run_gaussian_fit_cpp",
                  data, mixing_dist_params, mcmc_params,
                  PACKAGE = "dirichletprocess")

  if ("labels_chain" %in% names(result)) {
    result$labelsChain <- lapply(seq_len(nrow(result$labels_chain)), function(i) {
      as.integer(result$labels_chain[i, ])
    })
  }

  if ("alpha_chain" %in% names(result)) {
    result$alphaChain <- as.numeric(result$alpha_chain)
  }

  if ("likelihood_chain" %in% names(result)) {
    result$likelihoodChain <- as.numeric(result$likelihood_chain)
  }

  result
}

run_normal_fixed_variance_fit_cpp_batch <- function(data, mixing_dist_params, mcmc_params) {
  result <- .Call("_dirichletprocess_run_normal_fixed_variance_fit_cpp",
                  data, mixing_dist_params, mcmc_params,
                  PACKAGE = "dirichletprocess")

  if ("labels_chain" %in% names(result)) {
    result$labelsChain <- lapply(seq_len(nrow(result$labels_chain)), function(i) {
      as.integer(result$labels_chain[i, ])
    })
  }

  if ("alpha_chain" %in% names(result)) {
    result$alphaChain <- as.numeric(result$alpha_chain)
  }

  if ("likelihood_chain" %in% names(result)) {
    result$likelihoodChain <- as.numeric(result$likelihood_chain)
  }

  result
}

run_exponential_fit_cpp_batch <- function(data, mixing_dist_params, mcmc_params) {
  result <- .Call("_dirichletprocess_run_exponential_fit_cpp",
                  data, mixing_dist_params, mcmc_params,
                  PACKAGE = "dirichletprocess")

  if ("labels_chain" %in% names(result)) {
    result$labelsChain <- lapply(seq_len(nrow(result$labels_chain)), function(i) {
      as.integer(result$labels_chain[i, ])
    })
  }

  if ("alpha_chain" %in% names(result)) {
    result$alphaChain <- as.numeric(result$alpha_chain)
  }

  if ("likelihood_chain" %in% names(result)) {
    result$likelihoodChain <- as.numeric(result$likelihood_chain)
  }

  result
}

run_mvnormal_fit_cpp_batch <- function(data, mixing_dist_params, mcmc_params) {
  result <- .Call("_dirichletprocess_run_mvnormal_fit_cpp",
                  data, mixing_dist_params, mcmc_params,
                  PACKAGE = "dirichletprocess")

  if ("labels_chain" %in% names(result)) {
    result$labelsChain <- lapply(seq_len(nrow(result$labels_chain)), function(i) {
      as.integer(result$labels_chain[i, ])
    })
  }

  if ("alpha_chain" %in% names(result)) {
    result$alphaChain <- as.numeric(result$alpha_chain)
  }

  if ("likelihood_chain" %in% names(result)) {
    result$likelihoodChain <- as.numeric(result$likelihood_chain)
  }

  result
}

safe_matrix_inverse <- function(mat) {
  tryCatch(solve(mat), error = function(e) qr.solve(mat))
}

flatten_symmetric_upper_rowmajor <- function(mat) {
  d <- nrow(mat)
  out <- numeric(d * (d + 1) / 2)
  idx <- 1L

  for (i in seq_len(d)) {
    for (j in i:d) {
      out[idx] <- mat[i, j]
      idx <- idx + 1L
    }
  }

  out
}

unflatten_symmetric_upper_rowmajor <- function(values, d) {
  mat <- matrix(0, nrow = d, ncol = d)
  idx <- 1L

  for (i in seq_len(d)) {
    for (j in i:d) {
      mat[i, j] <- values[idx]
      mat[j, i] <- values[idx]
      idx <- idx + 1L
    }
  }

  mat
}

cpp_cluster_parameters_from_dp <- function(dp_obj) {
  md <- dp_obj$mixingDistribution
  cluster_params <- dp_obj$clusterParameters
  n_clusters <- dp_obj$numberClusters

  if (is.null(cluster_params) || is.null(n_clusters) || n_clusters <= 0) {
    return(list())
  }

  if (inherits(md, "beta") || inherits(md, "beta2")) {
    return(lapply(seq_len(n_clusters), function(k) {
      c(cluster_params[[1]][1, 1, k], cluster_params[[2]][1, 1, k])
    }))
  }

  if (inherits(md, "normal_inverse_gamma") || inherits(md, "normal")) {
    return(lapply(seq_len(n_clusters), function(k) {
      c(cluster_params[[1]][1, 1, k], cluster_params[[2]][1, 1, k]^2)
    }))
  }

  if (inherits(md, "normalFixedVariance")) {
    return(lapply(seq_len(n_clusters), function(k) {
      c(cluster_params[[1]][1, 1, k])
    }))
  }

  if (inherits(md, "exponential")) {
    return(lapply(seq_len(n_clusters), function(k) {
      c(cluster_params[[1]][1, 1, k])
    }))
  }

  if (inherits(md, "weibull")) {
    return(lapply(seq_len(n_clusters), function(k) {
      c(cluster_params[[1]][1, 1, k], cluster_params[[2]][1, 1, k])
    }))
  }

  if (inherits(md, "mvnormal")) {
    return(lapply(seq_len(n_clusters), function(k) {
      mu_k <- as.numeric(cluster_params$mu[, , k, drop = TRUE])
      sig_k <- cluster_params$sig[, , k, drop = TRUE]
      prec_k <- safe_matrix_inverse(sig_k)
      c(mu_k, as.numeric(prec_k))
    }))
  }

  if (inherits(md, "mvnormal2")) {
    return(lapply(seq_len(n_clusters), function(k) {
      mu_k <- as.numeric(cluster_params$mu[, , k, drop = TRUE])
      sig_k <- cluster_params$sig[, , k, drop = TRUE]
      c(mu_k, flatten_symmetric_upper_rowmajor(sig_k))
    }))
  }

  stop("Initial state conversion not yet implemented in C++ wrapper for: ",
       mixing_distribution_display_class(md))
}

prepare_initial_state <- function(dp_obj) {
  if (is.null(dp_obj$clusterLabels) || is.null(dp_obj$clusterParameters)) {
    return(NULL)
  }

  list(
    initial_cluster_labels = as.integer(dp_obj$clusterLabels) - 1L,
    initial_cluster_params = cpp_cluster_parameters_from_dp(dp_obj)
  )
}

reconstruct_cluster_parameters_from_cpp <- function(dp_obj, cpp_params) {
  md <- dp_obj$mixingDistribution
  n_clusters <- length(cpp_params)

  if (inherits(md, "beta") || inherits(md, "beta2")) {
    mu_vals <- vapply(cpp_params, function(x) x[1], numeric(1))
    nu_vals <- vapply(cpp_params, function(x) x[2], numeric(1))

    return(list(
      mu = array(mu_vals, dim = c(1, 1, n_clusters)),
      nu = array(nu_vals, dim = c(1, 1, n_clusters))
    ))
  }

  if (inherits(md, "normal_inverse_gamma") || inherits(md, "normal")) {
    mu_vals <- vapply(cpp_params, function(x) x[1], numeric(1))
    sigma_vals <- sqrt(pmax(vapply(cpp_params, function(x) x[2], numeric(1)), 0))

    return(list(
      array(mu_vals, dim = c(1, 1, n_clusters)),
      array(sigma_vals, dim = c(1, 1, n_clusters))
    ))
  }

  if (inherits(md, "normalFixedVariance")) {
    mu_vals <- vapply(cpp_params, function(x) x[1], numeric(1))
    return(list(array(mu_vals, dim = c(1, 1, n_clusters))))
  }

  if (inherits(md, "exponential")) {
    lambda_vals <- vapply(cpp_params, function(x) x[1], numeric(1))
    return(list(array(lambda_vals, dim = c(1, 1, n_clusters))))
  }

  if (inherits(md, "weibull")) {
    alpha_vals <- vapply(cpp_params, function(x) x[1], numeric(1))
    lambda_vals <- vapply(cpp_params, function(x) x[2], numeric(1))

    return(list(
      array(alpha_vals, dim = c(1, 1, n_clusters)),
      array(lambda_vals, dim = c(1, 1, n_clusters))
    ))
  }

  if (inherits(md, "mvnormal")) {
    d <- ncol(dp_obj$data)
    mu_arr <- array(0, dim = c(1, d, n_clusters))
    sig_arr <- array(0, dim = c(d, d, n_clusters))

    for (k in seq_len(n_clusters)) {
      param_k <- as.numeric(cpp_params[[k]])
      mu_k <- matrix(param_k[seq_len(d)], nrow = 1)
      prec_k <- matrix(param_k[(d + 1):(d + d * d)], nrow = d, ncol = d)
      sig_k <- safe_matrix_inverse(prec_k)

      mu_arr[, , k] <- mu_k
      sig_arr[, , k] <- sig_k
    }

    return(list(mu = mu_arr, sig = sig_arr))
  }

  if (inherits(md, "mvnormal2")) {
    mu0 <- dp_obj$mixingDistribution$priorParameters$mu0
    d <- ncol(dp_obj$data)
    mu_arr <- array(0, dim = c(nrow(mu0), ncol(mu0), n_clusters))
    sig_arr <- array(0, dim = c(d, d, n_clusters))
    upper_len <- d * (d + 1) / 2

    for (k in seq_len(n_clusters)) {
      param_k <- as.numeric(cpp_params[[k]])
      mu_k <- matrix(param_k[seq_len(d)], nrow = nrow(mu0), ncol = ncol(mu0))
      sig_k <- unflatten_symmetric_upper_rowmajor(
        param_k[(d + 1):(d + upper_len)],
        d
      )

      mu_arr[, , k] <- mu_k
      sig_arr[, , k] <- sig_k
    }

    return(list(mu = mu_arr, sig = sig_arr))
  }

  stop("Result reconstruction not yet implemented in C++ wrapper for: ",
       mixing_distribution_display_class(md))
}

update_dpobj_from_cpp_result <- function(dp_obj, results) {
  final_labels <- as.integer(results$cluster_labels[[length(results$cluster_labels)]]) + 1L
  final_params <- results$theta[[length(results$theta)]]

  dp_obj$clusterLabels <- final_labels
  dp_obj$alpha <- as.numeric(tail(results$alphaChain, 1))
  dp_obj$clusterParameters <- reconstruct_cluster_parameters_from_cpp(dp_obj,
                                                                     final_params)
  dp_obj$numberClusters <- length(unique(final_labels))
  dp_obj$pointsPerCluster <- as.numeric(table(factor(final_labels,
                                                     levels = seq_len(dp_obj$numberClusters))))
  dp_obj$weights <- dp_obj$pointsPerCluster / dp_obj$n

  dp_obj
}

weights_from_labels <- function(labels, n) {
  counts <- as.numeric(table(factor(labels, levels = seq_len(max(labels)))))
  counts / n
}

update_gaussian_dpobj_from_cpp_batch_result <- function(dp_obj, results) {
  labels_chain <- results$labelsChain
  alpha_chain <- as.numeric(results$alphaChain)
  likelihood_chain <- as.numeric(results$likelihoodChain)

  cluster_parameters_chain <- lapply(results$theta_chain, function(params) {
    reconstruct_cluster_parameters_from_cpp(dp_obj, params)
  })

  weights_chain <- lapply(labels_chain, weights_from_labels, n = dp_obj$n)
  prior_parameters_chain <- replicate(
    length(alpha_chain),
    unserialize(serialize(dp_obj$mixingDistribution$priorParameters, NULL)),
    simplify = FALSE
  )

  final_labels <- as.integer(results$final_labels)
  final_params <- results$final_theta

  dp_obj$clusterLabels <- final_labels
  dp_obj$alpha <- as.numeric(results$final_alpha)
  dp_obj$clusterParameters <- reconstruct_cluster_parameters_from_cpp(dp_obj, final_params)
  dp_obj$numberClusters <- length(unique(final_labels))
  dp_obj$pointsPerCluster <- as.numeric(table(factor(final_labels,
                                                     levels = seq_len(dp_obj$numberClusters))))
  dp_obj$weights <- dp_obj$pointsPerCluster / dp_obj$n

  dp_obj$alphaChain <- alpha_chain
  dp_obj$likelihoodChain <- likelihood_chain
  dp_obj$weightsChain <- weights_chain
  dp_obj$clusterParametersChain <- cluster_parameters_chain
  dp_obj$priorParametersChain <- prior_parameters_chain
  dp_obj$labelsChain <- labels_chain

  dp_obj
}

update_normal_fixed_variance_dpobj_from_cpp_batch_result <- function(dp_obj, results) {
  update_gaussian_dpobj_from_cpp_batch_result(dp_obj, results)
}

update_exponential_dpobj_from_cpp_batch_result <- function(dp_obj, results) {
  update_gaussian_dpobj_from_cpp_batch_result(dp_obj, results)
}

update_mvnormal_dpobj_from_cpp_batch_result <- function(dp_obj, results) {
  update_gaussian_dpobj_from_cpp_batch_result(dp_obj, results)
}

#' Create mixing distribution parameters for C++
#' @param dp_obj Dirichlet process object
#' @return List of parameters formatted for C++
#' @keywords internal
prepare_mixing_dist_params <- function(dp_obj) {
  md <- dp_obj$mixingDistribution

  if (inherits(md, "exponential")) {
    list(
      type = "exponential",
      alpha0 = md$priorParameters[1],
      beta0 = md$priorParameters[2]
    )
  } else if (inherits(md, "weibull")) {
    # Extract Weibull parameters
    list(
      type = "weibull",
      phi = md$priorParameters[1],      # Upper bound for alpha
      alpha0 = md$priorParameters[2],   # Shape for Gamma prior on 1/lambda
      beta0 = md$priorParameters[3],    # Rate for Gamma prior on 1/lambda
      hyper_a1 = ifelse(length(md$hyperPriorParameters) >= 1,
                        md$hyperPriorParameters[1], 6.0),
      hyper_a2 = ifelse(length(md$hyperPriorParameters) >= 2,
                        md$hyperPriorParameters[2], 2.0),
      hyper_b1 = ifelse(length(md$hyperPriorParameters) >= 3,
                        md$hyperPriorParameters[3], 1.0),
      hyper_b2 = ifelse(length(md$hyperPriorParameters) >= 4,
                        md$hyperPriorParameters[4], 0.5),
      mh_step_alpha = ifelse(!is.null(md$mhStepSize), md$mhStepSize[1], 0.1),
      mh_draws = ifelse(!is.null(dp_obj$mhDraws), dp_obj$mhDraws, 100)
    )
  } else if (inherits(md, "mvnormal")) {
    # Extract MVNormal parameters
    if (!is.null(md$priorParameters)) {
      pp <- md$priorParameters
      params <- list(
        type = "mvnormal",
        mu0 = as.numeric(pp$mu0),
        kappa0 = as.numeric(pp$kappa0),
        Lambda = as.matrix(pp$Lambda),
        nu = as.numeric(pp$nu)
      )
      
      # Add covariance model if specified
      if (!is.null(pp$covModel)) {
        params$covModel = as.character(pp$covModel)
      }
      
      return(params)
    } else {
      stop("MVNormal mixing distribution missing prior parameters")
    }
  } else if (inherits(md, "mvnormal2")) {
    # Extract MVNormal2 parameters
    if (!is.null(md$priorParameters)) {
      pp <- md$priorParameters
      list(
        type = "mvnormal2",
        mu0 = as.matrix(pp$mu0),
        sigma0 = as.matrix(pp$sigma0),
        phi0 = as.matrix(pp$phi0),
        nu0 = as.numeric(pp$nu0),
        mh_draws = if (!is.null(dp_obj$mhDraws)) dp_obj$mhDraws else 250
      )
    } else {
      stop("MVNormal2 mixing distribution missing prior parameters")
    }
  } else if (inherits(md, "beta")) {
    # Beta distribution parameters
    list(
      type = "beta",
      alpha0 = md$priorParameters[1],
      beta0 = md$priorParameters[2],
      maxT = ifelse(is.null(md$maxT), 1, md$maxT),
      mhStepSize = md$mhStepSize,
      mh_draws = if (!is.null(dp_obj$mhDraws)) dp_obj$mhDraws else 250,
      hyperPriorParameters = md$hyperPriorParameters
    )
  } else if (inherits(md, "beta2")) {
    return(list(
      type = "beta2",
      gamma_prior = md$priorParameters[1],
      maxT = md$maxT,
      mh_step_size = md$mhStepSize,
      mh_draws = if (!is.null(dp_obj$mhDraws)) dp_obj$mhDraws else 250
    ))
  } else if (inherits(md, "normal_inverse_gamma") || inherits(md, "normal")) {
    # Gaussian parameters
    if (!is.null(md$priors)) {
      list(
        type = "gaussian",
        mu0 = md$priors$mu_0,
        kappa0 = md$priors$kappa_0,
        alpha0 = md$priors$alpha_0,
        beta0 = md$priors$beta_0
      )
    } else {
      list(
        type = "gaussian",
        mu0 = md$priorParameters[1],
        kappa0 = md$priorParameters[2],
        alpha0 = md$priorParameters[3],
        beta0 = md$priorParameters[4]
      )
    }
  } else if (inherits(md, "normalFixedVariance")) {
    return(list(
      type = "normalFixedVariance",
      mu0 = md$priorParameters[1],
      sigma0 = md$priorParameters[2],
      sigma = md$sigma
    ))
  } else if (inherits(md, "exponential")) {
    list(
      type = "exponential",
      alpha0 = md$priorParameters[1],
      beta0 = md$priorParameters[2]
    )
  } else {
    stop("Mixing distribution not yet implemented in C++: ", class(md))
  }
}

#' Create MCMC parameters for C++
#' @param dp_obj Dirichlet process object
#' @param its Number of iterations
#' @param updatePrior Whether to update prior parameters
#' @param n_burn Burn-in iterations
#' @param thin Thinning interval
#' @return List of MCMC parameters
#' @keywords internal
prepare_mcmc_params <- function(dp_obj, its, updatePrior, n_burn = 0, thin = 1,
                                store_history = TRUE) {
  mcmc_params <- list(
    n_iter = as.integer(its),
    n_burn = as.integer(n_burn),
    thin = as.integer(thin),
    update_concentration = TRUE,
    alpha = as.numeric(dp_obj$alpha),
    m_auxiliary = as.integer(if (is.null(dp_obj$m)) 3L else dp_obj$m),
    store_history = isTRUE(store_history)
  )

  # Add alpha prior parameters
  if (!is.null(dp_obj$alphaPriorParameters)) {
    mcmc_params$alpha_prior_shape <- dp_obj$alphaPriorParameters[1]
    mcmc_params$alpha_prior_rate <- dp_obj$alphaPriorParameters[2]
  } else {
    # Default Gamma(1,1) prior
    mcmc_params$alpha_prior_shape <- 1.0
    mcmc_params$alpha_prior_rate <- 1.0
  }

  initial_state <- prepare_initial_state(dp_obj)
  if (!is.null(initial_state)) {
    mcmc_params$initial_cluster_labels <- initial_state$initial_cluster_labels
    mcmc_params$initial_cluster_params <- initial_state$initial_cluster_params
  }

  return(mcmc_params)
}

#' Enable C++ implementations for specific samplers
#' @param enable Logical indicating whether to enable C++ samplers
#' @export
enable_cpp_samplers <- function(enable = TRUE) {
  if (missing(enable)) {
    # If no argument provided, return status (backward compatibility)
    return(invisible(exists("_dirichletprocess_run_mcmc_cpp", mode = "function")))
  }
  
  # Set option to force C++ usage
  options(dirichletprocesscpp.force_cpp_samplers = enable)
  invisible(enable)
}

#' Enable C++ implementations for hierarchical models
#' @param enable Logical indicating whether to enable hierarchical C++ samplers
#'   in the lower-level routing layer. This does not change the documented
#'   behavior of hierarchical constructor \code{cpp} arguments.
#' @export
enable_cpp_hierarchical_samplers <- function(enable = TRUE) {
  if (missing(enable)) {
    # If no argument provided, return status (backward compatibility)
    return(invisible(exists("_dirichletprocess_hierarchical_beta_fit_cpp", mode = "function")))
  }
  
  # Set option to force hierarchical C++ usage
  options(dirichletprocesscpp.force_cpp_hierarchical = enable)
  invisible(enable)
}

#' Check if using C++ samplers
#' @export
using_cpp_samplers <- function() {
  ns <- getNamespace("dirichletprocess")
  
  # Check if forced via options
  force_cpp <- getOption("dirichletprocesscpp.force_cpp_samplers", FALSE)
  if (force_cpp) {
    return(using_cpp() && exists("_dirichletprocess_run_mcmc_cpp", where = ns))
  }
  
  # Default behavior - check for existence of the compiled C++ function
  using_cpp() && exists("_dirichletprocess_run_mcmc_cpp", where = ns)
}

#' Check if using hierarchical C++ samplers
#' @export
using_cpp_hierarchical_samplers <- function() {
  ns <- getNamespace("dirichletprocess")
  
  # Check if forced via options
  force_hierarchical <- getOption("dirichletprocesscpp.force_cpp_hierarchical", NULL)
  if (!is.null(force_hierarchical)) {
    if (force_hierarchical) {
      return(using_cpp() && exists("_dirichletprocess_hierarchical_beta_fit_cpp", where = ns))
    } else {
      return(FALSE)  # Force R implementation
    }
  }
  
  # Default behavior - C++ functions from Rcpp are stored as "list" mode, not "function"
  using_cpp() && exists("_dirichletprocess_hierarchical_beta_fit_cpp", where = ns)
}
