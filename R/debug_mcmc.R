# R/debug_mcmc.R

#' Create Gaussian parameters for debugging
#' @keywords internal
create_gaussian_params <- function() {
  list(
    type = "gaussian",
    mu = 0,
    sigma2 = 1
  )
}

#' Debug MCMC C++ implementation
#'
#' @param data Input data for MCMC debugging
#' @param n_iter Number of MCMC iterations (default: 10)
#' @param verbose Whether to output verbose debugging information (default: TRUE)
#' @export
debug_mcmc_cpp <- function(data, n_iter = 10, verbose = TRUE) {
  if (!exists("_dirichletprocess_run_mcmc_cpp")) {
    stop("C++ implementation not compiled")
  }

  data_matrix <- matrix(data, ncol = 1)

  mcmc_params <- list(
    n_iter = n_iter,
    n_burn = 0,
    thin = 1,
    update_concentration = TRUE,
    alpha = 1.0,
    m_auxiliary = 3
  )

  dist_params <- list(
    type = "gaussian",
    mu0 = mean(data),
    kappa0 = 0.01,
    alpha0 = 2.0,
    beta0 = var(data)
  )

  result <- run_mcmc_cpp(data_matrix, dist_params, mcmc_params)

  if (verbose) {
    cat("Debug MCMC Results:\n")
    cat("Final clusters:", result$final_n_clusters, "\n")
    cat("Cluster evolution:", result$n_clusters, "\n")
    cat("Alpha evolution:", round(result$alpha, 3), "\n")
  }

  return(result)
}

#' Debug MCMC clustering behavior
#'
#' @param data Input data for clustering diagnosis
#' @param n_iter Number of MCMC iterations (default: 100)
#' @param alpha Concentration parameter (default: 1.0)
#' @export
diagnose_clustering <- function(data, n_iter = 100, alpha = 1.0) {
  data_matrix <- matrix(data, ncol = 1)

  result <- run_mcmc_cpp(
    data = data_matrix,
    mixing_dist_params = create_gaussian_params(),
    mcmc_params = list(
      n_iter = n_iter,
      n_burn = 0,
      thin = 1,
      update_concentration = TRUE,
      alpha = alpha
    )
  )

  # Print diagnostic information
  cat("Cluster evolution:\n")
  cat("Iterations 1-10:", result$n_clusters[1:min(10, length(result$n_clusters))], "\n")
  cat("Final clusters:", tail(result$n_clusters, 1), "\n")
  cat("Alpha evolution:", round(result$alpha[c(1, length(result$alpha))], 3), "\n")

  invisible(result)
}
