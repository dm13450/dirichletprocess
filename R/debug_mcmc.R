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
#' @keywords internal
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

  if (isTRUE(verbose)) {
    message("Debug MCMC Results:")
    message("Final clusters: ", result$final_n_clusters)
    message("Cluster evolution: ", paste(result$n_clusters, collapse = " "))
    message("Alpha evolution: ", paste(round(result$alpha, 3), collapse = " "))
  }

  return(result)
}

#' Debug MCMC clustering behavior
#'
#' @param data Input data for clustering diagnosis
#' @param n_iter Number of MCMC iterations (default: 100)
#' @param alpha Concentration parameter (default: 1.0)
#' @keywords internal
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

  message("Cluster evolution:")
  message("Iterations 1-10: ",
          paste(result$n_clusters[1:min(10, length(result$n_clusters))],
                collapse = " "))
  message("Final clusters: ", tail(result$n_clusters, 1))
  message("Alpha evolution: ",
          paste(round(result$alpha[c(1, length(result$alpha))], 3),
                collapse = " "))

  invisible(result)
}
