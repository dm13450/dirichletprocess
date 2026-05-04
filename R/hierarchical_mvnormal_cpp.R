#' Run Hierarchical MVNormal MCMC using C++
#'
#' @param data_list List of data matrices
#' @param hdp_params List of HDP parameters
#' @param mcmc_params List of MCMC parameters
#' @return List with MCMC results
#' @export
run_hierarchical_mvnormal_mcmc_cpp <- function(data_list, hdp_params, mcmc_params) {

  # Validate inputs
  if (!is.list(data_list)) {
    stop("data_list must be a list of matrices")
  }

  # Convert data to matrices
  data_list <- lapply(data_list, as.matrix)

  # Set default MCMC parameters
  default_mcmc <- list(
    n_iter = 1000,
    n_burn = 100,
    thin = 1,
    update_prior = TRUE,
    show_progress = TRUE
  )

  mcmc_params <- modifyList(default_mcmc, mcmc_params)

  # Call C++ implementation
  .Call(`_dirichletprocess_hierarchical_mvnormal_run`,
        data_list, hdp_params, mcmc_params,
        PACKAGE = "dirichletprocess")
}

#' Create Hierarchical MVNormal Dirichlet Process
#'
#' @param data_list List of data matrices
#' @param prior_params Prior parameters for MVNormal-Wishart
#' @param alpha_prior Prior for local concentration parameters
#' @param gamma_prior Prior for global concentration parameter
#' @param n_sticks Number of stick-breaking components
#' @return Hierarchical DP object
#' @export
HierarchicalDirichletProcessMVNormal <- function(data_list,
                                                 prior_params,
                                                 alpha_prior = c(1, 1),
                                                 gamma_prior = c(1, 1),
                                                 n_sticks = 20) {

  # Prepare parameters
  hdp_params <- list(
    prior_params = prior_params,
    alpha_prior = alpha_prior,
    gamma_prior = gamma_prior,
    n_sticks = n_sticks
  )

  # Create object
  hdp_obj <- list(
    data_list = data_list,
    hdp_params = hdp_params,
    type = "hierarchical_mvnormal"
  )

  class(hdp_obj) <- c("hdp_mvnormal", "hdp", "dirichletprocess")

  return(hdp_obj)
}

#' Fit method for Hierarchical MVNormal DP
#'
#' @param dpObj Hierarchical DP object
#' @param its Number of MCMC iterations
#' @param updatePrior Whether to update prior parameters
#' @param progressBar Whether to show progress bar
#' @param ... Additional MCMC parameters
#' @return Updated HDP object with samples
#' @export
Fit.hdp_mvnormal <- function(dpObj, its = 1000, updatePrior = FALSE, progressBar = TRUE, ...) {

  # Prepare MCMC parameters
  mcmc_params <- list(
    n_iter = its,
    ...
  )

  # Run MCMC
  results <- run_hierarchical_mvnormal_mcmc_cpp(
    dpObj$data_list,
    dpObj$hdp_params,
    mcmc_params
  )

  # Update object
  dpObj$samples <- results$samples
  dpObj$final_state <- results$final_state

  return(dpObj)
}
