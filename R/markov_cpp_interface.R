# R/markov_cpp_interface.R

#' Run Markov MCMC using C++ implementation
#' @param dp_obj Markov Dirichlet process object
#' @param its Number of iterations
#' @param update_prior Whether to update hyperparameters
#' @param progress_bar Whether to show progress
#' @return Updated DP object
#' @keywords internal
run_markov_mcmc_cpp_wrapper <- function(dp_obj, its, update_prior = FALSE, progress_bar = TRUE) {
  # Prepare data
  data <- as.matrix(dp_obj$data)

  # Prepare mixing distribution parameters
  mixing_params <- prepare_markov_mixing_params(dp_obj$mixingDistribution)

  # Prepare MCMC parameters
  mcmc_params <- list(
    n_iter = its,
    n_burn = floor(its * 0.1),  # 10% burn-in
    thin = 1,
    update_prior = update_prior,
    alpha = dp_obj$alpha,
    beta = dp_obj$beta,
    m_auxiliary = 3,  # Algorithm 8 auxiliary parameters
    alpha_prior_shape = 1,
    alpha_prior_rate = 1,
    beta_prior_shape = 1,
    beta_prior_rate = 1
  )

  # Run C++ MCMC - Call the actual C++ function
  result <- run_markov_mcmc_cpp(data, mixing_params, mcmc_params)

  # Update dp_obj with results
  dp_obj$states <- result$final_states
  dp_obj$params <- result$final_params
  dp_obj$uniqueParams <- result$final_unique_params
  dp_obj$alpha <- tail(result$alpha_chain, 1)[[1]]
  dp_obj$beta <- tail(result$beta_chain, 1)[[1]]
  dp_obj$alphaChain <- result$alpha_chain
  dp_obj$betaChain <- result$beta_chain
  dp_obj$statesChain <- result$states_chain
  dp_obj$paramChain <- result$params_chain

  return(dp_obj)
}

#' Prepare Markov mixing distribution parameters for C++
#' @param md Mixing distribution object
#' @return List of parameters for C++
#' @keywords internal
prepare_markov_mixing_params <- function(md) {
  if (inherits(md, "normal") || inherits(md, "gaussian")) {
    list(
      type = "gaussian",
      mu0 = ifelse(!is.null(md$priorParameters), md$priorParameters[1], 0),
      kappa0 = ifelse(!is.null(md$priorParameters), md$priorParameters[2], 1),
      alpha0 = ifelse(!is.null(md$priorParameters), md$priorParameters[3], 1),
      beta0 = ifelse(!is.null(md$priorParameters), md$priorParameters[4], 1)
    )
  } else if (inherits(md, "beta")) {
    list(
      type = "beta",
      alpha0 = md$priorParameters[1],
      beta0 = md$priorParameters[2]
    )
  } else if (inherits(md, "exponential")) {
    list(
      type = "exponential",
      rate_shape = md$priorParameters[1],
      rate_rate = md$priorParameters[2]
    )
  } else {
    stop("Mixing distribution not yet implemented for Markov MCMC: ", class(md))
  }
}
