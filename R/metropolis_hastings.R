#' Metropolis-Hastings MCMC Sampler
#'
#' Performs Metropolis-Hastings sampling for non-conjugate Dirichlet process mixtures.
#' This function is used internally for parameter updates in non-conjugate models
#' where analytical posterior updates are not available.
#'
#' @param mixingDistribution A mixing distribution object
#' @param x Data for which to sample parameters
#' @param start_pos Starting position for the MCMC chain - a list of parameter arrays
#' @param no_draws Number of MCMC draws to perform (default: 100)
#'
#' @return A list containing:
#' \itemize{
#'   \item parameter_samples: List of parameter sample arrays
#'   \item accept_ratio: Acceptance ratio of the MCMC chain
#' }
#'
#' @details This function implements the Metropolis-Hastings algorithm for sampling
#' from posterior distributions in non-conjugate Dirichlet process mixtures.
#' Different mixing distributions may have specialized implementations.
#'
#' @references Metropolis, N., et al. (1953). Equation of state calculations by fast computing machines.
#'             Journal of Chemical Physics, 21(6), 1087-1092.
#'
#' @export
MetropolisHastings <- function(mixingDistribution, x, start_pos, no_draws=100){
  UseMethod("MetropolisHastings", mixingDistribution)
}

#' @export
MetropolisHastings.default <- function(mixingDistribution, x, start_pos, no_draws = 100) {
  parameter_samples <- vector("list", length(start_pos))
  for (i in seq_along(start_pos)) {
    parameter_samples[[i]] <- array(dim = c(dim(start_pos[[i]])[1:2], no_draws))
    parameter_samples[[i]][, , 1] <- start_pos[[i]][, , 1]
  }

  accept_count <- 0
  old_param <- start_pos

  old_prior <- log(PriorDensity(mixingDistribution, old_param))
  old_Likelihood <- sum(log(Likelihood(mixingDistribution, x, old_param)))

  for (i in seq_len(no_draws - 1)) {

    prop_param <- MhParameterProposal(mixingDistribution, old_param)

    new_prior <- log(PriorDensity(mixingDistribution, prop_param))
    new_Likelihood <- sum(log(Likelihood(mixingDistribution, x, prop_param)))

    accept_prob <- min(1, exp(new_prior + new_Likelihood - old_prior - old_Likelihood))

    if (is.na(accept_prob) || !is.finite(accept_prob) ) {
      accept_prob <- 0
    }

    if (runif(1) < accept_prob) {
      accept_count <- accept_count + 1
      sampled_param <- prop_param
      old_Likelihood <- new_Likelihood
      old_prior <- new_prior
    } else {
      sampled_param <- old_param
    }

    old_param <- sampled_param
    for (j in seq_along(start_pos)) {
      parameter_samples[[j]][, , i + 1] <- sampled_param[[j]]
    }
  }

  accept_ratio <- accept_count/no_draws

  return(list(parameter_samples = parameter_samples, accept_ratio = accept_ratio))
}

#' @export
MetropolisHastings.weibull <- function(mixingDistribution, x, start_pos, no_draws=100){

  lamSamp <- 1/rgamma(1, length(x)+mixingDistribution$priorParameters[2],
                      sum(x^c(start_pos[[1]])) + mixingDistribution$priorParameters[3])
  start_pos[[2]] <- array(lamSamp, dim=c(1,1,1))

  parameter_samples <- list()
  for (i in seq_along(start_pos)) {
    parameter_samples[[i]] <- array(dim = c(dim(start_pos[[i]])[1:2], no_draws))
    parameter_samples[[i]][, , 1] <- start_pos[[i]][, , 1]
  }

  accept_count <- 0
  old_param <- start_pos

  old_prior <- log(PriorDensity(mixingDistribution, old_param))
  old_Likelihood <- sum(log(Likelihood(mixingDistribution, x, old_param)))

  for(i in seq_len(no_draws-1)){

    # Gibbs update for lambda conditional on the current alpha.
    lamSamp <- 1/rgamma(1, length(x)+mixingDistribution$priorParameters[2],
                        sum(x^c(old_param[[1]])) + mixingDistribution$priorParameters[3])
    old_param[[2]] <- array(lamSamp, dim=c(1,1,1))

    old_prior <- log(PriorDensity(mixingDistribution, old_param))
    old_Likelihood <- sum(log(Likelihood(mixingDistribution, x, old_param)))

    # Symmetric random-walk proposal on alpha, with lambda held fixed.
    prop_param <- old_param
    prop_param[[1]] <- array(c(old_param[[1]]) +
                               mixingDistribution$mhStepSize * rnorm(1, 0, 1.7),
                             dim = c(1, 1, 1))

    new_prior <- log(PriorDensity(mixingDistribution, prop_param))
    new_Likelihood <- sum(log(Likelihood(mixingDistribution, x, prop_param)))

    accept_prob <- min(1, exp(new_prior + new_Likelihood - old_prior - old_Likelihood))

    if (is.na(accept_prob) || !is.finite(accept_prob)) {
      accept_prob <- 0
    }

    if (runif(1) < accept_prob) {
      accept_count <- accept_count + 1
      sampled_param <- prop_param
      old_Likelihood <- new_Likelihood
      old_prior <- new_prior
    } else {
      sampled_param <- old_param
    }

    old_param <- sampled_param
    for (j in seq_along(start_pos)) {
      parameter_samples[[j]][, , i + 1] <- sampled_param[[j]]
    }

  }
  accept_ratio <- accept_count/no_draws

  return(list(parameter_samples = parameter_samples, accept_ratio = accept_ratio))

}

#' @export
MetropolisHastings.list <- function(mixingDistribution, x, start_pos, no_draws = 100) {
  dist_class <- mixing_distribution_method_class(mixingDistribution,
                                                 "MetropolisHastings")
  if (!is.null(dist_class)) {
    method_func <- utils::getS3method("MetropolisHastings",
                                      dist_class,
                                      optional = TRUE)
    return(method_func(mixingDistribution, x, start_pos, no_draws))
  }

  return(MetropolisHastings.default(mixingDistribution, x, start_pos, no_draws))
}
