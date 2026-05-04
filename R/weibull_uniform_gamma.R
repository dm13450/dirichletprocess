#' Create a Weibull mixing distribution.
#'
#' See \code{\link{DirichletProcessWeibull}} for the default prior and hyper prior distributions.
#'
#' @param priorParameters Prior parameters for the Weibull parameters
#' @param mhStepSize Metropolis Hastings Step Size
#' @param hyperPriorParameters Parameters for the hyper-priors
#' @return A mixing distribution object.
#' @export
WeibullMixtureCreate <- function(priorParameters, mhStepSize,
                                 hyperPriorParameters = c(6, 2, 1, 0.5)) {

  mdObj <- MixingDistribution("weibull", priorParameters, "nonconjugate",
                              mhStepSize, hyperPriorParameters)
  return(mdObj)
}

weibull_extract_theta_values <- function(theta_component) {
  theta_dims <- dim(theta_component)

  if (is.null(theta_dims) || length(theta_dims) <= 2) {
    return(as.numeric(theta_component))
  }

  if (length(theta_dims) == 3) {
    return(theta_component[, , , drop = TRUE])
  }

  if (length(theta_dims) == 4) {
    return(theta_component[, , , , drop = TRUE])
  }

  as.numeric(theta_component)
}

#' @export
Likelihood.weibull <- function(mdObj, x, theta) {
  x <- as.vector(x, "numeric")

  if (!is.list(theta) || length(theta) < 2) {
    return(rep(0, length(x)))
  }

  alpha <- weibull_extract_theta_values(theta[[1]])
  lambda <- weibull_extract_theta_values(theta[[2]])

  out_length <- max(length(x), length(alpha), length(lambda))
  x_vals <- rep_len(x, out_length)
  alpha_vals <- rep_len(alpha, out_length)
  lambda_vals <- rep_len(lambda, out_length)

  y <- numeric(out_length)
  valid <- is.finite(x_vals) &
    is.finite(alpha_vals) &
    is.finite(lambda_vals) &
    alpha_vals > 0 &
    lambda_vals > 0 &
    x_vals >= 0

  if (any(valid)) {
    x_valid <- x_vals[valid]
    alpha_valid <- alpha_vals[valid]
    lambda_valid <- lambda_vals[valid]

    y[valid] <- (alpha_valid / lambda_valid) *
      x_valid^(alpha_valid - 1) *
      exp(-(x_valid^alpha_valid) / lambda_valid)
  }

  as.numeric(y)
}

#' @export
#' @rdname PriorDraw
PriorDraw.weibull <- function(mdObj, n = 1, ...) {

  priorParameters <- mdObj$priorParameters

  # Draw gamma values and handle potential NAs
  gamma_values <- rgamma(n, priorParameters[2], priorParameters[3])
  
  # Handle NA values that can occur with extreme parameters
  if (any(is.na(gamma_values))) {
    gamma_values[is.na(gamma_values)] <- 1.0  # Default to reasonable value
  }
  
  # Ensure we don't divide by zero
  gamma_values[gamma_values == 0] <- 1e-04
  
  lambdas <- 1/gamma_values
  
  # Draw uniform values and handle potential NAs
  alpha_values <- runif(n, 0, priorParameters[1])
  if (any(is.na(alpha_values))) {
    alpha_values[is.na(alpha_values)] <- 1.0  # Default to reasonable value
  }
  
  theta <- list(array(alpha_values, dim = c(1, 1, n)),
                array(lambdas, dim = c(1, 1, n)))
  return(theta)
}

#' @export
#' @rdname PriorDensity
PriorDensity.weibull <- function(mdObj, theta) {

  priorParameters <- mdObj$priorParameters
  alpha_dims <- dim(theta[[1]])
  lambda_dims <- dim(theta[[2]])
  alpha <- weibull_extract_theta_values(theta[[1]])
  lambda <- weibull_extract_theta_values(theta[[2]])

  out_length <- max(length(alpha), length(lambda))
  alpha_vals <- rep_len(alpha, out_length)
  lambda_vals <- rep_len(lambda, out_length)

  theta_density <- numeric(out_length)
  valid <- is.finite(alpha_vals) &
    is.finite(lambda_vals) &
    alpha_vals > 0 &
    lambda_vals > 0 &
    alpha_vals <= priorParameters[1]

  if (any(valid)) {
    theta_density[valid] <- dunif(alpha_vals[valid], 0, priorParameters[1]) *
      dgamma(1 / lambda_vals[valid], priorParameters[2], priorParameters[3]) /
      lambda_vals[valid]^2
  }

  if (!is.null(alpha_dims) && prod(alpha_dims) == out_length) {
    return(array(theta_density, dim = alpha_dims))
  }

  if (!is.null(lambda_dims) && prod(lambda_dims) == out_length) {
    return(array(theta_density, dim = lambda_dims))
  }

  theta_density
}

#' @export
#' @rdname PosteriorDraw
PosteriorDraw.weibull <- function(mdObj, x, n = 100, ...) {

  if (missing(...)){
    start_pos <- PriorDraw(mdObj, 1)
  } else {
    start_pos <- list(...)$start_pos
  }

  mh_result <- MetropolisHastings.weibull(mdObj, x, start_pos, no_draws = n)

  theta <- list(array(mh_result$parameter_samples[[1]], dim = c(1, 1, n)),
                array(mh_result$parameter_samples[[2]],
                      dim = c(1, 1, n)))

  return(theta)
}

#' @export
#' @rdname PriorParametersUpdate
PriorParametersUpdate.weibull <- function(mdObj, clusterParameters, n = 1) {

  hyperPriorParameters <- mdObj$hyperPriorParameters
  priorParameters <- mdObj$priorParameters

  numClusters <- dim(clusterParameters[[1]])[3]

  newPhi <- rpareto(n, max(clusterParameters[[1]], hyperPriorParameters[1]),
                    hyperPriorParameters[2] + numClusters)
  newGamma <- rgamma(n, hyperPriorParameters[3] + priorParameters[2] * numClusters,
                     hyperPriorParameters[4] + sum(1/clusterParameters[[2]]))

  new_priorParameters <- matrix(c(newPhi[n],
                                  priorParameters[2],
                                  newGamma[n]),
                                  ncol = 3)
  mdObj$priorParameters <- new_priorParameters
  return(mdObj)
}

#' @export
MhParameterProposal.weibull <- function(mdObj, old_params) {

  mhStepSize <- mdObj$mhStepSize
  new_params <- old_params

  new_params[[1]] <- array(abs(c(old_params[[1]]) + mhStepSize * rnorm(1, 0, 1.7)), dim=c(1,1,1))

  return(new_params)
}

#' @export
MetropolisHastings.beta <- function(mixingDistribution, x, start_pos, no_draws) {
  # Initialize parameter storage
  parameter_samples <- list()
  for (i in seq_along(start_pos)) {
    parameter_samples[[i]] <- array(dim = c(dim(start_pos[[i]])[1:2], no_draws))
    parameter_samples[[i]][, , 1] <- start_pos[[i]][, , 1]
  }

  accept_count <- 0
  old_param <- start_pos

  # Calculate initial log prior and likelihood
  old_prior <- log(PriorDensity(mixingDistribution, old_param))
  old_likelihood <- sum(log(Likelihood(mixingDistribution, x, old_param)))

  # MCMC loop
  for (i in seq_len(no_draws - 1)) {
    # Propose new parameters
    prop_param <- MhParameterProposal(mixingDistribution, old_param)

    # Calculate new log prior and likelihood
    new_prior <- log(PriorDensity(mixingDistribution, prop_param))
    new_likelihood <- sum(log(Likelihood(mixingDistribution, x, prop_param)))

    # Calculate acceptance probability
    log_ratio <- (new_prior + new_likelihood) - (old_prior + old_likelihood)
    accept_prob <- min(1, exp(log_ratio))

    # Handle numerical issues
    if (is.na(accept_prob) || !is.finite(accept_prob)) {
      accept_prob <- 0
    }

    # Accept or reject
    if (runif(1) < accept_prob) {
      accept_count <- accept_count + 1
      sampled_param <- prop_param
      old_likelihood <- new_likelihood
      old_prior <- new_prior
    } else {
      sampled_param <- old_param
    }

    # Store parameters
    old_param <- sampled_param
    for (j in seq_along(start_pos)) {
      parameter_samples[[j]][, , i + 1] <- sampled_param[[j]][, , 1]
    }
  }

  accept_ratio <- accept_count / no_draws

  return(list(parameter_samples = parameter_samples, accept_ratio = accept_ratio))
}

#' @export
MetropolisHastings.list <- function(mixingDistribution, x, start_pos, no_draws = 100) {
  dist_class <- mixing_distribution_method_class(mixingDistribution,
                                                 "MetropolisHastings")
  if (!is.null(dist_class)) {
    ns <- getNamespace("dirichletprocess")
    method_func <- utils::getS3method("MetropolisHastings",
                                      dist_class,
                                      optional = TRUE)

    if (dist_class == "weibull") {
      weibull_func <- get("MetropolisHastings.weibull", envir = ns)
      return(weibull_func(mixingDistribution, x, start_pos, no_draws))
    }

    if (dist_class == "beta") {
      # Call the beta method directly here
      parameter_samples <- list()
      for (i in seq_along(start_pos)) {
        parameter_samples[[i]] <- array(dim = c(dim(start_pos[[i]])[1:2], no_draws))
        parameter_samples[[i]][, , 1] <- start_pos[[i]][, , 1]
      }

      accept_count <- 0
      old_param <- start_pos

      # Calculate initial log prior and likelihood
      old_prior <- log(PriorDensity(mixingDistribution, old_param))
      old_likelihood <- sum(log(Likelihood(mixingDistribution, x, old_param)))

      # MCMC loop
      for (i in seq_len(no_draws - 1)) {
        # Propose new parameters
        prop_param <- MhParameterProposal(mixingDistribution, old_param)

        # Calculate new log prior and likelihood
        new_prior <- log(PriorDensity(mixingDistribution, prop_param))
        new_likelihood <- sum(log(Likelihood(mixingDistribution, x, prop_param)))

        # Calculate acceptance probability
        log_ratio <- (new_prior + new_likelihood) - (old_prior + old_likelihood)
        accept_prob <- min(1, exp(log_ratio))

        # Handle numerical issues
        if (is.na(accept_prob) || !is.finite(accept_prob)) {
          accept_prob <- 0
        }

        # Accept or reject
        if (runif(1) < accept_prob) {
          accept_count <- accept_count + 1
          sampled_param <- prop_param
          old_likelihood <- new_likelihood
          old_prior <- new_prior
        } else {
          sampled_param <- old_param
        }

        # Store parameters
        old_param <- sampled_param
        for (j in seq_along(start_pos)) {
          parameter_samples[[j]][, , i + 1] <- sampled_param[[j]][, , 1]
        }
      }

      accept_ratio <- accept_count / no_draws
      return(list(parameter_samples = parameter_samples, accept_ratio = accept_ratio))
    }

    if (!is.null(method_func)) {
      return(method_func(mixingDistribution, x, start_pos, no_draws))
    }
  }

  return(MetropolisHastings.default(mixingDistribution, x, start_pos, no_draws))
}
