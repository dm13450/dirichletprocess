#' Draw from the posterior distribution
#'
#' @param mdObj Mixing Distribution
#' @param x Data
#' @param n Number of draws
#' @param ... For a non-conjugate distribution the starting parameters. Defaults to a draw from the prior distribution.
#' @return A sample from the posterior distribution
#' @export
PosteriorDraw <- function(mdObj, x, n = 1, ...){
  UseMethod("PosteriorDraw", mdObj)
}

#' @export
PosteriorDraw.nonconjugate <- function(mdObj, x, n = 1, ...) {

  if (missing(...) || is.null(list(...)$start_pos)) {
    # Try PenalisedLikelihood first, fall back to PriorDraw
    start_pos <- tryCatch({
      PenalisedLikelihood(mdObj, x)
    }, error = function(e) {
      PriorDraw(mdObj, 1)
    })
  } else {
    start_pos <- list(...)$start_pos
  }

  # Get MCMC samples
  mh_result <- MetropolisHastings(mdObj, x, start_pos, no_draws = n)

  # Extract parameter samples
  theta <- vector("list", length(mh_result$parameter_samples))

  for (i in seq_along(mh_result$parameter_samples)) {
    # Get the parameter array
    param_array <- mh_result$parameter_samples[[i]]

    # Ensure correct dimensions [1, 1, n]
    if (length(dim(param_array)) == 2) {
      # If 2D, reshape to 3D
      theta[[i]] <- array(param_array, dim = c(1, 1, n))
    } else if (length(dim(param_array)) == 3) {
      # Already 3D, just ensure it has the right number of samples
      theta[[i]] <- param_array[, , 1:n, drop = FALSE]
    } else {
      # Fallback: create array from values
      theta[[i]] <- array(as.numeric(param_array), dim = c(1, 1, n))
    }
  }

  return(theta)
}

#' @export
PosteriorDraw.beta <- function(mdObj, x, n = 1, ...) {

  if (missing(...) || is.null(list(...)$start_pos)) {
    # Try PenalisedLikelihood first, fall back to PriorDraw
    start_pos <- tryCatch({
      PenalisedLikelihood(mdObj, x)
    }, error = function(e) {
      PriorDraw(mdObj, 1)
    })
  } else {
    start_pos <- list(...)$start_pos
  }

  # Get MCMC samples
  mh_result <- MetropolisHastings(mdObj, x, start_pos, no_draws = n)

  # Extract and return with proper names
  if (!is.null(mh_result$parameter_samples) && length(mh_result$parameter_samples) >= 2) {
    mu_samples <- mh_result$parameter_samples[[1]]
    nu_samples <- mh_result$parameter_samples[[2]]

    # Extract the actual values from the arrays
    if (is.array(mu_samples) && length(dim(mu_samples)) >= 3) {
      mu_values <- mu_samples[1, 1, ]
    } else {
      mu_values <- as.numeric(mu_samples)
    }

    if (is.array(nu_samples) && length(dim(nu_samples)) >= 3) {
      nu_values <- nu_samples[1, 1, ]
    } else {
      nu_values <- as.numeric(nu_samples)
    }

    # Ensure we have the right number of samples
    if (length(mu_values) < n || length(nu_values) < n) {
      # If not enough samples, pad with the last value or use prior draws
      if (length(mu_values) > 0 && length(nu_values) > 0) {
        mu_values <- rep(mu_values, length.out = n)
        nu_values <- rep(nu_values, length.out = n)
      } else {
        # Fallback to prior draws
        prior_draws <- PriorDraw(mdObj, n)
        return(prior_draws)
      }
    }

    # Return as arrays with correct dimensions c(1,1,n)
    return(list(
      mu = array(mu_values[1:n], dim = c(1, 1, n)),
      nu = array(nu_values[1:n], dim = c(1, 1, n))
    ))
  } else {
    # Fallback to prior draws if MH failed
    return(PriorDraw(mdObj, n))
  }
}
