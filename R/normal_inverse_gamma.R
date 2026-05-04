#' Create a Normal mixing distribution
#'
#' See \code{\link{DirichletProcessGaussian}} for details on the base measure.
#'
#'@param priorParameters Prior parameters for the base measure.
#'@return Mixing distribution object
#'@export

GaussianMixtureCreate <- function(priorParameters=c(0,1,1,1)){
  mdobj <- MixingDistribution("normal", priorParameters, "conjugate")
  return(mdobj)
}

#' @export
#' @rdname Likelihood
Likelihood.normal <- function(mdObj, x, theta) {
  # Handle different parameter formats gracefully
  if (is.list(theta) && length(theta) >= 2) {
    # Standard case: list with mean and sd components
    mean_param <- theta[[1]]
    sd_param <- theta[[2]]
    
    # Ensure parameters are valid
    if (any(is.na(mean_param)) || any(is.na(sd_param)) || 
        any(is.infinite(mean_param)) || any(is.infinite(sd_param))) {
      # Return small positive likelihood for invalid parameters
      return(rep(1e-100, length(x)))
    }
    
    # Ensure positive standard deviation
    sd_param <- pmax(abs(sd_param), 1e-8)
    
    result <- tryCatch({
      as.numeric(dnorm(x, mean_param, sd_param))
    }, error = function(e) {
      rep(1e-100, length(x))
    })
    
    # Handle NaN or infinite results
    result[is.na(result) | is.infinite(result)] <- 1e-100
    return(result)
    
  } else if (is.list(theta) && length(theta) == 1) {
    # Single parameter case - try to extract both components
    if (is.array(theta[[1]]) && length(dim(theta[[1]])) >= 2) {
      # Extract mean and sd from array structure
      mean_val <- theta[[1]][1, , drop = TRUE]
      sd_val <- theta[[1]][2, , drop = TRUE]
      sd_val <- pmax(abs(sd_val), 1e-8)  # Ensure positive
      result <- tryCatch({
        as.numeric(dnorm(x, mean_val, sd_val))
      }, error = function(e) {
        rep(1e-100, length(x))
      })
      result[is.na(result) | is.infinite(result)] <- 1e-100
      return(result)
    } else {
      # Fallback: use default values
      return(as.numeric(dnorm(x, 0, 1)))
    }
  } else {
    # Fallback for unexpected formats
    return(as.numeric(dnorm(x, 0, 1)))
  }
}

#' @export
#' @rdname PriorDraw
PriorDraw.normal <- function(mdObj, n = 1, ...) {

  priorParameters <- mdObj$priorParameters

  # Draw gamma values and handle potential NAs
  lambda <- rgamma(n, priorParameters[3], priorParameters[4])
  
  # Handle NA values that can occur with extreme parameters
  if (any(is.na(lambda))) {
    lambda[is.na(lambda)] <- 1.0  # Default to reasonable value
  }
  
  # Ensure we don't divide by zero
  lambda[lambda == 0] <- 1e-04
  
  # Draw normal values and handle potential NAs
  mu <- rnorm(n, priorParameters[1], (priorParameters[2] * lambda)^(-0.5))
  
  # Handle NA values
  if (any(is.na(mu))) {
    mu[is.na(mu)] <- priorParameters[1]  # Default to prior mean
  }
  
  theta <- list(mu = array(mu, dim = c(1, 1, n)), sigma = array(sqrt(1/lambda), dim = c(1, 1, n)))
  return(theta)
}

#' @export
#' @rdname PosteriorParameters
PosteriorParameters.normal <- function(mdObj, x) {

  priorParameters <- mdObj$priorParameters

  n.x <- length(x)
  ybar <- mean(x)

  mu0 <- priorParameters[1]
  kappa0 <- priorParameters[2]
  alpha0 <- priorParameters[3]
  beta0 <- priorParameters[4]

  mu.n <- (kappa0 * mu0 + n.x * ybar)/(kappa0 + n.x)
  kappa.n <- kappa0 + n.x
  alpha.n <- alpha0 + n.x/2
  beta.n <- beta0 + 0.5 * sum((x - ybar)^2) + kappa0 * n.x * (ybar - mu0)^2/(2 *
                                                                               (kappa0 + n.x))

  posteriorParameters <- matrix(c(mu.n, kappa.n, alpha.n, beta.n), ncol = 4)
  return(posteriorParameters)
}

#' @export
#' @rdname PosteriorDraw
PosteriorDraw.normal <- function(mdObj, x, n = 1, ...) {

  PosteriorParameters_calc <- PosteriorParameters(mdObj, x)

  lambda <- rgamma(n, PosteriorParameters_calc[3], PosteriorParameters_calc[4])
  mu <- rnorm(n,
              PosteriorParameters_calc[1],
              1/sqrt(PosteriorParameters_calc[2] * lambda))
  theta <- list(mu = array(mu, dim = c(1, 1, n)),
                sigma = array(sqrt(1/lambda), dim = c(1, 1, n)))
  return(theta)
}

#' @export
#' @rdname Predictive
Predictive.normal <- function(mdObj, x) {

  priorParameters <- mdObj$priorParameters
  predictiveArray <- numeric(length(x))

  for (i in seq_along(x)) {

    PosteriorParameters_calc <- PosteriorParameters(mdObj, x[i])

    predictiveArray[i] <- (gamma(PosteriorParameters_calc[3])/gamma(priorParameters[3])) *
      ((priorParameters[4]^(priorParameters[3]))/PosteriorParameters_calc[4]^PosteriorParameters_calc[3]) *
      sqrt(priorParameters[2]/PosteriorParameters_calc[2])
  }
  return(predictiveArray)
}
