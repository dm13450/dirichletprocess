#' Create a Gaussian Mixing Distribution with fixed variance.
#'
#'
#' @param priorParameters The prior parameters for the base measure.
#' @param sigma The fixed variance of the model.
#' @return A mixing distribution object.
#' @export
GaussianFixedVarianceMixtureCreate <- function(priorParameters=c(0,1),
                                               sigma){
  mdobj <- MixingDistribution("normalFixedVariance",
                              priorParameters,
                              "conjugate")
  mdobj$sigma <- sigma
  return(mdobj)
}

#' @export
#' @rdname Likelihood
Likelihood.normalFixedVariance <- function(mdObj, x, theta) {
  as.numeric(dnorm(x, theta[[1]], mdObj$sigma))
}

#' @export
#' @rdname PriorDraw
PriorDraw.normalFixedVariance <- function(mdObj, n = 1, ...) {

  # Use C++ if enabled
  if (can_use_cpp()) {
    mu <- cpp_normal_fixed_variance_prior_draw(
      mdObj$priorParameters[1],
      mdObj$priorParameters[2],
      mdObj$sigma,
      n
    )
    return(list(array(mu, dim = c(1, 1, n))))
  }

  priorParameters <- mdObj$priorParameters

  # Draw normal values and handle potential NAs
  mu <- rnorm(n, priorParameters[1], mdObj$sigma)

  # Handle NA values that can occur with extreme parameters
  if (any(is.na(mu))) {
    mu[is.na(mu)] <- priorParameters[1]  # Default to prior mean
  }

  theta <- list(array(mu, dim = c(1, 1, n)))
  return(theta)
}

#' @export
#' @rdname PosteriorParameters
PosteriorParameters.normalFixedVariance <- function(mdObj, x) {

  priorParameters <- mdObj$priorParameters

  n.x <- length(x)
  ybar <- mean(x)

  sigma <- mdObj$sigma
  mu0 <- priorParameters[1]
  sigma0 <- priorParameters[2]

  sigmaPosterior <- (1/sigma0^2 + n.x/sigma^2) ^ (-1)
  muPosterior <- sigmaPosterior * (mu0/sigma0^2 + sum(x)/sigma^2)
  posteriorParameters <- matrix(c(muPosterior, sigmaPosterior), ncol=2)
  return(posteriorParameters)
}

#' @export
#' @rdname PosteriorDraw
PosteriorDraw.normalFixedVariance <- function(mdObj, x, n = 1, ...) {

  posteriorParameters <- PosteriorParameters(mdObj, x)

  mu <- rnorm(n,
              posteriorParameters[1],
              posteriorParameters[2])
  theta <- list(array(mu, dim = c(1, 1, n)))
  return(theta)
}

#' @export
#' @rdname Predictive
Predictive.normalFixedVariance <- function(mdObj, x) {
  priorParameters <- mdObj$priorParameters
  sigma0 <- priorParameters[[2]]
  sigma <- mdObj$sigma

  predictiveArray <- numeric(length(x))

  for (i in seq_along(x)) {

    posteriorParameters <- PosteriorParameters(mdObj, x[i])

    predictiveArray[i] <- dnorm(x[i],
                                posteriorParameters[1],
                                sigma0^2 + sigma^2)
  }
  return(predictiveArray)
}
