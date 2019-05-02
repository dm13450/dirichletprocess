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

  as.numeric(dnorm(x, theta[[1]], theta[[2]]))
}

#' @export
#' @rdname PriorDraw
PriorDraw.normal <- function(mdObj, n = 1) {

  priorParameters <- mdObj$priorParameters

  lambda <- rgamma(n, priorParameters[3], priorParameters[4])
  mu <- rnorm(n, priorParameters[1], (priorParameters[2] * lambda)^(-0.5))
  theta <- list(array(mu, dim = c(1, 1, n)), array(sqrt(1/lambda), dim = c(1, 1,
    n)))
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

  PosteriorParameters <- matrix(c(mu.n, kappa.n, alpha.n, beta.n), ncol = 4)
  return(PosteriorParameters)
}

#' @export
#' @rdname PosteriorDraw
PosteriorDraw.normal <- function(mdObj, x, n = 1, ...) {

  PosteriorParameters_calc <- PosteriorParameters(mdObj, x)

  lambda <- rgamma(n, PosteriorParameters_calc[3], PosteriorParameters_calc[4])
  mu <- rnorm(n,
              PosteriorParameters_calc[1],
              1/sqrt(PosteriorParameters_calc[2] * lambda))
  theta <- list(array(mu, dim = c(1, 1, n)),
                array(sqrt(1/lambda), dim = c(1, 1, n)))
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
