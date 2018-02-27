#' Create a Exponential mixing distribution
#'
#' See \code{\link{DirichletProcessExponential}} for details on the base measure.
#'
#'@param priorParameters Prior parameters for the base measure.
#'@return Mixing distribution object
#'@export
ExponentialMixtureCreate <- function(priorParameters=c(0.01, 0.01)){
  mdObj <- MixingDistribution("exponential", priorParameters, "conjugate")
  return(mdObj)
}

Likelihood.exponential <- function(mdObj, x, theta){
  y <- as.numeric(dexp(x, theta[[1]]))
  return(y)
}

PriorDraw.exponential <- function(mdobj, n){
  draws <- rgamma(n, mdobj$priorParameters[1], mdobj$priorParameters[2])
  theta <- list(array(draws, dim=c(1,1,n)))
  return(theta)
}

PosteriorDraw.exponential <- function(mdobj, x, n=1){
  priorParameters <- mdobj$priorParameters
  theta <- rgamma(n, priorParameters[1] + length(x), priorParameters[2] + sum(x))
  return(list(array(theta, dim=c(1,1,n))))
}

Predictive.exponential <- function(mdobj, x){

  priorParameters <- mdobj$priorParameters

  pred <- numeric(length(x))

  for(i in seq_along(x)){
    alphaPost <- priorParameters[1] + length(x[i])
    betaPost <- priorParameters[2] + sum(x[i])
    pred[i] <- (gamma(alphaPost)/gamma(priorParameters[1])) * ((priorParameters[2] ^priorParameters[1])/(betaPost^alphaPost))
  }
  return(pred)
}
