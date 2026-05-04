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

#' @export
#' @rdname Likelihood
Likelihood.exponential <- function(mdObj, x, theta){
  y <- as.numeric(dexp(x, theta[[1]]))
  return(y)
}

#' @export
#' @rdname PriorDraw
PriorDraw.exponential <- function(mdObj, n, ...){
  # Draw gamma values and handle potential NAs
  draws <- rgamma(n, mdObj$priorParameters[1], mdObj$priorParameters[2])
  
  # Handle NA values that can occur with extreme parameters
  if (any(is.na(draws))) {
    draws[is.na(draws)] <- 1.0  # Default to reasonable value
  }
  
  # Ensure we don't have zero values
  draws[draws == 0] <- 1e-04
  
  theta <- list(array(draws, dim=c(1,1,n)))
  return(theta)
}

#' @export
#' @rdname PosteriorDraw
PosteriorDraw.exponential <- function(mdObj, x, n=1, ...){
  priorParameters <- mdObj$priorParameters
  theta <- rgamma(n, priorParameters[1] + length(x), priorParameters[2] + sum(x))
  return(list(array(theta, dim=c(1,1,n))))
}

#' @export
#' @rdname Predictive
Predictive.exponential <- function(mdObj, x){

  priorParameters <- mdObj$priorParameters

  pred <- numeric(length(x))

  for(i in seq_along(x)){
    alphaPost <- priorParameters[1] + length(x[i])
    betaPost <- priorParameters[2] + sum(x[i])
    # Corrected line:
    pred[i] <- (gamma(alphaPost)/gamma(priorParameters[1])) * ((priorParameters[2]^priorParameters[1])/((betaPost)^alphaPost))
  }
  return(pred)
}

#' @export
#' @rdname PosteriorParameters
PosteriorParameters.exponential <- function(mdObj, x){
  priorParameters <- mdObj$priorParameters
  alpha_n <- priorParameters[1] + length(x)
  beta_n <- priorParameters[2] + sum(x)
  return(matrix(c(alpha_n, beta_n), nrow = 1))
}
