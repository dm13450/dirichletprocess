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

#' @export
Likelihood.weibull <- function(mdObj, x, theta) {
  # as.numeric(dweibull(x, theta[[1]], theta[[2]]))
  x <- as.vector(x, "numeric")
  alpha <- theta[[1]][, , , drop = TRUE]
  lambda <- theta[[2]][, , , drop = TRUE]

  # a <- alpha
  # b <- lambda^(1/alpha)
  # b[is.infinite(b)] <- 1000000000000000
  #y <- dweibull(x, a, b, log = TRUE)

  y <- as.numeric(lambda^(-1) * alpha * x^(alpha - 1) * exp(-lambda^(-1) * x^alpha))
  y[is.infinite(lambda)] <- 0
  y[x < 0] <- 0
  return(y)
}
PriorDraw.weibull <- function(mdObj, n = 1) {

  priorParameters <- mdObj$priorParameters

  lambdas <- 1/rgamma(n, priorParameters[2], priorParameters[3])
  theta <- list(array(runif(n, 0, priorParameters[1]), dim = c(1, 1, n)),
                array(lambdas, dim = c(1, 1, n)))
  return(theta)
}

PriorDensity.weibull <- function(mdObj, theta) {

  priorParameters <- mdObj$priorParameters

  theta_density <- dunif(theta[[1]], 0, priorParameters[1])
  #theta_density <- thetaDensity * dgamma(1/theta[[2]], priorParameters[2], priorParameters[3])
  return(theta_density)
}

PosteriorDraw.weibull <- function(mdObj, x, n = 100, start_pos) {

  if (missing(start_pos)){
    start_pos <- PriorDraw(mdObj)
  }

  mh_result <- MetropolisHastings.weibull(mdObj, x, start_pos, no_draws = n)

  theta <- list(array(mh_result$parameter_samples[[1]], dim = c(1, 1, n)), array(mh_result$parameter_samples[[2]],
    dim = c(1, 1, n)))

  return(theta)
}

PriorParametersUpdate.weibull <- function(mdObj, clusterParameters, n = 1) {

  hyperPriorParameters <- mdObj$hyperPriorParameters
  priorParameters <- mdObj$priorParameters

  numClusters <- dim(clusterParameters[[1]])[3]

  newPhi <- rpareto(n, max(clusterParameters[[1]], hyperPriorParameters[1]),
                    hyperPriorParameters[2] + numClusters)
  newGamma <- rgamma(n, hyperPriorParameters[3] + 2 * numClusters,
                     hyperPriorParameters[4] + sum(1/clusterParameters[[2]]))

  new_priorParameters <- matrix(c(newPhi[n],
                                  priorParameters[2],
                                  newGamma[n]),
                                  ncol = 3)
  mdObj$priorParameters <- new_priorParameters
  return(mdObj)
}

MhParameterProposal.weibull <- function(mdObj, old_params) {

  mhStepSize <- mdObj$mhStepSize
  new_params <- old_params
  new_params[[1]] <- array(abs(c(old_params[[1]]) + mhStepSize * rnorm(1, 0, 1.7)), dim=c(1,1,1))

  return(new_params)
}
