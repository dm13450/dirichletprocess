#' Create a Beta mixing distribution.
#'
#' See \code{\link{DirichletProcessBeta}} for the default prior and hyper prior distributions.
#'
#' @param priorParameters The prior parameters for the base measure.
#' @param mhStepSize The Metropolis Hastings step size. A numeric vector of length 2.
#' @param maxT The upper bound of the Beta distribution. Defaults to 1 for the standard Beta distribution.
#' @param hyperPriorParameters The parameters for the hyper prior.
#' @return A mixing distribution object.
#' @export
BetaMixtureCreate <- function(priorParameters = c(2, 8), mhStepSize = c(1, 1), maxT = 1,
                              hyperPriorParameters = c(1, 0.125)) {

  mdObj <- MixingDistribution("beta",
                              priorParameters, "nonconjugate",
                              mhStepSize, hyperPriorParameters)
  mdObj$maxT <- maxT
  return(mdObj)
}

#' @export
#' @rdname Likelihood
Likelihood.beta <- function(mdObj, x, theta) {
  maxT <- mdObj$maxT
  x <- as.vector(x, "numeric")
  mu <- theta[[1]][, , , drop = TRUE]
  tau <- theta[[2]][, , , drop = TRUE]


  a <- (mu * tau)/maxT
  b <- (1 - mu/maxT) * tau
  #cat(c(mu, tau, a, b), '\n')
  # numerator <- (a - 1) * log(x) + (b - 1) * log(maxT - x)
  # numerator <- numerator - lbeta(a, b) - (tau - 1) * log(maxT)
  # y <- exp(numerator)

  y <- 1/maxT * dbeta(x/maxT, a, b)

  return(as.numeric(y))
}

#' @export
#' @rdname PriorDraw
PriorDraw.beta <- function(mdObj, n = 1) {

  priorParameters <- mdObj$priorParameters

  mu <- runif(n, 0, mdObj$maxT)
  nu <- 1/rgamma(n, priorParameters[1], priorParameters[2])
  theta <- list(mu = array(mu, c(1, 1, n)), nu = array(nu, c(1, 1, n)))
  return(theta)
}

#' @export
#' @rdname PriorDensity
PriorDensity.beta <- function(mdObj, theta) {

  priorParameters <- mdObj$priorParameters
  muDensity <- dunif(theta[[1]], 0, mdObj$maxT)
  nuDensity <- dgamma(1/theta[[2]], priorParameters[1], priorParameters[2])
  thetaDensity <- muDensity * nuDensity
  return(as.numeric(thetaDensity))
}

# PosteriorDraw.beta <- function(mdObj, x, n=100, start_pos){
# if(missing(start_pos)){ start_pos <- PriorDraw(mdObj) } mh_result <-
# MetropolisHastings(x, start_pos, mdObj, no_draws=n) theta <-
# list(mu=array(mh_result$parameter_samples[[1]], dim=c(1,1,n)),
# nu=array(mh_result$parameter_samples[[2]], dim=c(1,1,n))) return(theta) }

#' @export
#' @rdname PriorParametersUpdate
PriorParametersUpdate.beta <- function(mdObj, clusterParameters, n = 1) {

  hyperPriorParameters <- mdObj$hyperPriorParameters
  priorParameters <- mdObj$priorParameters

  numClusters <- dim(clusterParameters[[1]])[3]

  posteriorShape <- hyperPriorParameters[1] + priorParameters[1] * numClusters
  posteriorRate <- hyperPriorParameters[2] + sum(1/clusterParameters[[2]])

  newGamma <- rgamma(n, posteriorShape, posteriorRate)

  newPriorParameters <- matrix(c(priorParameters[1], newGamma), ncol = 2)
  mdObj$priorParameters <- newPriorParameters

  return(mdObj)
}

#' @export
#' @rdname MhParameterProposal
MhParameterProposal.beta <- function(mdObj, old_params) {

  mhStepSize <- mdObj$mhStepSize

  new_params <- old_params

  new_params[[1]] <- old_params[[1]] + mhStepSize[1] * rnorm(1, 0, 2.4)

  if (new_params[[1]] > mdObj$maxT | new_params[[1]] < 0) {
    new_params[[1]] <- old_params[[1]]
  }

  new_params[[2]] <- abs(old_params[[2]] + mhStepSize[2] * rnorm(1, 0, 2.4))

  return(new_params)
}

#' @export
#' @rdname PenalisedLikelihood
PenalisedLikelihood.beta <- function(mdObj, x){

  optimStartParams <- c(mdObj$maxT/2, 2)

  optimParams <- tryCatch(optim(optimStartParams, function(params){

    ll <- sum(log(Likelihood(mdObj, x, VectorToArray(params))))
    ll <- ll + log(PriorDensity(mdObj, VectorToArray(params)))

    if (is.infinite(ll)) ll <- -1e30

    return(-ll)
  }, method="L-BFGS-B", lower=c(0,0), upper=c(mdObj$maxT, Inf)), error = function(e) list(par=optimStartParams))


  optimParamsRet <- VectorToArray(optimParams$par)

  return(optimParamsRet)
}



