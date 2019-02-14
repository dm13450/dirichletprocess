#' Create a Beta mixture with zeros at the boundaries.
#'
#' @param priorParameters The prior parameters for the base measure.
#' @param mhStepSize The Metropolis Hastings step size. A numeric vector of length 2.
#' @param maxT The upper bound of the Beta distribution. Defaults to 1 for the standard Beta distribution.
#' @return A mixing distribution object.
#' @export
BetaMixture2Create <- function(priorParameters = 2, mhStepSize = c(1, 1), maxT = 1){

  mdObj <- MixingDistribution("beta2",
                              priorParameters, "nonconjugate",
                              mhStepSize)
  mdObj$maxT <- maxT
  return(mdObj)
}

#' @export
#' @rdname Likelihood
Likelihood.beta2 <- function(mdObj, x, theta){

  Likelihood.beta(mdObj, x, theta)

}

#' @export
#' @rdname PriorDraw
PriorDraw.beta2 <- function(mdObj, n=1){

  priorParameters <- mdObj$priorParameters

  mu <- runif(n, 0, mdObj$maxT)

  muLim <- vapply(mu, function(x) max(1/(x/mdObj$maxT), 1/(1-(x/mdObj$maxT))), numeric(1))
  nu <- rpareto(n, muLim, priorParameters[1])

  theta <- list(mu = array(mu, c(1, 1, n)), nu = array(nu, c(1, 1, n)))
  return(theta)
}

#' @export
#' @rdname PriorDensity
PriorDensity.beta2 <- function(mdObj, theta){

  priorParameters <- mdObj$priorParameters
  muDensity <- dunif(theta[[1]], 0, mdObj$maxT)
  muLim <- vapply(theta[[1]], function(x) max(1/(x/mdObj$maxT), 1/(1-(x/mdObj$maxT))), numeric(1))
  nuDensity <- dpareto(theta[[2]], muLim, priorParameters[1])

  thetaDensity <- muDensity * nuDensity
  return(as.numeric(thetaDensity))
}

#' @export
#' @rdname MhParameterProposal
MhParameterProposal.beta2 <- function(mdObj, old_params){

  mhStepSize <- mdObj$mhStepSize

  new_params <- old_params

  new_params[[1]] <- old_params[[1]] + mhStepSize[1] * rnorm(1, 0, 2.4)

  if (new_params[[1]] > mdObj$maxT | new_params[[1]] < 0) {
    new_params[[1]] <- old_params[[1]]
  }

  new_params[[2]] <- abs(old_params[[2]] + mhStepSize[2] * rnorm(1, 0, 2.4))

  return(new_params)

}


