#' Create a Beta mixture with zeros at the boundaries.
#'
#' This bounded Beta mixing distribution also uses the mean and precision
#' parameterisation on \eqn{(0, maxT)}.
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

  # Create a temporary beta object with the same parameters
  temp_mdObj <- mdObj
  class(temp_mdObj) <- c("list", "beta", "nonconjugate")

  # Get result from beta likelihood and return as-is to preserve attributes/structure
  result <- Likelihood(temp_mdObj, x, theta)
  
  return(result)
}

#' @export
#' @rdname PriorDraw
PriorDraw.beta2 <- function(mdObj, n=1, ...){

  # Match repaired-R initialization semantics by only taking the C++ helper
  # path when the package is explicitly configured to use C++.
  if (using_cpp() && exists("cpp_beta2_prior_draw", mode = "function")) {
    params <- cpp_beta2_prior_draw(mdObj$priorParameters[1], mdObj$maxT, n)
    mu <- params[1:n]
    nu <- params[(n+1):(2*n)]
    return(list(mu = array(mu, c(1, 1, n)), nu = array(nu, c(1, 1, n))))
  }

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
#' @rdname Initialise
Initialise.beta2 <- function(dpObj, posterior = TRUE, m = 3, verbose = TRUE, numInitialClusters = 1, ...) {
  Initialise.nonconjugate(dpObj,
                          posterior = posterior,
                          m = m,
                          verbose = verbose,
                          numInitialClusters = numInitialClusters)
}

#' @export
#' @rdname MhParameterProposal
MhParameterProposal.beta2 <- function(mdObj, old_params){

  mhStepSize <- mdObj$mhStepSize

  new_params <- old_params

  # Extract current values
  old_mu <- as.numeric(old_params[[1]])
  old_nu <- as.numeric(old_params[[2]])

  # Propose new mu
  new_mu <- old_mu + mhStepSize[1] * rnorm(1, 0, 2.4)

  if (new_mu > mdObj$maxT || new_mu < 0) {
    new_mu <- old_mu
  }

  # Handle NA values
  if (is.na(new_mu)) {
    new_mu <- old_mu
  }

  # Propose new nu (ensure positive)
  new_nu <- abs(old_nu + mhStepSize[2] * rnorm(1, 0, 2.4))

  # Handle NA values and ensure minimum values
  if (is.na(new_nu) || new_nu == 0) {
    new_nu <- 1e-04
  }

  # Return in proper format
  new_params[[1]] <- array(new_mu, dim = c(1, 1, 1))
  new_params[[2]] <- array(new_nu, dim = c(1, 1, 1))

  return(new_params)

}
