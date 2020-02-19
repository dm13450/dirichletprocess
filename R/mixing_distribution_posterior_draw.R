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

  if (missing(...)) {
    ### This might need a try catch for models that don't have a penalised likelihood.
    start_pos <- PenalisedLikelihood(mdObj, x)
  } else {
    start_pos <- list(...)$start_pos
  }

  mh_result <- MetropolisHastings(mdObj, x, start_pos, no_draws = n)

  theta <- vector("list", length(mh_result$parameter_samples))

  for (i in seq_along(mh_result$parameter_samples)) {
    theta[[i]] <- array(mh_result$parameter_samples[[i]],
                        dim = c(dim(mh_result$parameter_sample[[i]])[1:2], n))
  }

  return(theta)
}
