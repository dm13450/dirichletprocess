#' Draw from the posterior distribution
#'
#' @param mdObj Mixing Distribution
#' @param x Data
#' @param n Number of draws
#' @param ... For a non-conjugate distribution the starting parameters. Defaults to a draw from the prior distribution.
#' @return A sample from the posterior distribution
#' @export
PosteriorDraw <- function(mdObj, x, n = 1, ...) UseMethod("PosteriorDraw", mdObj)

PosteriorDraw.nonconjugate <- function(mdObj, x, n = 1, start_pos, ...) {

  if (missing(start_pos)) {
    start_pos <- PenalisedLikelihood(mdObj, x)
  }

  mh_result <- MetropolisHastings(mdObj, x, start_pos, no_draws = n)

  theta <- vector("list", length(mh_result))

  for (i in seq_along(mh_result$parameter_samples)) {
    theta[[i]] <- array(mh_result$parameter_samples[[i]],
                        dim = c(dim(mh_result$parameter_sample[[i]])[1:2], n))
  }

  return(theta)
}
