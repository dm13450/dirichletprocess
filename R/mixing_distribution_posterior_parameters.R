#' Calculate the posterior parameters for a conjugate prior.
#'
#' @param mdObj Mixing distribution object
#' @param x Data
#' @return Parameters of the posterior distribution
#' @export
PosteriorParameters <- function(mdObj, x) UseMethod("PosteriorParameters", mdObj)

