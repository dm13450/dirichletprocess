#' Draw from the prior distribution
#'
#' @param mdObj Mixing Distribution
#' @param n Number of draws.
#' @return A sample from the prior distribution

#' @export
PriorDraw <- function(mdObj, n) UseMethod("PriorDraw", mdObj)

#' @export
PriorDraw.hierarchical <- function(mdObj, n = 1) {

  probs <- mdObj$pi_k

  ind <- sample(which(probs > 0), n, prob = probs[probs > 0], replace=TRUE)

  return(lapply(mdObj$theta_k, function(x) x[, , ind, drop = FALSE]))
}
