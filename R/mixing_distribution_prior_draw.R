#' Draw from the prior distribution
#'
#' @param mdObj Mixing Distribution
#' @param n Number of draws.
#' @return A sample from the prior distribution
#' @export
PriorDraw <- function(mdObj, n) UseMethod("PriorDraw", mdObj)

PriorDraw.hierarchical <- function(mdObj, n = 1) {

  ##ind <- sample.int(length(mdObj$pi_k), n, prob = mdObj$pi_k, replace = TRUE)

  probs <- mdObj$pi_k

  ind <- sample(which(probs > 0), n, prob = probs[probs > 0], replace=TRUE)

  return(lapply(mdObj$theta_k, function(x) x[, , ind, drop = FALSE]))
}
