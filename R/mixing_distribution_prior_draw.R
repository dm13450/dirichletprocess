#' Draw from the prior distribution
#'
#' @param mdObj Mixing Distribution
#' @param n Number of draws.
#' @param ... Additional arguments (ignored)
#' @return A sample from the prior distribution

#' @export
PriorDraw <- function(mdObj, n, ...) UseMethod("PriorDraw", mdObj)

#' @export
PriorDraw.hierarchical <- function(mdObj, n = 1, ...) {

  probs <- mdObj$pi_k

  ind <- sample(which(probs > 0), n, prob = probs[probs > 0], replace=TRUE)

  # Extract parameters and preserve names from theta_k
  result <- lapply(mdObj$theta_k, function(x) x[, , ind, drop = FALSE])
  
  # Ensure the result has proper names regardless of theta_k naming
  if (is.null(names(result)) || any(names(result) == "")) {
    # For hierarchical beta distributions, we know it should be mu and nu
    if (inherits(mdObj, "beta") && length(result) == 2) {
      names(result) <- c("mu", "nu")
    } else if (!is.null(names(mdObj$theta_k)) && all(names(mdObj$theta_k) != "")) {
      names(result) <- names(mdObj$theta_k)
    }
  }
  
  return(result)
}
