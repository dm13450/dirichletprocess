#' Calculate how well the prior predicts the data.
#'
#' @param mdObj The distribution
#' @param x The data
#' @return The probability of the data being from the prior.
#' @export
Predictive <- function(mdObj, x) UseMethod("Predictive", mdObj)

