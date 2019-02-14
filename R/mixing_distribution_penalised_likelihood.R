#' Calculate the parameters that maximise the penalised likelihood.
#'
#' Used to find suitable starting parameters for nonconjugate mixtures.
#' For some mixing distributions this hasn't been implemented yet.
#'
#' @param mdObj Mixing distribution object
#' @param x Data
#'
#' @export
PenalisedLikelihood <- function(mdObj, x){
  UseMethod("PenalisedLikelihood", mdObj)
}

#' @export
#' @rdname PenalisedLikelihood
PenalisedLikelihood.default <- function(mdObj, x){
  return(PriorDraw(mdObj, 1))
}
