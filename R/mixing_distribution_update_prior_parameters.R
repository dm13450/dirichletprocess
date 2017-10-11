#' Update the prior parameters of a mixing distribution
#'
#' @param mdObj Mixing Distribution Object
#' @param clusterParameters Current cluster parameters
#' @param n Number of samples
#' @return mdobj New Mixing Distribution object with updated cluster parameters
#' @export
PriorParametersUpdate <- function(mdObj, clusterParameters, n = 1){
  UseMethod("PriorParametersUpdate", mdObj)
}
