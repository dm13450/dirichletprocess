#'Calculate the prior density of a mixing distribution
#'
#'@param mdObj Mixing distribution
#'@param x Prior parameters
#'@export
PriorDensity <- function(mdObj, x) UseMethod("PriorDensity", mdObj)
