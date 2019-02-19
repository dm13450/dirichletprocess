#'Calculate the prior density of a mixing distribution
#'
#'@param mdObj Mixing distribution
#'@param theta Prior parameters
#'@export
PriorDensity <- function(mdObj, theta) UseMethod("PriorDensity", mdObj)
