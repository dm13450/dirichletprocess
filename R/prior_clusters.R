#' Draw prior clusters and weights from the Dirichlet process
#'
#' @param dpobj A Dirichlet process object
#' @return A list of weights and parameters of the prior distribution of the Dirichcet process

#' @export
PriorClusters <- function(dpobj) UseMethod("PriorClusters")

#' @export
PriorClusters.dirichletprocess <- function(dpobj){


  alpha <- rgamma(1, dpobj$alphaPriorParameters[1], dpobj$alphaPriorParameters[2])
  numBreaks <- ceiling(alpha) * 20 + 5
  sticks <- StickBreaking(alpha, numBreaks)


  priorParams <- PriorDraw(dpobj$mixingDistribution, numBreaks)

  returnList <- list(weights = sticks, params = priorParams)

  return(returnList)
}
