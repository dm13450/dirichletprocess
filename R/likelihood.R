#' The likelihood of the Dirichlet process object
#'
#' Calculate the likelihood of each data point with its parameter.
#'
#' @param dpobj The dirichletprocess object on which to calculate the likelihood.
#'
#' @export
LikelihoodDP <- function(dpobj){

  allParameters <- lapply(seq_along(dpobj$clusterParameters),
                          function(i) dpobj$clusterParameters[[i]][,,dpobj$clusterLabels, drop=FALSE])

  likelihoodValues <- Likelihood(dpobj$mixingDistribution, dpobj$data, allParameters)

  return(likelihoodValues)

}
