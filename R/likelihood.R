#' The likelihood of the Dirichlet process object
#'
#' Calculate the likelihood of each data point with its parameter.
#'
#' @param dpobj The dirichletprocess object on which to calculate the likelihood.
#'
#' @export
LikelihoodDP <- function(dpobj){

  clusters_parameters <- dpobj$clusterParameters

  likelihoodValues <- vapply(seq_len(nrow(dpobj$data)),
                            function(i) Likelihood(dpobj$mixingDistribution, dpobj$data[i, ,drop=FALSE], clusters_parameters),
                            numeric(dpobj$numberClusters))

  dim(likelihoodValues) <- c(nrow(dpobj$data), dpobj$numberClusters)

  weight <- dpobj$pointsPerCluster / dpobj$n


  likelihoodValues <- as.matrix(likelihoodValues) %*% weight

  return(likelihoodValues)
}



