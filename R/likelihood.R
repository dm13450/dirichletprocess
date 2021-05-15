#' The likelihood of the Dirichlet process object
#'
#' Calculate the likelihood of each data point with its parameter.
#'
#' @param dpobj The dirichletprocess object on which to calculate the likelihood.
#'
#' @export
LikelihoodDP <- function(dpobj){

  #extract the parameters of each cluster 1 time
  clusters_parameters <- dpobj$clusterParameters

  #calculate the likelihood, each point(row) has k densities(columns), each one is calculated for a different cluster
  likelihoodValues <- Likelihood(dpobj$mixingDistribution, dpobj$data, clusters_parameters) #n x k matrix

  #This is if you want to also multiply by the weight and in the mean time sum the columns, instead of the rowSums:

  weight <- dpobj$pointsPerCluster / dpobj$n #the problem with that is that now it is calculated also in the fit function before using
                                              #LikkelihoodDP function

  likelihoodValues <-  as.matrix(likelihoodValues) %*% weight #n x k * k x 1 = n x 1 the vector of likelihood for the points.

  return(likelihoodValues)

}
