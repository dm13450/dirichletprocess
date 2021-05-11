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
  
  #sum by rows(for each row sum all the columns,i.e. sum the densities of a point for different clusters)
  likelihoodValues <- rowSums(likelihoodValues) #n x 1 vector
  
  
  #This is if you want to also multiply by the weight and in the mean time sum the columns, instead of the rowSums:
  
  #1 weight <- dpObj$pointsPerCluster / dpObj$n #the problem with that is that now it is calculated also in the fit function before using 
                                              #LikkelihoodDP function
  
  #2 likelihoodValues <-  likelihoodValues %*% weight #n x k * k x 1 = n x 1 the vector of likelihood for the points.
  
  return(likelihoodValues)

}
