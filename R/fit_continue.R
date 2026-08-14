FitContinue<- function(dpObj,its_start ,its_finish, updatePrior = FALSE, progressBar = interactive()) {
  
  if (progressBar){
    pb <- txtProgressBar(min=its_start, max=its_finish, width=50, char="-", style=3)
  }
  alphaChain <- numeric(its_finish)
  likelihoodChain <- numeric(its_finish)
  weightsChain <- vector("list", length = its_finish)
  clusterParametersChain <- vector("list", length = its_finish)
  priorParametersChain <- vector("list", length = its_finish)
  labelsChain <- vector("list", length = its_finish)
  
  alphaChain[1:(its_start-1)] <- dpObj$alphaChain
  likelihoodChain[1:(its_start-1)] <- dpObj$likelihoodChain
  weightsChain[1:(its_start-1)] <- dpObj$weightsChain
  clusterParametersChain[1:(its_start-1)] <- dpObj$clusterParametersChain
  priorParametersChain[1:(its_start-1)] <-   dpObj$priorParametersChain 
  labelsChain[1:(its_start-1)] <-   dpObj$labelsChain 
  
  iteration <- its_start : its_finish
  for (i in iteration) {
    
    alphaChain[i] <- dpObj$alpha
    weightsChain[[i]] <- dpObj$pointsPerCluster / dpObj$n
    clusterParametersChain[[i]] <- dpObj$clusterParameters
    priorParametersChain[[i]] <- dpObj$mixingDistribution$priorParameters
    labelsChain[[i]] <- dpObj$clusterLabels
    
    
    likelihoodChain[i] <- sum(log(LikelihoodDP(dpObj)))
    
    dpObj <- ClusterComponentUpdate(dpObj)
    dpObj <- ClusterParameterUpdate(dpObj)
    dpObj <- UpdateAlpha(dpObj)
    
    if (updatePrior) {
      dpObj$mixingDistribution <- PriorParametersUpdate(dpObj$mixingDistribution,
                                                        dpObj$clusterParameters)
    }
    if (progressBar){
      setTxtProgressBar(pb, i)
    }
  }
  
  dpObj$weights <- dpObj$pointsPerCluster / dpObj$n
  dpObj$alphaChain <- alphaChain
  dpObj$likelihoodChain <- likelihoodChain
  dpObj$weightsChain <- weightsChain
  dpObj$clusterParametersChain <- clusterParametersChain
  dpObj$priorParametersChain <- priorParametersChain
  dpObj$labelsChain <- labelsChain
  
  if (progressBar) {
    close(pb)
  }
  return(dpObj)
}
