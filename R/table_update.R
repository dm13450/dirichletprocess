TableUpdate <- function(dpobjlist){

  globalLabels <- lapply(seq_along(dpobjlist), function(x) match(dpobjlist[[x]]$clusterParameters[[1]],
                                                                 dpobjlist[[1]]$mixingDistribution$theta_k[[1]]))

  globalParamTable <- data.frame(table(GlobalParam=unlist(globalLabels)))
  globalParamTable$GlobalParam <- as.numeric(levels(globalParamTable$GlobalParam))

  globalParams <- dpobjlist[[1]]$mixingDistribution$theta_k

  for(i in seq_along(dpobjlist)){
    for(j in seq_len(dpobjlist[[i]]$numberClusters)){

      dataSel <- dpobjlist[[i]]$data[dpobjlist[[i]]$clusterLabels==j, ]
      globalLabelInd <- globalLabels[[i]][j]

      nTable <- globalParamTable$Freq
      nTable[globalParamTable$GlobalParam == globalLabelInd] <- nTable[globalParamTable$GlobalParam == globalLabelInd] - 1

      likeVec <- numeric(nrow(globalParamTable) + 1)

      for(k in nrow(globalParamTable)){
        globParamInd <- list(globalParams[[1]][,,globalParamTable$GlobalParam[k], drop=FALSE], globalParams[[2]][,,globalParamTable$GlobalParam[k], drop=FALSE])
        likeVec[k] <- prod(Likelihood(dpobjlist[[i]]$mixingDistribution, dataSel, globParamInd))
      }

      newParam <- PriorDraw.beta(dpobjlist[[i]]$mixingDistribution)

      likeVec[k+1] <- prod(Likelihood(dpobjlist[[i]]$mixingDistribution, dataSel, newParam))

      likeVec <- likeVec * c(nTable, dpobjlist[[i]]$mixingDistribution$gamma)

      if(all(likeVec==0)) likeVec <- rep_len(1, nrow(globalParamTable) + 1)

      newTableParamInd <- sample.int(nrow(globalParamTable)+1, 1, prob=likeVec)

      if(newTableParamInd == nrow(globalParamTable)+1){
        ### Doing something with the global parameters...
        dpobjlist[[i]]$clusterParameters[[1]][,,j] <- newParam[[1]][,,1,drop=FALSE]
        dpobjlist[[i]]$clusterParameters[[2]][,,j] <- newParam[[2]][,,1,drop=FALSE]

        globalParams[[1]] <- array(c(globalParams[[1]], newParam[[1]]), dim = c(1,1,length(globalParams[[1]])+1))
        globalParams[[2]] <- array(c(globalParams[[2]], newParam[[2]]), dim = c(1,1,length(globalParams[[1]])+1))

      }
      else{
        dpobjlist[[i]]$clusterParameters[[1]][,,j] <- globalParams[[1]][,,globalParamTable$GlobalParam[newTableParamInd], drop=FALSE]
        dpobjlist[[i]]$clusterParameters[[2]][,,j] <- globalParams[[2]][,,globalParamTable$GlobalParam[newTableParamInd], drop=FALSE]
      }

      globalLabels <- lapply(seq_along(dpobjlist), function(x) match(dpobjlist[[x]]$clusterParameters[[1]],
                                                                     dpobjlist[[1]]$mixingDistribution$theta_k[[1]]))

      globalParamTable <- data.frame(table(GlobalParam=unlist(globalLabels)))
      globalParamTable$GlobalParam <- as.numeric(levels(globalParamTable$GlobalParam))

    }
  }

  dpobjlist[[1]]$mixingDistribution$theta_k <- globalParams
  dpobjlist[[1]]$mixingDistribution$theta_k <- globalParams

  return(dpobjlist)
}
