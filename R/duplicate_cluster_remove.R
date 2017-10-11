DuplicateClusterRemove <- function(dpobj){

  cp <- dpobj$clusterParameters

  dup <- duplicated(c(cp[[1]]))

  if (all(!dup)){
    return(dpobj)
  }

  dupLabels <- seq_len(dpobj$numberClusters)
  inds <- match((c(cp[[1]])), unique(c(cp[[1]])))

  oldLabs <- dpobj$clusterLabels
  newLabs <- oldLabs

  for(i in seq_along(dupLabels)){
    if(dupLabels[i] == inds[i]){
      next
    }
    else{
      newLabs[which(oldLabs==dupLabels[i])] <- inds[i]
    }
  }

  dpobj$clusterLabels <- newLabs

  newCP <- cp
  newCP[[1]] <- cp[[1]][,,which(!dup), drop=FALSE]
  newCP[[2]] <- cp[[2]][,,which(!dup), drop=FALSE]

  newPointsPerCluster <- sapply(1:max(newLabs), function(x) sum(newLabs==x))

  while (any(newPointsPerCluster==0)){

    newLabs[newLabs > which(newPointsPerCluster == 0)] = newLabs[newLabs > which(newPointsPerCluster == 0)] - 1
    newPointsPerCluster <- sapply(1:max(newLabs), function(x) sum(newLabs==x))
     #print(oldLabs)
     #print(newLabs)
     print(c(newCP[[1]]))
  }

  dpobj$clusterLabels <- newLabs
  dpobj$clusterParameters <- newCP
  dpobj$numberClusters <- length(unique(newLabs))
  dpobj$pointsPerCluster <- sapply(1:max(newLabs), function(x) sum(newLabs==x))
  return(dpobj)
}
