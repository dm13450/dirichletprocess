ClusterLabelChange <- function(dpObj, x, newLabel, currentLabel, ...){
  UseMethod("ClusterLabelChange", dpObj)
}

ClusterLabelChange.conjugate <- function(dpObj, i, newLabel, currentLabel) {

  x <- dpObj$data[i, , drop = FALSE]
  pointsPerCluster <- dpObj$pointsPerCluster
  clusterLabels <- dpObj$clusterLabels
  clusterParams <- dpObj$clusterParameters
  numLabels <- dpObj$numberClusters
  mdObj <- dpObj$mixingDistribution

  if (newLabel <= numLabels) {
    pointsPerCluster[newLabel] <- pointsPerCluster[newLabel] + 1
    clusterLabels[i] <- newLabel

    if (pointsPerCluster[currentLabel] == 0) {
      ### Removing the Empty Cluster ###
      numLabels <- numLabels - 1
      pointsPerCluster <- pointsPerCluster[-currentLabel]

      # clusterParams <- clusterParams[-currentLabel, ,drop=FALSE]
      clusterParams <- lapply(clusterParams, function(x) x[, , -currentLabel,
        drop = FALSE])

      inds <- clusterLabels > currentLabel
      clusterLabels[inds] <- clusterLabels[inds] - 1
    }
  } else {

    if (pointsPerCluster[currentLabel] == 0) {

      post_draw <- PosteriorDraw(mdObj, x)

      for (i in seq_along(clusterParams)) {
        clusterParams[[i]][, , currentLabel] <- post_draw[[i]]
      }

      pointsPerCluster[currentLabel] <- pointsPerCluster[currentLabel] + 1
    } else {

      clusterLabels[i] <- newLabel
      numLabels <- numLabels + 1
      pointsPerCluster <- c(pointsPerCluster, 1)

      post_draw <- PosteriorDraw(mdObj, x)

      # clusterParams = rbind(clusterParams, posteriorDraw(mdObj, x))

      for (j in seq_along(clusterParams)) {
        clusterParams[[j]] <- array(c(clusterParams[[j]], post_draw[[j]]),
          dim = c(dim(post_draw[[j]])[1:2], dim(clusterParams[[j]])[3] +
          1))
      }

    }
  }

  dpObj$pointsPerCluster <- pointsPerCluster
  dpObj$clusterLabels <- clusterLabels
  dpObj$clusterParameters <- clusterParams
  dpObj$numberClusters <- numLabels
  return(dpObj)
}

ClusterLabelChange.nonconjugate <- function(dpObj, i, newLabel, currentLabel, aux) {

  pointsPerCluster <- dpObj$pointsPerCluster
  clusterLabels <- dpObj$clusterLabels
  clusterParams <- dpObj$clusterParameters
  numLabels <- dpObj$numberClusters
  # mdObj <- dpObj$mixingDistribution

  if (newLabel <= numLabels) {
    pointsPerCluster[newLabel] <- pointsPerCluster[newLabel] + 1
    clusterLabels[i] <- newLabel

    if (pointsPerCluster[currentLabel] == 0) {
      # print('B') Removing the Empty Cluster ###
      numLabels <- numLabels - 1
      pointsPerCluster <- pointsPerCluster[-currentLabel]
      # clusterParams <- clusterParams[-currentLabel, ,drop=FALSE]
      clusterParams <- lapply(clusterParams, function(x) x[, , -currentLabel,
        drop = FALSE])

      inds <- clusterLabels > currentLabel
      clusterLabels[inds] <- clusterLabels[inds] - 1
    }
  } else {

    if (pointsPerCluster[currentLabel] == 0) {
      # print('C') clusterParams[currentLabel, ] = aux[newLabel-numLabels, ]

      for (j in seq_along(clusterParams)) {
        clusterParams[[j]][, , currentLabel] <- aux[[j]][, , newLabel - numLabels]
      }
      pointsPerCluster[currentLabel] <- pointsPerCluster[currentLabel] + 1

    } else {
      # print('D')
      clusterLabels[i] <- numLabels + 1
      pointsPerCluster <- c(pointsPerCluster, 1)
      # clusterParams = rbind(clusterParams, aux[newLabel-numLabels, ])

      for (j in seq_along(clusterParams)) {
        clusterParams[[j]] <- array(c(clusterParams[[j]],
                                      aux[[j]][, , newLabel - numLabels]),
                                    dim = c(dim(clusterParams[[j]])[1:2],
                                            dim(clusterParams[[j]])[3] + 1))
      }

      numLabels <- numLabels + 1
    }
  }

  dpObj$pointsPerCluster <- pointsPerCluster
  dpObj$clusterLabels <- clusterLabels
  dpObj$clusterParameters <- clusterParams
  dpObj$numberClusters <- numLabels
  return(dpObj)
}


