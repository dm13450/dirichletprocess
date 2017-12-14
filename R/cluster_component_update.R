#' Update the component of the Dirichlet process
#'
#' Update the cluster assignment for each data point.
#'
#' @param dpObj Dirichlet Process object
#' @return Dirichlet process object with update components.
#'
#' @examples
#' dp <- DirichletProcessGaussian(rnorm(10))
#' dp <- ClusterComponentUpdate(dp)
#'
#' @export
ClusterComponentUpdate <- function(dpObj){
  UseMethod("ClusterComponentUpdate", dpObj)
}

#'@export
ClusterComponentUpdate.conjugate <- function(dpObj) {

  y <- dpObj$data
  n <- dpObj$n
  alpha <- dpObj$alpha

  clusterLabels <- dpObj$clusterLabels
  clusterParams <- dpObj$clusterParameters
  numLabels <- dpObj$numberClusters
  mdObj <- dpObj$mixingDistribution

  pointsPerCluster <- dpObj$pointsPerCluster

  predictiveArray <- dpObj$predictiveArray

  for (i in seq_len(n)) {

    probs <- numeric(numLabels + 1)

    currentLabel <- clusterLabels[i]

    pointsPerCluster[currentLabel] <- pointsPerCluster[currentLabel] - 1

    probs[1:numLabels] <- pointsPerCluster * Likelihood(mdObj, y[i, , drop = FALSE],
      clusterParams)
    probs[numLabels + 1] <- alpha * predictiveArray[i]

    if(all(probs==0)){
      probs[1:(numLabels+1)] <- 1
    }
    newLabel <- sample.int(numLabels + 1, 1, prob = probs)

    dpObj$pointsPerCluster <- pointsPerCluster

    dpObj <- ClusterLabelChange(dpObj, i, newLabel, currentLabel)

    pointsPerCluster <- dpObj$pointsPerCluster
    clusterLabels <- dpObj$clusterLabels
    clusterParams <- dpObj$clusterParameters
    numLabels <- dpObj$numberClusters

  }

  dpObj$pointsPerCluster <- pointsPerCluster
  dpObj$clusterLabels <- clusterLabels
  dpObj$clusterParameters <- clusterParams
  dpObj$numberClusters <- numLabels
  return(dpObj)
}
#'@export
ClusterComponentUpdate.nonconjugate <- function(dpObj) {

  y <- dpObj$data
  n <- dpObj$n
  alpha <- dpObj$alpha

  clusterLabels <- dpObj$clusterLabels
  clusterParams <- dpObj$clusterParameters
  numLabels <- dpObj$numberClusters

  mdObj <- dpObj$mixingDistribution
  m <- dpObj$m

  pointsPerCluster <- dpObj$pointsPerCluster

  aux <- vector("list", length(clusterParams))

  for (i in seq_len(n)) {

    probs <- numeric(numLabels + 1)

    currentLabel <- clusterLabels[i]

    pointsPerCluster[currentLabel] <- pointsPerCluster[currentLabel] - 1

    if (pointsPerCluster[currentLabel] == 0) {

      priorDraws <- PriorDraw(mdObj, m - 1)

      for (j in seq_along(priorDraws)) {
        aux[[j]] <- array(c(clusterParams[[j]][, , currentLabel], priorDraws[[j]]),
          dim = c(dim(priorDraws[[j]])[1:2], m))
      }
    } else {
        aux <- PriorDraw(mdObj, m)
    }

    probs[1:numLabels] <- pointsPerCluster * Likelihood(mdObj, y[i, , drop = FALSE],
      clusterParams)
    probs[(numLabels + 1):(numLabels + m)] <- (alpha/m) * Likelihood(mdObj, y[i, , drop = FALSE], aux)

    if (any(is.nan(probs))) {
      probs[is.nan(probs)] <- 0
    }

    if (anyNA(probs)) {
      probs[is.na(probs)] <- 0
    }

    if (any(is.infinite(probs))) {
      probs[is.infinite(probs)] <- 1
      probs[-is.infinite(probs)] <- 0
    }

    if (all(probs == 0)) {
      probs <- rep_len(1, length(probs))
    }
    newLabel <- sample.int(numLabels + m, 1, prob = probs)

    dpObj$pointsPerCluster <- pointsPerCluster

    dpObj <- ClusterLabelChange(dpObj, i, newLabel, currentLabel, aux)

    pointsPerCluster <- dpObj$pointsPerCluster
    clusterLabels <- dpObj$clusterLabels
    clusterParams <- dpObj$clusterParameters
    numLabels <- dpObj$numberClusters

  }

  dpObj$pointsPerCluster <- pointsPerCluster
  dpObj$clusterLabels <- clusterLabels
  dpObj$clusterParameters <- clusterParams
  dpObj$numberClusters <- numLabels
  return(dpObj)
}
#'@export
ClusterComponentUpdate.hierarchical <- function(dpObj){

  for(i in seq_along(dpObj$indDP)){
    dpObj$indDP[[i]] <- ClusterComponentUpdate(dpObj$indDP[[i]])
    dpObj$indDP[[i]] <- DuplicateClusterRemove(dpObj$indDP[[i]])
  }
  return(dpObj)
}




