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

stable_allocation_probs <- function(log_probs) {
  log_probs[is.na(log_probs)] <- -Inf

  pos_inf <- is.infinite(log_probs) & log_probs > 0
  if (any(pos_inf)) {
    probs <- numeric(length(log_probs))
    probs[pos_inf] <- 1
    return(probs)
  }

  finite_inds <- is.finite(log_probs)
  if (!any(finite_inds)) {
    return(rep_len(1, length(log_probs)))
  }

  max_log_prob <- max(log_probs[finite_inds])
  probs <- exp(log_probs - max_log_prob)
  probs[!is.finite(probs)] <- 0
  return(probs)
}

weighted_log_probs <- function(weights, likelihoods) {
  weights <- rep_len(weights, length(likelihoods))
  likelihoods <- as.numeric(likelihoods)

  log_probs <- rep_len(-Inf, length(likelihoods))

  finite_mass <- !is.na(likelihoods) & !is.nan(likelihoods) &
    weights > 0 & likelihoods > 0 & is.finite(likelihoods)
  log_probs[finite_mass] <- log(weights[finite_mass]) + log(likelihoods[finite_mass])

  infinite_mass <- weights > 0 & is.infinite(likelihoods) & likelihoods > 0
  log_probs[infinite_mass] <- Inf

  return(log_probs)
}

#' @export
#' @rdname ClusterComponentUpdate
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


    currentLabel <- clusterLabels[i]

    pointsPerCluster[currentLabel] <- pointsPerCluster[currentLabel] - 1

    log_probs <- c(
      weighted_log_probs(pointsPerCluster,
                         Likelihood(mdObj, y[i, , drop = FALSE], clusterParams)),
      weighted_log_probs(alpha, predictiveArray[i])
    )
    probs <- stable_allocation_probs(log_probs)

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

    log_probs <- c(
      weighted_log_probs(pointsPerCluster,
                         Likelihood(mdObj, y[i, , drop = FALSE], clusterParams)),
      weighted_log_probs(alpha / m,
                         Likelihood(mdObj, y[i, , drop = FALSE], aux))
    )
    probs <- stable_allocation_probs(log_probs)
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

#' @export
#' @rdname ClusterComponentUpdate
ClusterComponentUpdate.hierarchical <- function(dpObj){

  for(i in seq_along(dpObj$indDP)){
    dpObj$indDP[[i]] <- ClusterComponentUpdate(dpObj$indDP[[i]])
    dpObj$indDP[[i]] <- DuplicateClusterRemove(dpObj$indDP[[i]])
  }
  return(dpObj)
}
