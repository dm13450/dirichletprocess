#'Update the cluster parameters of the Dirichlet process.
#'
#' Update the parameters of each individual cluster using all the data assigned to the particular cluster.
#' A sample is taken from the posterior distribution using a direct sample if the mixing distribution is conjugate or the Metropolis Hastings algorithm for non-conjugate mixtures.
#'
#'@param dpObj Dirichlet process object
#'@return Dirichlet process object with update cluster parameters
#'
#'@examples
#' dp <- DirichletProcessGaussian(rnorm(10))
#' dp <- ClusterParameterUpdate(dp)
#'
#'@export
ClusterParameterUpdate <- function(dpObj) UseMethod("ClusterParameterUpdate", dpObj)
#'@export
ClusterParameterUpdate.conjugate <- function(dpObj) {

  y <- dpObj$data
  numLabels <- dpObj$numberClusters

  clusterLabels <- dpObj$clusterLabels
  clusterParams <- dpObj$clusterParameters

  mdobj <- dpObj$mixingDistribution

  for (i in 1:numLabels) {
    pts <- y[which(clusterLabels == i), , drop = FALSE]

    post_draw <- PosteriorDraw(mdobj, pts)

    for (j in seq_along(clusterParams)) {
      clusterParams[[j]][, , i] <- post_draw[[j]]
    }

  }

  dpObj$clusterParameters <- clusterParams
  return(dpObj)
}
#'@export
ClusterParameterUpdate.nonconjugate <- function(dpObj) {

  y <- dpObj$data
  numLabels <- dpObj$numberClusters

  clusterLabels <- dpObj$clusterLabels
  clusterParams <- dpObj$clusterParameters

  mdobj <- dpObj$mixingDistribution
  mhDraws <- dpObj$mhDraws

  accept_ratio <- numeric(numLabels)

  start_pos <- PriorDraw(mdobj)

  for (i in 1:numLabels) {
    pts <- y[which(clusterLabels == i), , drop = FALSE]

    for (j in seq_along(clusterParams)) {
      start_pos[[j]] <- clusterParams[[j]][, , i, drop = FALSE]
    }

    parameter_samples <- PosteriorDraw(mdobj, pts, mhDraws, start_pos = start_pos)

    for (j in seq_along(clusterParams)) {
      clusterParams[[j]][, , i] <- parameter_samples[[j]][, , mhDraws]
    }


    accept_ratio[i] <- length(unique(parameter_samples[[1]]))/mhDraws
  }
  dpObj$clusterParameters <- clusterParams
  return(dpObj)
}

cluster_parameter_update <- function(mdobj, data, clusters, params){

  uniqueClusters <- unique(clusters)

  newParams <- lapply(uniqueClusters, function(i){
    updateData <- data[clusters==i, ,drop=F]
    newParam <- PosteriorDraw(mdobj, updateData)
    return(newParam)

  } )

  newParamsFull <- newParams[clusters]
  return(newParamsFull)
}



