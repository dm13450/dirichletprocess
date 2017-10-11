#' Change the observations of fitted Dirichlet Process.
#'
#' Using a fitted Dirichlet process object include new data. The new data will be assigned to the best fitting cluster for each point.
#'@param dpobj The Dirichlet process object.
#'@param newData New data to be included
#'@return Changed Dirichlet process object
#'@examples
#'
#' y <- rnorm(10)
#' dp <- DirichletProcessGaussian(y)
#' dp <- ChangeObservations(dp, rnorm(10))
#'
#'@export
ChangeObservations <- function(dpobj, newData) UseMethod("ChangeObservations", dpobj)

#' @export
ChangeObservations.default <- function(dpobj, newData) {

  if (is.numeric(newData))
    newData <- matrix(newData, ncol = 1)

  predicted_data <- ClusterLabelPredict(dpobj, newData)

  predicted_data$pointsPerCluster[1:dpobj$numberClusters] <- predicted_data$pointsPerCluster[1:dpobj$numberClusters] -
    dpobj$pointsPerCluster  #removes the old data from the clusters

  emptyClusters <- which(predicted_data$pointsPerCluster == 0)

  if (length(emptyClusters) > 0) {

    predicted_data$pointsPerCluster <- predicted_data$pointsPerCluster[-emptyClusters]
    # predicted_data$clusterParams = predicted_data$clusterParams[-emptyClusters, ,
    # drop=FALSE]
    predicted_data$clusterParams <- lapply(predicted_data$clusterParams, function(x) x[,
      , -emptyClusters, drop = FALSE])
    predicted_data$numLabels <- predicted_data$numLabels - length(emptyClusters)

    for (i in length(emptyClusters):1) {
      # go through backwards to reindex correctly
      predicted_data$componentIndexes[predicted_data$componentIndexes > emptyClusters[i]] <- predicted_data$componentIndexes[predicted_data$componentIndexes >
        emptyClusters[i]] - 1
    }
  }

  dpobj$data <- newData
  dpobj$n <- length(newData)

  dpobj$clusterLabels <- predicted_data$componentIndexes
  dpobj$pointsPerCluster <- predicted_data$pointsPerCluster
  dpobj$numberClusters <- predicted_data$numLabels
  dpobj$clusterParameters <- predicted_data$clusterParams

  dpobj <- InitialisePredictive(dpobj)

  return(dpobj)
}

#'@export
ChangeObservations.hierarchical <- function(dpobj, newData){
  for(i in seq_along(dpobj$indDP)){
    dpobj$indDP[[i]] <- ChangeObservations(dpobj$indDP[[i]], newData[[i]])
  }
  return(dpobj)
}

