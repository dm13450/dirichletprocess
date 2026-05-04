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

  if (!is.matrix(newData)){
    newData <- matrix(newData, ncol = 1)
  }

  # Store original state for debugging
  original_numClusters <- dpobj$numberClusters
  original_pointsPerCluster <- dpobj$pointsPerCluster

  predicted_data <- ClusterLabelPredict(dpobj, newData)

  # Calculate the change in points per cluster
  new_pointsPerCluster <- predicted_data$pointsPerCluster
  if (length(new_pointsPerCluster) >= original_numClusters) {
    # Subtract old data counts from the clusters that existed before
    new_pointsPerCluster[1:original_numClusters] <-
      new_pointsPerCluster[1:original_numClusters] - original_pointsPerCluster
  } else {
    # This shouldn't happen
    stop("Predicted data has fewer clusters than original")
  }

  # Find empty clusters
  emptyClusters <- which(new_pointsPerCluster == 0)

  if (length(emptyClusters) > 0) {
    # Remove empty clusters
    new_pointsPerCluster <- new_pointsPerCluster[-emptyClusters]

    # For mvnormal with pre-allocated arrays, we don't actually remove slots
    if (inherits(dpobj, "mvnormal") && is.list(predicted_data$clusterParams)) {
      # Just mark the slots as inactive by reordering
      active_clusters <- setdiff(seq_len(predicted_data$numLabels), emptyClusters)
      new_numLabels <- length(active_clusters)

      # Create mapping from old to new indices
      new_idx <- integer(predicted_data$numLabels)
      new_idx[active_clusters] <- seq_along(active_clusters)

      # Remap component indexes
      for (i in seq_along(predicted_data$componentIndexes)) {
        old_idx <- predicted_data$componentIndexes[i]
        if (old_idx %in% active_clusters) {
          predicted_data$componentIndexes[i] <- new_idx[old_idx]
        }
      }

      # Compact the parameters
      for (j in seq_along(predicted_data$clusterParams)) {
        param_dims <- dim(predicted_data$clusterParams[[j]])
        if (length(param_dims) == 3) {
          # Move active clusters to the front
          for (k in seq_along(active_clusters)) {
            if (k != active_clusters[k]) {
              predicted_data$clusterParams[[j]][, , k] <-
                predicted_data$clusterParams[[j]][, , active_clusters[k]]
            }
          }
        }
      }

      predicted_data$numLabels <- new_numLabels
    } else {
      # Original logic for non-mvnormal distributions - with dimension checking
      predicted_data$clusterParams <- lapply(predicted_data$clusterParams,
                                             function(x) {
                                               if (length(dim(x)) >= 3) {
                                                 x[, , -emptyClusters, drop = FALSE]
                                               } else if (length(dim(x)) == 2) {
                                                 x[, -emptyClusters, drop = FALSE]
                                               } else {
                                                 x[-emptyClusters]
                                               }
                                             })
      predicted_data$numLabels <- predicted_data$numLabels - length(emptyClusters)

      # Reindex component assignments
      for (i in length(emptyClusters):1) {
        predicted_data$componentIndexes[predicted_data$componentIndexes > emptyClusters[i]] <-
          predicted_data$componentIndexes[predicted_data$componentIndexes > emptyClusters[i]] - 1
      }
    }
  }

  # Update dpobj with new data and cluster information
  dpobj$data <- newData
  dpobj$n <- nrow(newData)
  dpobj$clusterLabels <- predicted_data$componentIndexes
  dpobj$pointsPerCluster <- new_pointsPerCluster
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

