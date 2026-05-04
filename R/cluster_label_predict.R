#' Predict the cluster labels of some new data.
#'
#' Given a fitted Dirichlet process object and some new data use this function to predict what clusters the new data belong to and associated cluster parameters.
#'
#' @param dpobj Fitted Dirichlet Process
#' @param newData New data to have cluster labels predicted.
#' @return A list of the predicted cluster labels of some new unseen data.
#'
#' @examples
#' y <- rnorm(10)
#' dp <- DirichletProcessGaussian(y)
#' dp <- Fit(dp, 5)
#' newY <- rnorm(10, 1)
#' pred <- ClusterLabelPredict(dp, newY)
#'
#' @export
ClusterLabelPredict <- function(dpobj, newData){
  UseMethod("ClusterLabelPredict", dpobj)
}

#' @export
ClusterLabelPredict.conjugate <- function(dpobj, newData) {

  if (!is.matrix(newData)) {
    # For multivariate distributions, single observations should be row vectors
    if ("mvnormal" %in% class(dpobj$mixingDistribution)) {
      newData <- matrix(newData, nrow = 1)
    } else {
      # For univariate distributions, use column vector
      newData <- matrix(newData, ncol = 1)
    }
  }

  alpha <- dpobj$alpha
  clusterParams <- dpobj$clusterParameters
  numLabels <- dpobj$numberClusters
  mdobj <- dpobj$mixingDistribution
  pointsPerCluster <- dpobj$pointsPerCluster
  Predictive_newData <- Predictive(mdobj, newData)
  componentIndexes <- numeric(nrow(newData))

  # For mvnormal with pre-allocated arrays, check capacity and expand if necessary
  if (inherits(dpobj, "mvnormal") && is.list(clusterParams)) {
    current_capacity <- dim(clusterParams[[1]])[3]
    # Check for valid capacity values
    if (!is.null(current_capacity) && !is.na(current_capacity) && 
        current_capacity < numLabels + nrow(newData)) {
      # Expand arrays preemptively
      new_capacity <- numLabels + nrow(newData) + 20
      for (j in seq_along(clusterParams)) {
        param_dims <- dim(clusterParams[[j]])
        if (length(param_dims) == 3) {
          new_array <- array(NA_real_, dim = c(param_dims[1], param_dims[2], new_capacity))
          new_array[, , 1:current_capacity] <- clusterParams[[j]]

          # Fill remaining slots with prior draws
          if (current_capacity < new_capacity) {
            extra_params <- PriorDraw(mdobj, new_capacity - current_capacity)
            if (j == 1) {
              new_array[, , (current_capacity+1):new_capacity] <- extra_params$mu
            } else {
              new_array[, , (current_capacity+1):new_capacity] <- extra_params$sig
            }
          }
          clusterParams[[j]] <- new_array
        }
      }
    }
  }

  for (i in seq_len(nrow(newData))) {
    dataVal <- newData[i, , drop = FALSE]
    weights <- numeric(numLabels + 1)

    # FIX: Re-extract active parameters inside the loop to reflect the current number of clusters.
    active_clusterParams <- clusterParams
    if (inherits(dpobj, "mvnormal") && is.list(clusterParams)) {
      active_clusterParams <- vector("list", length(clusterParams))
      names(active_clusterParams) <- names(clusterParams)
      for (j in seq_along(clusterParams)) {
        param_dims <- dim(clusterParams[[j]])
        if (length(param_dims) == 3 && param_dims[3] >= numLabels) {
          # Extract only the active clusters
          active_clusterParams[[j]] <- clusterParams[[j]][, , 1:numLabels, drop = FALSE]
        } else {
          active_clusterParams[[j]] <- clusterParams[[j]]
        }
      }
    } else if (is.list(clusterParams)) { # General case for other array-based distributions
      active_clusterParams <- vector("list", length(clusterParams))
      names(active_clusterParams) <- names(clusterParams)
      for (j in seq_along(clusterParams)) {
        param_dims <- dim(clusterParams[[j]])
        if (length(param_dims) == 3 && param_dims[3] >= numLabels) {
          active_clusterParams[[j]] <- clusterParams[[j]][, , 1:numLabels, drop = FALSE]
        } else {
          active_clusterParams[[j]] <- clusterParams[[j]]
        }
      }
    }

    weights[1:numLabels] <- pointsPerCluster * Likelihood(mdobj, dataVal, active_clusterParams)
    weights[numLabels + 1] <- alpha * Predictive_newData[i]

    # Handle NAs and invalid weights
    if (anyNA(weights)) {
      weights[is.na(weights)] <- 0
    }
    if (any(is.nan(weights))) {
      weights[is.nan(weights)] <- 0
    }
    if (all(weights == 0)) {
      weights[] <- 1 / length(weights)  # Equal probabilities
    }

    ind <- numLabels + 1
    component <- sample.int(ind, 1, prob = weights)

    if (component <= numLabels) {
      componentIndexes[i] <- component
      pointsPerCluster[component] <- pointsPerCluster[component] + 1
    } else {
      componentIndexes[i] <- component
      numLabels <- numLabels + 1
      pointsPerCluster <- c(pointsPerCluster, 1)
      post_draw <- PosteriorDraw(mdobj, newData[i, ,drop=FALSE])

      if (inherits(dpobj, "mvnormal") && is.list(clusterParams)) {
        # For mvnormal with pre-allocated arrays
        current_capacity <- dim(clusterParams[[1]])[3]
        if (!is.null(current_capacity) && !is.na(current_capacity) && 
            numLabels > current_capacity) {
          # This should not be reached due to pre-expansion
          stop("Insufficient capacity in pre-allocated arrays")
        }

        # Use the pre-allocated slot
        for (j in seq_along(clusterParams)) {
          param_dims <- dim(clusterParams[[j]])
          if (length(param_dims) == 3) {
            # Extract the single draw properly
            if (j == 1) { # For mu
              clusterParams[[j]][1, , numLabels] <- post_draw[[j]][1, , 1]
            } else { # For sig
              clusterParams[[j]][, , numLabels] <- post_draw[[j]][, , 1]
            }
          }
        }
      } else {
        # Original expansion logic for other distributions
        for (j in seq_along(clusterParams)) {
          # Check if corresponding post_draw element exists
          if (j <= length(post_draw) && !is.null(post_draw[[j]])) {
            cluster_dim <- dim(clusterParams[[j]])
            post_dim <- dim(post_draw[[j]])
            
            # Check dimensions exist and are valid
            if (!is.null(cluster_dim) && !is.null(post_dim) && 
                length(cluster_dim) >= 3 && length(post_dim) >= 2 &&
                cluster_dim[3] > 0) {
              clusterParams[[j]] <- array(c(clusterParams[[j]], post_draw[[j]]),
                                          dim = c(post_dim[1:2], cluster_dim[3] + 1))
            } else {
              # Fallback: try to append the new draw with dimension adjustment
              tryCatch({
                clusterParams[[j]] <- abind::abind(clusterParams[[j]], post_draw[[j]], along = 3)
              }, error = function(e) {
                # If abind fails due to dimension mismatch, try to reshape post_draw
                target_dims <- dim(clusterParams[[j]])
                if (!is.null(target_dims) && length(target_dims) >= 2) {
                  # Try to reshape post_draw to match the first two dimensions
                  tryCatch({
                    reshaped_post <- array(post_draw[[j]], dim = c(target_dims[1:2], 1))
                    clusterParams[[j]] <- abind::abind(clusterParams[[j]], reshaped_post, along = 3)
                  }, error = function(e2) {
                    # If all else fails, skip the expansion
                    warning("Could not expand cluster parameters due to dimension mismatch")
                  })
                }
              })
            }
          } else {
            # No corresponding post_draw element, skip expansion for this parameter
            warning(paste("No post_draw element for parameter", j, "- skipping expansion"))
          }
        }
      }
    }
  }

  outList <- list(componentIndexes = componentIndexes,
                  pointsPerCluster = pointsPerCluster,
                  clusterParams = clusterParams,
                  numLabels = numLabels)
  return(outList)
}

#' @export
ClusterLabelPredict.nonconjugate <- function(dpobj, newData) {

  if (!is.matrix(newData)) {
    # For multivariate distributions, single observations should be row vectors
    if ("mvnormal" %in% class(dpobj$mixingDistribution)) {
      newData <- matrix(newData, nrow = 1)
    } else {
      # For univariate distributions, use column vector
      newData <- matrix(newData, ncol = 1)
    }
  }

  alpha <- dpobj$alpha

  # clusterLabels <- dpobj$clusterLabels
  clusterParams <- dpobj$clusterParameters
  numLabels <- dpobj$numberClusters
  mdobj <- dpobj$mixingDistribution
  m <- dpobj$m

  pointsPerCluster <- dpobj$pointsPerCluster

  # Use nrow for matrices, length for vectors
  n_obs <- if (is.matrix(newData)) nrow(newData) else length(newData)
  componentIndexes <- numeric(n_obs)

  for (i in seq_len(n_obs)) {

    aux <- PriorDraw(mdobj, m)

    dataVal <- newData[i, , drop = FALSE]
    weights <- numeric(numLabels + 1)

    weights[1:numLabels] <- pointsPerCluster * Likelihood(mdobj, dataVal, clusterParams)
    weights[(numLabels + 1):(numLabels + m)] <- (alpha/m) * Likelihood(mdobj,
                                                                       dataVal, aux)

    if (all(weights == 0)) {
      weights[1:(numLabels + m)] <- 1
    }
    if (anyNA(weights)) {
      weights[is.na(weights)] <- 0
    }
    if (any(is.nan(weights))) {
      weights[is.nan(weights)] <- 0
    }

    ind <- numLabels + m
    component <- sample.int(ind, 1, prob = weights)

    if (component <= numLabels) {
      componentIndexes[i] <- component
      pointsPerCluster[component] <- pointsPerCluster[component] + 1
    } else {
      componentIndexes[i] <- numLabels + 1
      pointsPerCluster <- c(pointsPerCluster, 1)
      # clusterParams = rbind(clusterParams, aux[component-numLabels,])

      # Validate component index before accessing aux
      aux_index <- component - numLabels
      
      # Check if aux has elements and validate aux_index bounds
      if (length(aux) == 0 || aux_index < 1) {
        # Invalid index or empty aux - skip this iteration
        next
      }
      
      # Additional validation for aux structure
      if (length(aux) > 0 && !is.null(aux[[1]]) && length(dim(aux[[1]])) >= 3 && 
          aux_index > dim(aux[[1]])[3]) {
        # aux_index exceeds bounds - skip this iteration
        next
      }
      
      for (j in seq_along(clusterParams)) {
        # Validate that aux has this parameter index
        if (j > length(aux) || is.null(aux[[j]])) {
          # aux doesn't have parameter j - skip this parameter
          next
        }
        
        # Check if clusterParams[[j]] has valid dimensions
        current_dims <- dim(clusterParams[[j]])
        if (is.null(current_dims) || length(current_dims) == 0) {
          # If no dimensions, treat as empty and initialize from aux
          clusterParams[[j]] <- aux[[j]][, , aux_index, drop = FALSE]
        } else {
          # Ensure dimensions are positive before creating array
          new_dim3 <- current_dims[3] + 1
          if (!is.na(new_dim3) && is.finite(new_dim3) && new_dim3 > 0) {
            # Normal case: append to existing array
            clusterParams[[j]] <- array(c(clusterParams[[j]], aux[[j]][, , aux_index]), 
                                      dim = c(current_dims[1:2], new_dim3))
          } else {
            # Fallback: just use aux data
            clusterParams[[j]] <- aux[[j]][, , aux_index, drop = FALSE]
          }
        }
      }

      numLabels <- numLabels + 1
    }
  }
  outList <- list(componentIndexes = componentIndexes,
                  pointsPerCluster = pointsPerCluster,
                  clusterParams = clusterParams,
                  numLabels = numLabels)
  return(outList)
}
