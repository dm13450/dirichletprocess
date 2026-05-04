#' Update the parameters of the hierarchical Dirichlet process object.
#'
#' @param dpobjlist List of Dirichlet Process objects.

#'@export
GlobalParameterUpdate <- function(dpobjlist){
  UseMethod("GlobalParameterUpdate",dpobjlist)
}

#'@export
GlobalParameterUpdate.hierarchical <- function(dpobjlist) {
  # Use C++ implementation if enabled and available
  if (using_cpp_hierarchical_samplers() && all(sapply(dpobjlist$indDP, function(x) inherits(x, "beta")))) {
    return(GlobalParameterUpdate.hierarchical.cpp(dpobjlist))
  }

  # Original R implementation
  theta_k <- dpobjlist$globalParameters
  
  # Ensure theta_k has proper names from the start
  if (is.null(names(theta_k)) && length(theta_k) == 2) {
    names(theta_k) <- c("mu", "nu")
  }

  global_labels <- unique(unlist(lapply(seq_along(dpobjlist$indDP),
                                        function(x) match(
                                          unlist(dpobjlist$indDP[[x]]$clusterParameters[[1]]),
                                          theta_k[[1]])
  )
  )
  )

  global_labels <- true_cluster_labels(global_labels, dpobjlist)

  for (i in seq_along(global_labels)) {

    param <- theta_k[[1]][, , global_labels[i]]

    pts <- vector("list", length(dpobjlist$indDP))
    localIndex <- rep_len(NA, length(dpobjlist$indDP))

    for (k in seq_along(dpobjlist$indDP)) {

      localInd <- which(dpobjlist$indDP[[k]]$clusterParameters[[1]] == param)
      localInd <- true_cluster_labels(localInd, dpobjlist)
      if(length(localInd) != 0){
        localIndex[k] <- localInd
        pts[[k]] <- dpobjlist$indDP[[k]]$data[dpobjlist$indDP[[k]]$clusterLabels %in% localIndex[k],]
      }
    }

    # Remove NULL entries from pts before unlisting
    pts_clean <- pts[!sapply(pts, is.null)]
    
    # Handle case where no data points are found
    if (length(pts_clean) == 0) {
      # Skip this global parameter if no data points are associated with it
      next
    }
    
    total_pts <- matrix(unlist(pts_clean), ncol=ncol(dpobjlist$indDP[[1]]$data), byrow = TRUE)

    #start_pos <- vector("list", length(theta_k))
    #for (k in seq_along(start_pos)) {
    #start_pos[[k]] <- theta_k[[k]][, , global_labels[i], drop = FALSE]
    #}

    new_param <- PosteriorDraw(dpobjlist$indDP[[1]]$mixingDistribution,
                               total_pts,
                               100) #, start_pos)

    for (k in seq_along(new_param)) {
      # Handle different parameter dimensions
      theta_k_dims <- dim(theta_k[[k]])
      new_param_dims <- dim(new_param[[k]])
      
      if (length(theta_k_dims) == 3 && length(new_param_dims) == 3) {
        # 3D array case
        theta_k[[k]][, , global_labels[i]] <- new_param[[k]][, , 100]
      } else if (length(theta_k_dims) == 2 && length(new_param_dims) == 3) {
        # theta_k is 2D, new_param is 3D
        theta_k[[k]][, global_labels[i]] <- new_param[[k]][, , 100]
      } else if (length(theta_k_dims) == 1 && length(new_param_dims) == 3) {
        # theta_k is 1D, new_param is 3D
        theta_k[[k]][global_labels[i]] <- new_param[[k]][, , 100]
      } else {
        # Try direct assignment for other cases
        tryCatch({
          theta_k[[k]][global_labels[i]] <- new_param[[k]][100]
        }, error = function(e) {
          # Fallback: extract scalar value
          if (is.array(new_param[[k]])) {
            theta_k[[k]][global_labels[i]] <- as.numeric(new_param[[k]])[100]
          } else {
            theta_k[[k]][global_labels[i]] <- new_param[[k]][100]
          }
        })
      }
    }

    for (k in seq_along(dpobjlist$indDP)) {
      if (is.na(localIndex[k])){
        next
      }
      else{
        for (j in seq_along(new_param)) {
          # Handle different parameter dimensions for individual DPs
          ind_param_dims <- dim(dpobjlist$indDP[[k]]$clusterParameters[[j]])
          new_param_dims <- dim(new_param[[j]])
          
          if (length(ind_param_dims) == 3 && length(new_param_dims) == 3) {
            # 3D array case
            dpobjlist$indDP[[k]]$clusterParameters[[j]][, , localIndex[k]] <- new_param[[j]][, , 100]
          } else if (length(ind_param_dims) == 2 && length(new_param_dims) == 3) {
            # ind_param is 2D, new_param is 3D
            dpobjlist$indDP[[k]]$clusterParameters[[j]][, localIndex[k]] <- new_param[[j]][, , 100]
          } else if (length(ind_param_dims) == 1 && length(new_param_dims) == 3) {
            # ind_param is 1D, new_param is 3D
            dpobjlist$indDP[[k]]$clusterParameters[[j]][localIndex[k]] <- new_param[[j]][, , 100]
          } else {
            # Try direct assignment for other cases
            tryCatch({
              dpobjlist$indDP[[k]]$clusterParameters[[j]][localIndex[k]] <- new_param[[j]][100]
            }, error = function(e) {
              # Fallback: extract scalar value
              if (is.array(new_param[[j]])) {
                dpobjlist$indDP[[k]]$clusterParameters[[j]][localIndex[k]] <- as.numeric(new_param[[j]])[100]
              } else {
                dpobjlist$indDP[[k]]$clusterParameters[[j]][localIndex[k]] <- new_param[[j]][100]
              }
            })
          }
        }
      }
    }
  }

  # Ensure theta_k always has proper names before assignment
  if (is.null(names(theta_k))) {
    if (length(theta_k) == 2) {
      names(theta_k) <- c("mu", "nu")
    } else if (!is.null(names(dpobjlist$globalParameters))) {
      names(theta_k) <- names(dpobjlist$globalParameters)
    }
  }

  for(i in seq_along(dpobjlist$indDP)){
    # Ensure each individual mixing distribution gets properly named theta_k
    dpobjlist$indDP[[i]]$mixingDistribution$theta_k <- theta_k
  }

  dpobjlist$globalParameters <- theta_k
  return(dpobjlist)
}
