#' Update the parameters of the hierarchical Dirichlet process object.
#'
#' @param dpobjlist List of Dirichlet Process objects.

#'@export
GlobalParameterUpdate <- function(dpobjlist){
  UseMethod("GlobalParameterUpdate",dpobjlist)
}

#'@export
GlobalParameterUpdate.hierarchical <- function(dpobjlist) {

  theta_k <- dpobjlist$globalParameters

  global_labels <- unique(unlist(lapply(seq_along(dpobjlist$indDP),
                                        function(x) match(
                                          unlist(dpobjlist$indDP[[x]]$clusterParameters[[1]]),
                                          theta_k[[1]])
                                        )
                                 )
                          )


  for (i in seq_along(global_labels)) {

    param <- theta_k[[1]][, , global_labels[i]]

    pts <- vector("list", length(dpobjlist$indDP))
    localIndex <- rep_len(NA, length(dpobjlist$indDP))

    for (k in seq_along(dpobjlist$indDP)) {

      localInd <- which(dpobjlist$indDP[[k]]$clusterParameters[[1]] == param)
      if(length(localInd) != 0){
        localIndex[k] <- localInd
        pts[[k]] <- dpobjlist$indDP[[k]]$data[dpobjlist$indDP[[k]]$clusterLabels %in% localIndex[k],]
      }
    }

    total_pts <- matrix(unlist(pts), ncol = 1)

    #start_pos <- vector("list", length(theta_k))
    #for (k in seq_along(start_pos)) {
      #start_pos[[k]] <- theta_k[[k]][, , global_labels[i], drop = FALSE]
    #}

    new_param <- PosteriorDraw(dpobjlist$indDP[[1]]$mixingDistribution,
                               total_pts,
                               100) #, start_pos)

    for (k in seq_along(new_param)) {
      theta_k[[k]][, , global_labels[i]] <- new_param[[k]][, , 100]
    }

    for (k in seq_along(dpobjlist$indDP)) {
      if (is.na(localIndex[k])){
        next
      }
      else{
        for (j in seq_along(new_param)) {
          dpobjlist$indDP[[k]]$clusterParameters[[j]][, , localIndex[k]] <- new_param[[j]][, , 100]
        }
      }
    }
  }

  for(i in seq_along(dpobjlist$indDP)){
    dpobjlist$indDP[[i]]$mixingDistribution$theta_k <- theta_k
  }

  dpobjlist$globalParameters <- theta_k
  return(dpobjlist)
}
