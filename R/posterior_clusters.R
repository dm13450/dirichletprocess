#' Draw posterior clusters conditional on one fitted state
#'
#' Using the stick-breaking representation, this lower-level helper draws
#' posterior clusters and weights conditional on the current fitted state, or
#' on one retained stored iteration when \code{ind} is supplied. See also
#' \code{\link{PosteriorFunction}} and \code{\link{PosteriorSummary}}.
#'
#' @param dpobj Fitted Dirichlet process
#' @param ind Stored-iteration index for which the posterior will be drawn.
#'   If omitted, the current fitted state is used.
#' @return A list with the weights and cluster parameters that form a
#'   conditional posterior draw from the Dirichlet process.
#'
#' @examples
#' y <- rnorm(10)
#' dp <- DirichletProcessGaussian(y)
#' dp <- Fit(dp, 5, progressBar = FALSE)
#' postClusters <- PosteriorClusters(dp)
#'
#' @export
PosteriorClusters <- function(dpobj, ind) UseMethod("PosteriorClusters", dpobj)

#' @importFrom stats sd
#' @export
PosteriorClusters.dirichletprocess <- function(dpobj, ind) {

  if (!missing(ind)) {
    pointsPerCluster <- dpobj$weightsChain[[ind]] * dpobj$n
    alpha <- dpobj$alphaChain[ind]
    clusterParams <- dpobj$clusterParametersChain[[ind]]
  } else {
    pointsPerCluster <- dpobj$pointsPerCluster
    alpha <- dpobj$alpha
    clusterParams <- dpobj$clusterParameters
  }

  numLabels <- length(pointsPerCluster)
  mdobj <- dpobj$mixingDistribution

  # Remove zero clusters to avoid invalid arguments in rdirichlet
  non_zero_clusters <- pointsPerCluster > 0
  active_pointsPerCluster <- pointsPerCluster[non_zero_clusters]
  
  # If no active clusters, create a minimum viable cluster
  if (length(active_pointsPerCluster) == 0) {
    active_pointsPerCluster <- c(1)
  }
  
  # Ensure alpha is numeric
  if (!is.numeric(alpha)) {
    alpha <- as.numeric(alpha)
  }
  
  dirichlet_draws <- gtools::rdirichlet(1, c(active_pointsPerCluster, alpha))
  numBreaks <- ceiling(alpha + numLabels) * 20 + 5

  sticks <- StickBreaking(alpha, numBreaks)
  active_numLabels <- length(active_pointsPerCluster)
  sticks <- sticks * dirichlet_draws[active_numLabels + 1]

  # Build the full sticks vector including zeros for empty clusters
  full_sticks <- numeric(numLabels)
  full_sticks[non_zero_clusters] <- dirichlet_draws[1:active_numLabels]
  sticks <- c(full_sticks, sticks)
  # postParams <- rbind(clusterParams, PriorDraw(mdobj, numBreaks))

  #n_smps <- numBreaks + numLabels

  PriorDraws <- PriorDraw(mdobj, numBreaks)
  postParams <- list()

  # For normal distributions, preserve the sampled cluster parameters stored on
  # the DP object rather than reconstructing empirical summaries from data.
  if (inherits(mdobj, "normal") && inherits(mdobj, "conjugate")) {
    param_names <- names(PriorDraws)

    if (is.null(param_names)) {
      param_names <- names(clusterParams)
    }

    if (!is.null(param_names) && length(param_names) == length(clusterParams)) {
      for (i in seq_along(param_names)) {
        param_name <- param_names[i]
        cluster_array <- if (!is.null(names(clusterParams)) &&
                             param_name %in% names(clusterParams)) {
          clusterParams[[param_name]]
        } else {
          clusterParams[[i]]
        }

        if (param_name %in% names(PriorDraws)) {
          postParams[[i]] <- array(c(cluster_array, PriorDraws[[param_name]]),
                                   dim = c(dim(PriorDraws[[param_name]])[1:2],
                                           numBreaks + numLabels))
        } else {
          postParams[[i]] <- cluster_array
        }
      }
      names(postParams) <- param_names
    }
  } else {
    # For other distributions, use the original logic with better error handling
    param_names <- names(clusterParams)
    if (is.null(param_names)) {
      param_names <- names(PriorDraws)
    }
    
    # Ensure we have valid names and matching structure
    if (is.null(param_names) || length(param_names) != length(clusterParams)) {
      # Fallback: use numeric indices but check bounds
      for (i in seq_along(clusterParams)) {
        if (i <= length(PriorDraws)) {
          # Check if PriorDraws element has proper dimensions
          prior_dims <- dim(PriorDraws[[i]])
          if (!is.null(prior_dims) && length(prior_dims) >= 2) {
            postParams[[i]] <- array(c(clusterParams[[i]], PriorDraws[[i]]),
                                     dim = c(prior_dims[1:2],
                                             numBreaks + numLabels))
          } else {
            # If PriorDraws element doesn't have proper dimensions, just use cluster params
            postParams[[i]] <- clusterParams[[i]]
          }
        } else {
          # If PriorDraws is shorter, just use cluster params
          postParams[[i]] <- clusterParams[[i]]
        }
      }
    } else {
      # Use names to match parameters correctly
      for (i in seq_along(param_names)) {
        param_name <- param_names[i]
        if (param_name %in% names(PriorDraws)) {
          postParams[[i]] <- array(c(clusterParams[[i]], PriorDraws[[param_name]]),
                                   dim = c(dim(PriorDraws[[param_name]])[1:2],
                                           numBreaks + numLabels))
        } else {
          # If parameter not found in PriorDraws, just use cluster params
          postParams[[i]] <- clusterParams[[i]]
        }
      }
      names(postParams) <- param_names
    }
  }


  # smps <- sample.int(n_smps, replace = T, prob = sticks)
  # smpTable <- data.frame((table(smps)/n_smps))
  #   retParams <- list()
  # for (i in seq_along(clusterParams)) {
  #   retParams[[i]] <- postParams[[i]][, , smpTable$smp, drop = FALSE]
  # }
  # returnList <- list(weights = smpTable$Freq, params = retParams)
  #

  returnList <- list(weights=sticks, params=postParams)

  return(returnList)
}
