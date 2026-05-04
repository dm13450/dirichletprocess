#' The likelihood of the Dirichlet process object
#'
#' Calculate the likelihood of each data point with its parameter.
#'
#' @param dpobj The dirichletprocess object on which to calculate the likelihood.
#'
#' @export
LikelihoodDP <- function(dpobj){

  clusters_parameters <- dpobj$clusterParameters

  if (inherits(dpobj, "normal_inverse_gamma") || inherits(dpobj, "normal")) {
    # Match the repaired R Gaussian likelihood bookkeeping exactly, including
    # the original vapply + dim-reset behavior that `Fit()` stores in
    # likelihoodChain before each update.
    likelihoodValues <- vapply(
      seq_len(nrow(dpobj$data)),
      function(i) Likelihood(dpobj$mixingDistribution,
                             dpobj$data[i, , drop = FALSE],
                             clusters_parameters),
      numeric(dpobj$numberClusters)
    )

    dim(likelihoodValues) <- c(nrow(dpobj$data), dpobj$numberClusters)

    weight <- dpobj$pointsPerCluster / dpobj$n

    likelihoodValues <- as.matrix(likelihoodValues) %*% weight

    return(likelihoodValues)
  }

  if (inherits(dpobj, "normalFixedVariance") ||
      inherits(dpobj$mixingDistribution, "normalFixedVariance")) {
    # Match the repaired R fixed-variance normal bookkeeping exactly for
    # diagnostics and the stored likelihoodChain.
    likelihoodValues <- vapply(
      seq_len(nrow(dpobj$data)),
      function(i) Likelihood(dpobj$mixingDistribution,
                             dpobj$data[i, , drop = FALSE],
                             clusters_parameters),
      numeric(dpobj$numberClusters)
    )

    dim(likelihoodValues) <- c(nrow(dpobj$data), dpobj$numberClusters)

    weight <- dpobj$pointsPerCluster / dpobj$n

    likelihoodValues <- as.matrix(likelihoodValues) %*% weight

    return(likelihoodValues)
  }

  if (inherits(dpobj, "exponential") ||
      inherits(dpobj$mixingDistribution, "exponential")) {
    likelihoodValues <- vapply(
      seq_len(nrow(dpobj$data)),
      function(i) Likelihood(dpobj$mixingDistribution,
                             dpobj$data[i, , drop = FALSE],
                             clusters_parameters),
      numeric(dpobj$numberClusters)
    )

    dim(likelihoodValues) <- c(nrow(dpobj$data), dpobj$numberClusters)

    weight <- dpobj$pointsPerCluster / dpobj$n

    likelihoodValues <- as.matrix(likelihoodValues) %*% weight

    return(likelihoodValues)
  }

  if (inherits(dpobj, "beta") ||
      inherits(dpobj$mixingDistribution, "beta")) {
    likelihoodValues <- vapply(
      seq_len(nrow(dpobj$data)),
      function(i) Likelihood(dpobj$mixingDistribution,
                             dpobj$data[i, , drop = FALSE],
                             clusters_parameters),
      numeric(dpobj$numberClusters)
    )

    dim(likelihoodValues) <- c(nrow(dpobj$data), dpobj$numberClusters)

    weight <- dpobj$pointsPerCluster / dpobj$n

    likelihoodValues <- as.matrix(likelihoodValues) %*% weight

    return(likelihoodValues)
  }

  if (inherits(dpobj, "beta2") ||
      inherits(dpobj$mixingDistribution, "beta2")) {
    likelihoodValues <- vapply(
      seq_len(nrow(dpobj$data)),
      function(i) Likelihood(dpobj$mixingDistribution,
                             dpobj$data[i, , drop = FALSE],
                             clusters_parameters),
      numeric(dpobj$numberClusters)
    )

    dim(likelihoodValues) <- c(nrow(dpobj$data), dpobj$numberClusters)

    weight <- dpobj$pointsPerCluster / dpobj$n

    likelihoodValues <- as.matrix(likelihoodValues) %*% weight

    return(likelihoodValues)
  }

  if (inherits(dpobj, "weibull") ||
      inherits(dpobj$mixingDistribution, "weibull")) {
    likelihoodValues <- vapply(
      seq_len(nrow(dpobj$data)),
      function(i) Likelihood(dpobj$mixingDistribution,
                             dpobj$data[i, , drop = FALSE],
                             clusters_parameters),
      numeric(dpobj$numberClusters)
    )

    dim(likelihoodValues) <- c(nrow(dpobj$data), dpobj$numberClusters)

    weight <- dpobj$pointsPerCluster / dpobj$n

    likelihoodValues <- as.matrix(likelihoodValues) %*% weight

    return(likelihoodValues)
  }

  if (inherits(dpobj, "mvnormal2") ||
      inherits(dpobj$mixingDistribution, "mvnormal2")) {
    likelihoodValues <- vapply(
      seq_len(nrow(dpobj$data)),
      function(i) Likelihood(dpobj$mixingDistribution,
                             dpobj$data[i, , drop = FALSE],
                             clusters_parameters),
      numeric(dpobj$numberClusters)
    )

    dim(likelihoodValues) <- c(nrow(dpobj$data), dpobj$numberClusters)

    weight <- dpobj$pointsPerCluster / dpobj$n

    likelihoodValues <- as.matrix(likelihoodValues) %*% weight

    return(likelihoodValues)
  }

  if (inherits(dpobj, "mvnormal") ||
      inherits(dpobj$mixingDistribution, "mvnormal")) {
    covModel <- if (is.null(dpobj$mixingDistribution$priorParameters$covModel)) {
      "FULL"
    } else {
      as.character(dpobj$mixingDistribution$priorParameters$covModel)
    }

    if (!identical(covModel, "FULL")) {
      stop("LikelihoodDP for mvnormal in dirichletprocess is only supported for covModel = 'FULL'.",
           call. = FALSE)
    }

    likelihoodValues <- vapply(
      seq_len(nrow(dpobj$data)),
      function(i) Likelihood(dpobj$mixingDistribution,
                             dpobj$data[i, , drop = FALSE],
                             clusters_parameters),
      numeric(dpobj$numberClusters)
    )

    dim(likelihoodValues) <- c(nrow(dpobj$data), dpobj$numberClusters)

    weight <- dpobj$pointsPerCluster / dpobj$n

    likelihoodValues <- as.matrix(likelihoodValues) %*% weight

    return(likelihoodValues)
  }

  # For multivariate normal with pre-allocated arrays, we need to extract only active clusters
  if (inherits(dpobj, "mvnormal") && is.list(clusters_parameters)) {
    # Create a subset of parameters for only active clusters
    active_params <- list()
    for (i in seq_along(clusters_parameters)) {
      param_dims <- dim(clusters_parameters[[i]])
      if (length(param_dims) == 3 && param_dims[3] > dpobj$numberClusters) {
        # Extract only the active clusters
        if (dpobj$numberClusters == 1) {
          active_params[[i]] <- array(clusters_parameters[[i]][, , 1:dpobj$numberClusters, drop = FALSE],
                                      dim = c(param_dims[1], param_dims[2], dpobj$numberClusters))
        } else {
          active_params[[i]] <- clusters_parameters[[i]][, , 1:dpobj$numberClusters, drop = FALSE]
        }
      } else {
        active_params[[i]] <- clusters_parameters[[i]]
      }
    }
    clusters_parameters <- active_params
    names(clusters_parameters) <- names(dpobj$clusterParameters)
  }

  # Get the actual structure of cluster parameters to determine expected result length
  actual_num_clusters <- 1
  if (is.list(clusters_parameters) && length(clusters_parameters) > 0) {
    first_param <- clusters_parameters[[1]]
    if (is.array(first_param) && length(dim(first_param)) == 3) {
      actual_num_clusters <- dim(first_param)[3]
    } else if (is.list(first_param) && length(first_param) > 0) {
      # Handle nested structure
      actual_num_clusters <- length(first_param)
    }
  }
  
  # Use the actual number of clusters from the parameter structure
  expected_clusters <- min(dpobj$numberClusters, actual_num_clusters)
  
  likelihoodValues <- vapply(seq_len(nrow(dpobj$data)),
                             function(i) {
                               lik <- Likelihood(dpobj$mixingDistribution,
                                                 dpobj$data[i, , drop=FALSE],
                                                 clusters_parameters)
                               # Ensure we return the right number of values
                               if (length(lik) > expected_clusters) {
                                 lik[1:expected_clusters]
                               } else if (length(lik) < expected_clusters) {
                                 # Pad with zeros if needed
                                 c(lik, rep(0, expected_clusters - length(lik)))
                               } else {
                                 lik
                               }
                             },
                             numeric(expected_clusters))

  if (expected_clusters == 1) {
    likelihoodValues <- matrix(likelihoodValues, ncol = 1)
  } else {
    likelihoodValues <- t(likelihoodValues)
  }

  # Use weights for the expected number of clusters
  weight <- dpobj$pointsPerCluster[1:expected_clusters] / dpobj$n

  likelihoodValues <- likelihoodValues %*% weight

  return(likelihoodValues)
}
