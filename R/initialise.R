#' Initialise a Dirichlet process object
#'
#' Initialise a Dirichlet process object by assigning all the data points to a single cluster with a posterior or prior draw for parameters.
#'
#' @param dpObj A Dirichlet process object.
#' @param posterior TRUE/FALSE value for whether the cluster parameters should be from the posterior. If false then the values are from the prior.
#' @param m Number of auxiliary variables to use for a non-conjugate mixing distribution. Defaults to m=3. See \code{\link{ClusterComponentUpdate}} for more details on m.
#' @param verbose Logical flag indicating whether to output the acceptance ratio for non-conjugate mixtures.
#' @param numInitialClusters Number of clusters to initialise with.
#' @param ... Additional arguments passed to specific methods.
#' @return A Dirichlet process object that has initial cluster allocations.
#' @export
Initialise <- function(dpObj, posterior = TRUE, m=3, verbose=TRUE, numInitialClusters = 1){
  UseMethod("Initialise", dpObj)
}

#' @export
Initialise.conjugate <- function(dpObj, posterior = TRUE, m=NULL, verbose=NULL, numInitialClusters = 1) {

  dpObj$clusterLabels <- rep_len(seq_len(numInitialClusters), length.out = dpObj$n)
  dpObj$numberClusters <- numInitialClusters
  dpObj$pointsPerCluster <- vapply(seq_len(numInitialClusters), function(x) sum(dpObj$clusterLabels == x), numeric(1))

  if (posterior && numInitialClusters == 1) {
    dpObj$clusterParameters <- PosteriorDraw(dpObj$mixingDistribution, dpObj$data, 1)
  } else {
    dpObj$clusterParameters <- PriorDraw(dpObj$mixingDistribution, numInitialClusters)
  }

  covModel <- if (inherits(dpObj, "mvnormal")) {
    if (is.null(dpObj$mixingDistribution$priorParameters$covModel)) {
      "FULL"
    } else {
      as.character(dpObj$mixingDistribution$priorParameters$covModel)
    }
  } else {
    NULL
  }

  # The repaired R mvnormal path stores only active FULL-model clusters.
  # Keep the legacy preallocated layout only for the extra non-FULL variants.
  if (inherits(dpObj, "mvnormal") && !identical(covModel, "FULL")) {
    # Get current dimensions
    mu_dim <- dim(dpObj$clusterParameters$mu)
    sig_dim <- dim(dpObj$clusterParameters$sig)

    # Handle case where mu_dim might not have 3 dimensions (e.g., 1D data)
    if (is.null(mu_dim) || length(mu_dim) < 3) {
      # For 1D data, mu might be a scalar or vector
      if (is.null(mu_dim)) {
        # It's a scalar or vector, convert to proper 3D array
        d <- ncol(dpObj$data)  # Get dimensions from data
        if (is.null(d) || d <= 0) d <- 1  # Default to 1D if issues
        
        # For scalars, convert to array format
        if (length(dpObj$clusterParameters$mu) == 1) {
          n_clusters <- 1
          dpObj$clusterParameters$mu <- array(dpObj$clusterParameters$mu, dim = c(1, d, n_clusters))
        } else {
          n_clusters <- length(dpObj$clusterParameters$mu)
          dpObj$clusterParameters$mu <- array(dpObj$clusterParameters$mu, dim = c(1, d, n_clusters))
        }
        
        # Handle sig dimensions - ensure it's properly formatted
        if (length(dim(dpObj$clusterParameters$sig)) == 3) {
          # Already in 3D format, keep as is
        } else {
          # Convert to 3D if needed
          dpObj$clusterParameters$sig <- array(dpObj$clusterParameters$sig, dim = c(d, d, n_clusters))
        }
      } else if (length(mu_dim) == 2) {
        # It's a 2D array, add the third dimension
        d <- mu_dim[1]
        n_clusters <- mu_dim[2]
        dpObj$clusterParameters$mu <- array(dpObj$clusterParameters$mu, dim = c(1, d, n_clusters))
        # For constrained models, sig dimensions are different
        if (exists("priorParameters", dpObj$mixingDistribution) && 
            is.null(dpObj$mixingDistribution$priorParameters$covModel) == FALSE &&
            dpObj$mixingDistribution$priorParameters$covModel != "FULL") {
          # Keep sig as is for constrained models
        } else {
          dpObj$clusterParameters$sig <- array(dpObj$clusterParameters$sig, dim = c(d, d, n_clusters))
        }
      }
      # Update dimensions
      mu_dim <- dim(dpObj$clusterParameters$mu)
      sig_dim <- dim(dpObj$clusterParameters$sig)
    }

    # Ensure we have at least enough slots for the data size or 50, whichever is larger
    min_slots <- max(50, dpObj$n, numInitialClusters * 10)

    # Get current number of clusters from mu_dim, handling dimension issues
    current_clusters <- if (is.null(mu_dim) || length(mu_dim) < 3 || is.na(mu_dim[3])) {
      1  # Default to 1 cluster if dimension is problematic
    } else {
      mu_dim[3]
    }

    if (current_clusters < min_slots) {
      # Expand arrays
      # Handle dimension access safely
      if (is.null(mu_dim) || length(mu_dim) < 2) {
        # For E/V models, mu is a vector, infer dimension from data
        d <- ncol(dpObj$data)
      } else {
        d <- mu_dim[2]
      }

      # Create new arrays with more space
      new_mu <- array(NA_real_, dim = c(1, d, min_slots))
      
      # For constrained models, sig dimensions are different
      if (exists("priorParameters", dpObj$mixingDistribution) && 
          !is.null(dpObj$mixingDistribution$priorParameters$covModel) &&
          dpObj$mixingDistribution$priorParameters$covModel != "FULL") {
        # Get number of parameters for this covariance model
        nParams <- dim(dpObj$clusterParameters$sig)[1]
        new_sig <- array(NA_real_, dim = c(nParams, min_slots))
        
        # Copy existing parameters
        new_mu[, , 1:current_clusters] <- dpObj$clusterParameters$mu
        
        # For constrained models, sig might be 3D but we need 2D
        if (length(sig_dim) == 3) {
          # Convert 3D sig to 2D for constrained models
          sig_2d <- matrix(dpObj$clusterParameters$sig, nrow = sig_dim[1], ncol = sig_dim[3])
          new_sig[, 1:sig_dim[3]] <- sig_2d
        } else {
          new_sig[, 1:sig_dim[2]] <- dpObj$clusterParameters$sig
        }
        
        # Fill remaining slots with prior draws
        if (current_clusters < min_slots) {
          extra_params <- PriorDraw(dpObj$mixingDistribution, min_slots - current_clusters)
          
          # Convert extra mu to 3D if needed
          if (is.null(dim(extra_params$mu))) {
            # Vector to 3D array
            extra_mu <- array(extra_params$mu, dim = c(1, d, length(extra_params$mu)))
          } else {
            extra_mu <- extra_params$mu
          }
          
          new_mu[, , (current_clusters+1):min_slots] <- extra_mu
          
          # Convert extra sig to 2D if needed
          if (length(dim(extra_params$sig)) == 3) {
            # Convert 3D sig to 2D for constrained models
            extra_sig_dims <- dim(extra_params$sig)
            extra_sig_2d <- matrix(extra_params$sig, nrow = extra_sig_dims[1], ncol = extra_sig_dims[3])
            new_sig[, (current_clusters+1):min_slots] <- extra_sig_2d
          } else {
            new_sig[, (current_clusters+1):min_slots] <- extra_params$sig
          }
        }
      } else {
        # Full covariance model
        new_sig <- array(NA_real_, dim = c(d, d, min_slots))
        
        # Copy existing parameters
        new_mu[, , 1:current_clusters] <- dpObj$clusterParameters$mu
        new_sig[, , 1:sig_dim[3]] <- dpObj$clusterParameters$sig
        
        # Fill remaining slots with prior draws
        if (current_clusters < min_slots) {
          extra_params <- PriorDraw(dpObj$mixingDistribution, min_slots - current_clusters)
          new_mu[, , (current_clusters+1):min_slots] <- extra_params$mu
          new_sig[, , (sig_dim[3]+1):min_slots] <- extra_params$sig
        }
      }

      dpObj$clusterParameters$mu <- new_mu
      dpObj$clusterParameters$sig <- new_sig
    }
  }

  dpObj <- InitialisePredictive(dpObj)

  return(dpObj)
}

#' @export
#' @rdname Initialise  
Initialise.hierarchical <- function(dpObj, posterior = TRUE, m = 3, verbose = TRUE, numInitialClusters = 1) {
  # For hierarchical objects that are not individual DPs, we don't initialize
  # Individual DPs are initialized separately in the hierarchical constructor
  if (!"indDP" %in% names(dpObj)) {
    # This is an individual DP with hierarchical mixing distribution
    # Delegate to the appropriate method based on the second class
    if (inherits(dpObj, "beta")) {
      return(Initialise.beta(dpObj, m = m, verbose = verbose))
    } else if (inherits(dpObj, "mvnormal") || inherits(dpObj, "mvnormal2")) {
      return(Initialise.conjugate(dpObj, posterior = posterior, m = m, verbose = verbose, numInitialClusters = numInitialClusters))
    } else {
      return(Initialise.nonconjugate(dpObj, posterior = posterior, m = m, verbose = verbose, numInitialClusters = numInitialClusters))
    }
  }
  # For the hierarchical container object, just return as-is
  return(dpObj)
}

#'@export
Initialise.nonconjugate <- function(dpObj, posterior = TRUE, m = 3, verbose = TRUE, numInitialClusters=1) {

  # dpObj$clusterLabels <- 1:dpObj$n dpObj$numberClusters <- dpObj$n
  # dpObj$pointsPerCluster <- rep(1, dpObj$n) dpObj$clusterParameters <-
  # PosteriorDraw(dpObj$MixingDistribution, dpObj$data, dpObj$n)
  dpObj$clusterLabels <- rep(1, dpObj$n)
  dpObj$numberClusters <- 1
  dpObj$pointsPerCluster <- dpObj$n

  if (posterior) {
    post_draws <- PosteriorDraw(dpObj$mixingDistribution, dpObj$data, 1000)

    if (verbose)
      cat(paste("Accept Ratio: ",
                length(unique(c(post_draws[[1]])))/1000,
                "\n"))

    dpObj$clusterParameters <- lapply(post_draws, function(x) x[, , 1000, drop = FALSE])


    # dpObj$clusterParameters <- list(post_draws[[1]][, , 1000, drop = FALSE],
                                    # post_draws[[2]][, , 1000, drop = FALSE])
  } else {
    dpObj$clusterParameters <- PriorDraw(dpObj$mixingDistribution, 1)
  }

  dpObj$m <- m

  return(dpObj)
}


InitialisePredictive <- function(dpObj) UseMethod("InitialisePredictive", dpObj)

#' @export
InitialisePredictive.conjugate <- function(dpObj) {

  dpObj$predictiveArray <- Predictive(dpObj$mixingDistribution, dpObj$data)

  return(dpObj)
}

#' @export
InitialisePredictive.nonconjugate <- function(dpObj) {
  return(dpObj)
}

# Covariance model-specific Initialise methods
#' @export
#' @rdname Initialise
Initialise.mvnormal.E <- function(dpObj, posterior = TRUE, m = NULL, verbose = NULL, numInitialClusters = 1) {
  # Call base mvnormal initialise with covariance model handling
  return(Initialise.conjugate(dpObj, posterior, m, verbose, numInitialClusters))
}

#' @export
#' @rdname Initialise
Initialise.mvnormal.V <- function(dpObj, posterior = TRUE, m = NULL, verbose = NULL, numInitialClusters = 1) {
  return(Initialise.conjugate(dpObj, posterior, m, verbose, numInitialClusters))
}

#' @export
#' @rdname Initialise
Initialise.mvnormal.EII <- function(dpObj, posterior = TRUE, m = NULL, verbose = NULL, numInitialClusters = 1) {
  return(Initialise.conjugate(dpObj, posterior, m, verbose, numInitialClusters))
}

#' @export
#' @rdname Initialise
Initialise.mvnormal.VII <- function(dpObj, posterior = TRUE, m = NULL, verbose = NULL, numInitialClusters = 1) {
  return(Initialise.conjugate(dpObj, posterior, m, verbose, numInitialClusters))
}

#' @export
#' @rdname Initialise
Initialise.mvnormal.EEI <- function(dpObj, posterior = TRUE, m = NULL, verbose = NULL, numInitialClusters = 1) {
  return(Initialise.conjugate(dpObj, posterior, m, verbose, numInitialClusters))
}

#' @export
#' @rdname Initialise
Initialise.mvnormal.VEI <- function(dpObj, posterior = TRUE, m = NULL, verbose = NULL, numInitialClusters = 1) {
  return(Initialise.conjugate(dpObj, posterior, m, verbose, numInitialClusters))
}

#' @export
#' @rdname Initialise
Initialise.mvnormal.EVI <- function(dpObj, posterior = TRUE, m = NULL, verbose = NULL, numInitialClusters = 1) {
  return(Initialise.conjugate(dpObj, posterior, m, verbose, numInitialClusters))
}

#' @export
#' @rdname Initialise
Initialise.mvnormal.VVI <- function(dpObj, posterior = TRUE, m = NULL, verbose = NULL, numInitialClusters = 1) {
  return(Initialise.conjugate(dpObj, posterior, m, verbose, numInitialClusters))
}


