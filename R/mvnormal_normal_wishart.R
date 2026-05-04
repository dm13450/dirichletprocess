#' Create a Multivariate Normal mixing distribution with conjugate prior
#'
#' Creates a multivariate normal mixing distribution with Normal-Wishart conjugate prior.
#' The base measure is \eqn{G_0(\mu, \Sigma | \mu_0, \kappa_0, \nu, \Lambda) = N(\mu | \mu_0, \Sigma/\kappa_0) * IW(\Sigma | \nu, \Lambda)}
#' Non-\code{"FULL"} covariance models are currently rejected in the live package.
#'
#' @param priorParameters A list containing prior parameters:
#'   \describe{
#'     \item{mu0}{Prior mean vector}
#'     \item{kappa0}{Prior precision parameter for the mean}
#'     \item{nu}{Prior degrees of freedom for the covariance}
#'     \item{Lambda}{Prior scale matrix for the covariance}
#'     \item{covModel}{Only \code{"FULL"} is currently supported in the live
#'                     package and is the default.}
#'   }
#' @return A mixing distribution object
#' @export
MvnormalCreate <- function(priorParameters) {

  # Set default parameters if missing
  if (missing(priorParameters) || is.null(priorParameters)) {
    priorParameters <- list(
      mu0 = c(0, 0),
      kappa0 = 1,
      nu = 3,
      Lambda = diag(2),
      covModel = "FULL"
    )
  }

  # Handle the case where a mixing distribution object is passed
  if (is.list(priorParameters) && !is.null(priorParameters$priorParameters)) {
    # A mixing distribution object was passed, extract the priorParameters
    priorParameters <- priorParameters$priorParameters
  }
  
  # Handle the case where a vector is passed instead of a list
  if (is.numeric(priorParameters) && !is.list(priorParameters)) {
    # Assume it's the mean vector
    d <- length(priorParameters)
    priorParameters <- list(
      mu0 = priorParameters,
      kappa0 = 1,
      nu = d + 1,
      Lambda = diag(d),
      covModel = "FULL"
    )
  }

  # Set default covariance model if not specified
  if (is.null(priorParameters$covModel)) {
    priorParameters$covModel <- "FULL"
  }
  
  # Fill in missing parameters with defaults
  # Default to 2D if mu0 is not specified
  default_d <- if (is.null(priorParameters$mu0)) 2 else length(priorParameters$mu0)
  
  if (is.null(priorParameters$mu0)) {
    priorParameters$mu0 <- rep(0, default_d)
  }
  if (is.null(priorParameters$kappa0)) {
    priorParameters$kappa0 <- 1
  }
  if (is.null(priorParameters$nu)) {
    priorParameters$nu <- default_d + 1
  }
  if (is.null(priorParameters$Lambda)) {
    priorParameters$Lambda <- diag(default_d)
  }

  # Validate covariance model against the current live support boundary.
  if (!identical(as.character(priorParameters$covModel), "FULL")) {
    stop("DirichletProcessMvnormal() currently supports only covModel = 'FULL'.",
         call. = FALSE)
  }

  # Ensure mu0 is a vector
  if (is.matrix(priorParameters$mu0)) {
    priorParameters$mu0 <- as.vector(priorParameters$mu0)
  }

  # Validate dimensions for univariate models
  d <- length(priorParameters$mu0)
  if (priorParameters$covModel %in% c("E", "V") && d != 1) {
    stop("Models 'E' and 'V' are for univariate data only (d=1)")
  }

  # Ensure Lambda is a matrix
  if (is.vector(priorParameters$Lambda)) {
    if (length(priorParameters$Lambda) == 1) {
      priorParameters$Lambda <- diag(d) * priorParameters$Lambda
    } else if (length(priorParameters$Lambda) == d^2) {
      priorParameters$Lambda <- matrix(priorParameters$Lambda, nrow = d)
    }
  }

  # Adjust Lambda based on covariance model
  if (priorParameters$covModel %in% c("EII", "VII")) {
    # For spherical models, Lambda should be proportional to identity
    if (!is.matrix(priorParameters$Lambda)) {
      priorParameters$Lambda <- diag(d) * priorParameters$Lambda
    } else {
      # Convert to spherical form (average of diagonal)
      lambda_val <- mean(diag(priorParameters$Lambda))
      priorParameters$Lambda <- diag(d) * lambda_val
    }
  } else if (priorParameters$covModel %in% c("EEI", "VEI", "EVI", "VVI")) {
    # For diagonal models, ensure Lambda is diagonal
    if (!is.matrix(priorParameters$Lambda)) {
      priorParameters$Lambda <- diag(d) * priorParameters$Lambda
    } else {
      priorParameters$Lambda <- diag(diag(priorParameters$Lambda))
    }
  }

  # Check other parameters
  if (length(priorParameters$kappa0) != 1 || priorParameters$kappa0 <= 0) {
    stop("kappa0 must be a positive scalar")
  }

  if (length(priorParameters$nu) != 1 || priorParameters$nu <= d - 1) {
    stop("nu must be a scalar greater than d-1")
  }

  # Create the object using MixingDistribution constructor
  mdObj <- MixingDistribution("mvnormal", priorParameters, "conjugate")

  # Add covariance model-specific class
  if (priorParameters$covModel != "FULL") {
    class(mdObj) <- c("list", paste0("mvnormal.", priorParameters$covModel), 
                      "mvnormal", "conjugate")
  }
  # For FULL model, keep the existing class structure from MixingDistribution

  return(mdObj)
}

#' @export
#' @rdname Likelihood
Likelihood.mvnormal <- function(mdObj, x, theta) {
  covModel <- if (is.null(mdObj$priorParameters$covModel)) "FULL" else as.character(mdObj$priorParameters$covModel)

  if (identical(covModel, "FULL")) {
    if (!is.matrix(x)) {
      x <- matrix(x, nrow = 1)
    }

    if (!is.list(theta)) {
      stop("theta must be a list")
    }

    if (all(c("mu", "sig") %in% names(theta))) {
      mu_array <- theta$mu
      sig_array <- theta$sig
    } else if (length(theta) >= 2 && is.null(names(theta))) {
      mu_array <- theta[[1]]
      sig_array <- theta[[2]]
    } else {
      stop("theta must be a list with either named components (mu, sig) or two unnamed components")
    }

    n_clusters <- if (!is.null(dim(mu_array)) && length(dim(mu_array)) >= 3) {
      dim(mu_array)[3]
    } else if (!is.null(dim(sig_array)) && length(dim(sig_array)) >= 3) {
      dim(sig_array)[3]
    } else {
      1L
    }

    y <- vapply(
      seq_len(n_clusters),
      function(i) {
        mvtnorm::dmvnorm(
          x,
          mean = as.vector(mu_array[, , i, drop = TRUE]),
          sigma = sig_array[, , i, drop = TRUE]
        )
      },
      numeric(nrow(x))
    )

    return(y)
  }

  if (using_cpp_samplers()) {
    # Ensure x is a vector (for single observation)
    if (is.matrix(x)) {
      if (nrow(x) > 1) {
        stop("Likelihood expects a single observation")
      }
      x <- as.vector(x)
    }
    
    # The theta parameter is the full cluster parameters structure
    # We need to process it for multiple clusters
    d <- length(x)
    
    # Check if theta contains mu and sig fields
    if (!is.list(theta)) {
      stop("theta must be a list")
    }
    
    # Handle different parameter formats
    if (all(c("mu", "sig") %in% names(theta))) {
      # Named list format: list(mu = ..., sig = ...)
      mu_array <- theta$mu
      sig_array <- theta$sig
    } else if (length(theta) >= 2 && is.null(names(theta))) {
      # Unnamed list format from LikelihoodDP: list(mu_array, sig_array)
      mu_array <- theta[[1]]
      sig_array <- theta[[2]]
    } else {
      # Fallback for unexpected theta structure - return small likelihood
      warning("Unexpected theta structure in mvnormal likelihood - using fallback")
      return(rep(1e-100, ifelse(is.matrix(x), nrow(x), 1)))
    }
    
    # Get dimensions
    mu_dim <- dim(mu_array)
    sig_dim <- dim(sig_array)
    
    # Extract number of clusters
    if (is.null(mu_dim) || length(mu_dim) < 3) {
      # Handle case where parameters are not 3D arrays yet
      if (is.null(mu_dim)) {
        # mu_array is a vector, likely single cluster
        num_clusters <- 1
      } else if (length(mu_dim) == 2) {
        # mu_array is a 2D array, clusters are in the second dimension
        num_clusters <- mu_dim[2]
      } else {
        num_clusters <- 1
      }
    } else {
      num_clusters <- mu_dim[3]
    }
    
    # Calculate likelihood for each cluster
    result <- numeric(num_clusters)
    for (k in 1:num_clusters) {
      # Extract parameters for cluster k
      if (is.null(mu_dim)) {
        # mu_array is a vector, single cluster
        cluster_mu <- mu_array
        cluster_sig <- sig_array
      } else if (length(mu_dim) == 2) {
        # 2D array, clusters in second dimension
        # Check if mu_array has the expected dimensions
        d <- length(x)
        if (ncol(mu_array) >= k && nrow(mu_array) == d) {
          cluster_mu <- mu_array[, k]
        } else if (nrow(mu_array) >= k && ncol(mu_array) == d) {
          cluster_mu <- mu_array[k, ]
        } else {
          # Try flat storage
          total_mu_params <- length(mu_array)
          if (total_mu_params %% d == 0) {
            start_idx <- (k - 1) * d + 1
            end_idx <- k * d
            if (end_idx <= total_mu_params) {
              cluster_mu <- as.vector(mu_array)[start_idx:end_idx]
            } else {
              cluster_mu <- mu_array
            }
          } else {
            cluster_mu <- mu_array
          }
        }
        if (mdObj$priorParameters$covModel == "FULL") {
          # Check if sig_array has 3 dimensions
          if (!is.null(sig_dim) && length(sig_dim) >= 3) {
            cluster_sig <- sig_array[, , k]
          } else if (!is.null(sig_dim) && length(sig_dim) == 2) {
            # For 2D sig_array, extract parameters for cluster k
            d <- length(x)
            nCovParams <- getNumCovParams(d, mdObj$priorParameters$covModel)
            
            # Check various possible storage patterns
            if (ncol(sig_array) >= k && nrow(sig_array) == nCovParams) {
              # nCovParams x nClusters format
              cluster_sig <- sig_array[, k]
            } else if (nrow(sig_array) >= k && ncol(sig_array) == nCovParams) {
              # nClusters x nCovParams format
              cluster_sig <- sig_array[k, ]
            } else {
              # Try flat storage: assume sigma contains nCovParams per cluster
              total_params <- length(sig_array)
              if (total_params %% nCovParams == 0) {
                # Flat storage: extract the right chunk
                start_idx <- (k - 1) * nCovParams + 1
                end_idx <- k * nCovParams
                if (end_idx <= total_params) {
                  cluster_sig <- as.vector(sig_array)[start_idx:end_idx]
                } else {
                  # Single cluster case
                  cluster_sig <- sig_array
                }
              } else {
                # Single cluster case
                cluster_sig <- sig_array
              }
            }
          } else {
            # Single cluster case - sig_array should be the covariance matrix
            cluster_sig <- sig_array
          }
        } else {
          cluster_sig <- sig_array[, k]
        }
      } else if (length(mu_dim) == 3) {
        # 3D array, clusters in third dimension
        cluster_mu <- mu_array[, , k]
        if (mdObj$priorParameters$covModel == "FULL") {
          # Check if sig_array has 3 dimensions
          if (!is.null(sig_dim) && length(sig_dim) >= 3) {
            cluster_sig <- sig_array[, , k]
          } else if (!is.null(sig_dim) && length(sig_dim) == 2) {
            # For 2D sig_array, extract parameters for cluster k
            d <- length(x)
            nCovParams <- getNumCovParams(d, mdObj$priorParameters$covModel)
            
            # Check various possible storage patterns
            if (ncol(sig_array) >= k && nrow(sig_array) == nCovParams) {
              # nCovParams x nClusters format
              cluster_sig <- sig_array[, k]
            } else if (nrow(sig_array) >= k && ncol(sig_array) == nCovParams) {
              # nClusters x nCovParams format
              cluster_sig <- sig_array[k, ]
            } else {
              # Try flat storage: assume sigma contains nCovParams per cluster
              total_params <- length(sig_array)
              if (total_params %% nCovParams == 0) {
                # Flat storage: extract the right chunk
                start_idx <- (k - 1) * nCovParams + 1
                end_idx <- k * nCovParams
                if (end_idx <= total_params) {
                  cluster_sig <- as.vector(sig_array)[start_idx:end_idx]
                } else {
                  # Single cluster case
                  cluster_sig <- sig_array
                }
              } else {
                # Single cluster case
                cluster_sig <- sig_array
              }
            }
          } else {
            # Single cluster case - sig_array should be the covariance matrix
            cluster_sig <- sig_array
          }
        } else {
          cluster_sig <- sig_array[, k]
        }
      } else {
        # Single value case
        cluster_mu <- mu_array
        cluster_sig <- sig_array
      }
      
      # Create individual cluster theta
      cluster_theta <- list(mu = cluster_mu, sig = cluster_sig)
      
      # Call wrapper for this cluster
      result[k] <- mvnormal_likelihood_wrapper_cpp(x, cluster_theta, mdObj$priorParameters)
    }
    
    return(result)
  }

  # R implementation
  if (!is.matrix(x)) {
    x <- matrix(x, nrow = 1)
  }

  # Check if theta contains mu and sig fields
  if (!is.list(theta)) {
    stop("theta must be a list")
  }
  
  # Handle different parameter formats
  if (all(c("mu", "sig") %in% names(theta))) {
    # Named list format: list(mu = ..., sig = ...)
    mu_array <- theta$mu
    sig_array <- theta$sig
  } else if (length(theta) >= 2 && is.null(names(theta))) {
    # Unnamed list format from LikelihoodDP: list(mu_array, sig_array)
    mu_array <- theta[[1]]
    sig_array <- theta[[2]]
  } else {
    stop("theta must be a list with either named components (mu, sig) or two unnamed components")
  }
  
  # Get dimensions
  mu_dim <- dim(mu_array)
  sig_dim <- dim(sig_array)
  
  # Extract parameters accounting for covariance model
  d <- ncol(x)
  
  # Extract number of clusters
  if (is.null(mu_dim) || length(mu_dim) < 3) {
    # Handle case where parameters are not 3D arrays yet
    if (is.null(mu_dim)) {
      # mu_array is a vector, likely single cluster
      num_clusters <- 1
    } else if (length(mu_dim) == 2) {
      # mu_array is a 2D array, clusters are in the second dimension
      num_clusters <- mu_dim[2]
    } else {
      num_clusters <- 1
    }
  } else {
    num_clusters <- mu_dim[3]
  }
  
  # Calculate likelihood for each cluster
  result <- numeric(num_clusters)
  for (k in 1:num_clusters) {
    # Extract parameters for cluster k
    if (is.null(mu_dim)) {
      # mu_array is a vector, single cluster
      cluster_mu <- mu_array
      cluster_sig <- sig_array
    } else if (length(mu_dim) == 2) {
      # 2D array, clusters in second dimension
      cluster_mu <- mu_array[, k]
      if (mdObj$priorParameters$covModel == "FULL") {
        cluster_sig <- sig_array[, , k]
      } else {
        cluster_sig <- sig_array[, k]
      }
    } else if (length(mu_dim) == 3) {
      # 3D array, clusters in third dimension
      cluster_mu <- mu_array[, , k]
      if (mdObj$priorParameters$covModel == "FULL") {
        cluster_sig <- sig_array[, , k]
      } else {
        cluster_sig <- sig_array[, k]
      }
    } else {
      # Single value case
      cluster_mu <- mu_array
      cluster_sig <- sig_array
    }
    
    # Convert to vectors/matrices for computation
    mu <- as.vector(cluster_mu)
    
    # Handle covariance based on model
    if (mdObj$priorParameters$covModel == "FULL") {
      # sig is precision matrix for full model
      sig_inv <- matrix(cluster_sig, ncol = d)
      sig_matrix <- solve(sig_inv)
    } else {
      # Reconstruct covariance from parameters
      sig_matrix <- reconstructCovarianceMatrix(cluster_sig, d,
                                                mdObj$priorParameters$covModel)
    }
    
    # Use mvtnorm for likelihood calculation
    result[k] <- mvtnorm::dmvnorm(x, mean = mu, sigma = sig_matrix)
  }
  
  return(result)
}

#' @export
#' @rdname PosteriorParameters
PosteriorParameters.mvnormal <- function(mdObj, x) {
  if (using_cpp_samplers()) {
    if (!is.matrix(x)) {
      x <- matrix(x, nrow = 1)
    }
    return(mvnormal_posterior_parameters_cpp(mdObj$priorParameters, x))
  }

  # R implementation
  if (!is.matrix(x)) {
    x <- matrix(x, nrow = 1)
  }

  priorParameters <- mdObj$priorParameters
  n <- nrow(x)

  if (n == 0) {
    return(list(
      mu_n = priorParameters$mu0,
      t_n = priorParameters$Lambda,
      Lambda_n = priorParameters$Lambda,
      kappa_n = priorParameters$kappa0,
      nu_n = priorParameters$nu
    ))
  }

  d <- ncol(x)
  x_bar <- colMeans(x)

  # Posterior parameters for mean
  kappa_n <- priorParameters$kappa0 + n
  mu_n <- (priorParameters$kappa0 * priorParameters$mu0 + n * x_bar) / kappa_n
  nu_n <- priorParameters$nu + n

  # Compute scatter matrix based on covariance model
  if (n > 1) {
    if (mdObj$priorParameters$covModel %in% c("E", "V")) {
      # Univariate
      S <- (n - 1) * var(x)
      S <- matrix(S, 1, 1)
    } else if (mdObj$priorParameters$covModel %in% c("EII", "VII")) {
      # Spherical
      centered <- sweep(x, 2, x_bar)
      trace_S <- sum(centered^2) / (n - 1)
      S <- diag(d) * (trace_S / d)
    } else if (mdObj$priorParameters$covModel %in% c("EEI", "VEI", "EVI", "VVI")) {
      # Diagonal
      S <- diag(apply(x, 2, var) * (n - 1))
    } else {
      # Full
      S <- (n - 1) * cov(x)
    }
  } else {
    S <- matrix(0, d, d)
  }

  # Update Lambda
  diff <- x_bar - priorParameters$mu0
  
  t_n <- priorParameters$Lambda + S +
    (priorParameters$kappa0 * n / kappa_n) * outer(diff, diff)

  # Ensure symmetry
  t_n <- (t_n + t(t_n)) / 2

  list(
    mu_n = mu_n,
    t_n = t_n,
    Lambda_n = t_n,  # For backward compatibility
    kappa_n = kappa_n,
    nu_n = nu_n
  )
}

#' @export
#' @rdname PriorDraw
PriorDraw.mvnormal <- function(mdObj, n = 1, ...) {
  covModel <- if (is.null(mdObj$priorParameters$covModel)) "FULL" else as.character(mdObj$priorParameters$covModel)

  if (identical(covModel, "FULL")) {
    priorParameters <- mdObj$priorParameters

    sig <- simplify2array(
      lapply(
        seq_len(n),
        function(x) solve(rWishart(1,
                                   priorParameters$nu,
                                   solve(priorParameters$Lambda))[,,1])
      )
    )

    mu <- simplify2array(
      lapply(
        seq_len(n),
        function(x) {
          mvtnorm::rmvnorm(
            1,
            priorParameters$mu0,
            sig[, , x] / priorParameters$kappa0
          )
        }
      )
    )

    return(list(mu = mu, sig = sig))
  }

  if (using_cpp_samplers()) {
    return(mvnormal_prior_draw_cpp(mdObj$priorParameters, n))
  }

  # R implementation
  priorParameters <- mdObj$priorParameters
  d <- length(priorParameters$mu0)

  # Draw from prior using safe rWishart
  sig <- safe_rWishart(n, priorParameters$nu, priorParameters$Lambda)

  if (mdObj$priorParameters$covModel == "FULL") {
    # Full model - return precision matrices
    mu <- simplify2array(
      lapply(seq_len(n),
             function(i) mvtnorm::rmvnorm(1,
                                          priorParameters$mu0,
                                          solve(priorParameters$kappa0 * sig[, , i]))
      )
    )
  } else {
    # Other models - convert and extract parameters
    mu <- matrix(NA, n, d)
    sig_params <- matrix(NA, n, getNumCovParams(d, mdObj$priorParameters$covModel))

    for (i in 1:n) {
      # Convert precision to covariance
      cov_i <- solve(sig[, , i])
      mu[i, ] <- mvtnorm::rmvnorm(1, priorParameters$mu0, cov_i / priorParameters$kappa0)

      # Extract model-specific parameters
      sig_params[i, ] <- extractCovarianceParams(cov_i, mdObj$priorParameters$covModel)
    }

    mu <- t(mu)
    sig <- t(sig_params)
  }

  return(list(mu = mu, sig = sig))
}

#' @export
#' @rdname PosteriorDraw
PosteriorDraw.mvnormal <- function(mdObj, x, n = 1, ...) {
  covModel <- if (is.null(mdObj$priorParameters$covModel)) "FULL" else as.character(mdObj$priorParameters$covModel)

  if (identical(covModel, "FULL")) {
    post_parameters <- PosteriorParameters(mdObj, x)

    sig <- simplify2array(
      lapply(
        seq_len(n),
        function(x) solve(rWishart(1,
                                   post_parameters$nu_n,
                                   solve(post_parameters$t_n))[,,1])
      )
    )

    mu <- simplify2array(
      lapply(
        seq_len(n),
        function(x) {
          mvtnorm::rmvnorm(
            1,
            post_parameters$mu_n,
            sig[, , x] / post_parameters$kappa_n
          )
        }
      )
    )

    return(list(mu = mu, sig = sig))
  }

  if (using_cpp_samplers()) {
    return(mvnormal_posterior_draw_cpp(mdObj$priorParameters, as.matrix(x), n))
  }

  # R implementation
  post_parameters <- PosteriorParameters(mdObj, x)
  d <- length(post_parameters$mu_n)

  # Use safe rWishart to handle numerical edge cases
  sig <- safe_rWishart(n, post_parameters$nu_n, post_parameters$t_n)

  if (mdObj$priorParameters$covModel == "FULL") {
    # Full model
    mu <- simplify2array(
      lapply(seq_len(n),
             function(i) mvtnorm::rmvnorm(1,
                                          post_parameters$mu_n,
                                          solve(post_parameters$kappa_n * sig[, , i]))
      )
    )
  } else {
    # Other models
    mu <- matrix(NA, n, d)
    sig_params <- matrix(NA, n, getNumCovParams(d, mdObj$priorParameters$covModel))

    for (i in 1:n) {
      cov_i <- solve(sig[, , i])
      mu[i, ] <- mvtnorm::rmvnorm(1, post_parameters$mu_n,
                                  cov_i / post_parameters$kappa_n)
      sig_params[i, ] <- extractCovarianceParams(cov_i, mdObj$priorParameters$covModel)
    }

    mu <- t(mu)
    sig <- t(sig_params)
  }

  return(list(mu = mu, sig = sig))
}

#' @export
#' @rdname Predictive
Predictive.mvnormal <- function(mdObj, x) {
  covModel <- if (is.null(mdObj$priorParameters$covModel)) "FULL" else as.character(mdObj$priorParameters$covModel)

  if (identical(covModel, "FULL")) {
    if (!is.matrix(x)) {
      x <- matrix(x, nrow = 1)
    }

    priorParameters <- mdObj$priorParameters
    pred <- numeric(nrow(x))
    d <- ncol(x)

    for (i in seq_along(pred)) {
      post_params <- PosteriorParameters(mdObj, x[i, , drop = FALSE])

      pred[i] <- (pi^(-nrow(x[i, , drop = FALSE]) * d / 2))
      pred[i] <- pred[i] * (priorParameters$kappa0 / post_params$kappa_n)^(d / 2)
      pred[i] <- pred[i] * (det(priorParameters$Lambda)^(priorParameters$nu / 2)) /
        (det(post_params$t_n)^(post_params$nu_n / 2))

      if (pred[i] > 0) {
        gamma_contrib <- prod(vapply(
          seq_len(d),
          function(j) gamma(priorParameters$nu / 2 +
                              nrow(x[i, , drop = FALSE]) / 2 +
                              (1 - j) / 2),
          numeric(1)
        )) / prod(vapply(
          seq_len(d),
          function(j) gamma(priorParameters$nu / 2 + (1 - j) / 2),
          numeric(1)
        ))

        pred[i] <- pred[i] * gamma_contrib
      }
    }

    return(pred)
  }

  if (using_cpp_samplers()) {
    if (!is.matrix(x)) {
      x <- matrix(x, nrow = 1)
    }
    return(mvnormal_predictive_cpp(mdObj$priorParameters, x))
  }

  # R implementation
  if (!is.matrix(x)) {
    x <- matrix(x, nrow = 1)
  }

  priorParameters <- mdObj$priorParameters
  n <- nrow(x)
  d <- ncol(x)
  result <- numeric(n)

  for (i in 1:n) {
    x_i <- matrix(x[i, ], nrow = 1)
    post_params <- PosteriorParameters(mdObj, x_i)

    # Multivariate t-distribution predictive
    det_Lambda <- det(priorParameters$Lambda)
    det_t_n <- det(post_params$t_n)

    ratio_det <- (det_Lambda / det_t_n)^(priorParameters$nu / 2)
    ratio_kappa <- (priorParameters$kappa0 / post_params$kappa_n)^(d / 2)

    # Multivariate gamma function ratio
    gamma_ratio <- 1
    for (j in 1:d) {
      gamma_ratio <- gamma_ratio *
        gamma((post_params$nu_n + 1 - j) / 2) / gamma((priorParameters$nu + 1 - j) / 2)
    }

    pi_const <- pi^(-d/2)

    result[i] <- pi_const * ratio_kappa * ratio_det * gamma_ratio
  }

  return(result)
}

# Helper functions

#' Get number of covariance parameters for a model
#' @keywords internal
getNumCovParams <- function(d, covModel) {
  switch(EXPR = covModel,
         "E" = 1,
         "V" = 1,
         "EII" = 1,
         "VII" = 1,
         "EEI" = d,
         "VEI" = d + 1,
         "EVI" = d,
         "VVI" = d,
         "FULL" = d * (d + 1) / 2
  )
}

#' Reconstruct covariance matrix from parameters
#' @keywords internal
reconstructCovarianceMatrix <- function(params, d, covModel) {
  sigma <- matrix(0, d, d)

  if (covModel %in% c("E", "V")) {
    # Univariate case
    return(matrix(params[1], 1, 1))
  } else if (covModel %in% c("EII", "VII")) {
    # Spherical
    return(diag(d) * params[1])
  } else if (covModel %in% c("EEI", "EVI", "VVI")) {
    # Diagonal
    return(diag(params[1:d]))
  } else if (covModel == "VEI") {
    # Diagonal with volume and shape
    volume <- params[1]
    shape <- params[2:(d+1)]
    
    # Add numerical stability checks
    if (volume <= 0) {
      volume <- 1e-6  # Small positive value
    }
    
    shape_prod <- prod(shape)
    if (shape_prod <= 0 || is.na(shape_prod) || is.infinite(shape_prod)) {
      shape <- rep(1, d)  # Fall back to identity shape
    } else {
      shape <- shape / (shape_prod^(1/d))
    }
    
    # Ensure volume^(1/d) is valid
    vol_root <- volume^(1/d)
    if (is.na(vol_root) || is.infinite(vol_root)) {
      vol_root <- 1
    }
    
    return(diag(vol_root * shape))
  } else {
    # Full covariance matrix
    idx <- 1
    for (i in 1:d) {
      for (j in 1:i) {
        sigma[i, j] <- params[idx]
        if (i != j) sigma[j, i] <- params[idx]
        idx <- idx + 1
      }
    }
    return(sigma)
  }
}

#' Extract covariance parameters from matrix
#' @keywords internal
extractCovarianceParams <- function(sigma, covModel) {
  d <- nrow(sigma)

  if (covModel %in% c("E", "V")) {
    return(sigma[1, 1])
  } else if (covModel %in% c("EII", "VII")) {
    return(mean(diag(sigma)))
  } else if (covModel %in% c("EEI", "EVI", "VVI")) {
    return(diag(sigma))
  } else if (covModel == "VEI") {
    diag_vals <- diag(sigma)
    
    # Ensure diagonal values are positive
    diag_vals <- pmax(diag_vals, 1e-6)
    
    volume <- prod(diag_vals)
    
    # Add numerical stability checks
    if (volume <= 0 || is.na(volume) || is.infinite(volume)) {
      volume <- 1
      shape <- rep(1, d)
    } else {
      vol_root <- volume^(1/d)
      if (is.na(vol_root) || is.infinite(vol_root)) {
        vol_root <- 1
        shape <- rep(1, d)
      } else {
        shape <- diag_vals / vol_root
      }
    }
    
    return(c(volume, shape))
  } else {
    # Full - extract lower triangular
    params <- numeric(d * (d + 1) / 2)
    idx <- 1
    for (i in 1:d) {
      for (j in 1:i) {
        params[idx] <- sigma[i, j]
        idx <- idx + 1
      }
    }
    return(params)
  }
}

#' C++ wrapper for likelihood calculation
#' @keywords internal
#' @importFrom utils head
#' @export
mvnormal_likelihood_wrapper_cpp <- function(x, theta, priorParams) {
  # Prepare data and parameters
  d <- length(x)
  x_mat <- matrix(x, nrow = 1)
  mu <- as.vector(theta$mu)
  
  # Handle covariance based on model
  if (priorParams$covModel == "FULL") {
    # User-facing FULL mvnormal objects store covariance, not precision.
    if (is.matrix(theta$sig)) {
      if (nrow(theta$sig) == ncol(theta$sig) && nrow(theta$sig) == d) {
        sig_matrix <- theta$sig
      } else {
        nCovParams <- getNumCovParams(d, priorParams$covModel)
        if (length(theta$sig) == nCovParams) {
          sig_matrix <- reconstructCovarianceMatrix(as.vector(theta$sig), d, priorParams$covModel)
        } else {
          stop(paste("Invalid sigma parameters: expected", nCovParams, "parameters for FULL model with", d, "dimensions, got", length(theta$sig)))
        }
      }
    } else {
      nCovParams <- getNumCovParams(d, priorParams$covModel)
      if (length(theta$sig) == nCovParams) {
        sig_matrix <- reconstructCovarianceMatrix(theta$sig, d, priorParams$covModel)
      } else {
        stop(paste("Invalid sigma parameters: expected", nCovParams, "parameters for FULL model with", d, "dimensions, got", length(theta$sig), 
                   "sig_dims:", paste(dim(theta$sig), collapse="x"),
                   "sig_values:", paste(head(as.vector(theta$sig), 10), collapse=",")))
      }
    }
  } else {
    # For constrained models, reconstruct the covariance matrix from parameters
    sig_matrix <- reconstructCovarianceMatrix(theta$sig, d, priorParams$covModel)
  }
  
  # Ensure sig_matrix is properly formatted for C++
  if (!is.matrix(sig_matrix) || nrow(sig_matrix) != d || ncol(sig_matrix) != d) {
    stop(paste("sig_matrix is not a proper", d, "x", d, "matrix. Got dimensions:", 
               paste(dim(sig_matrix), collapse="x"), 
               "class:", class(sig_matrix),
               "length:", length(sig_matrix)))
  }
  
  # Call C++ function with proper covariance matrix
  return(mvnormal_likelihood_cpp(x_mat, mu, sig_matrix))
}

# Covariance model-specific PosteriorDraw methods
#' @export
#' @rdname PosteriorDraw
PosteriorDraw.mvnormal.E <- function(mdObj, x, n = 1, ...) {
  return(PosteriorDraw.mvnormal(mdObj, x, n, ...))
}

#' @export
#' @rdname PosteriorDraw
PosteriorDraw.mvnormal.V <- function(mdObj, x, n = 1, ...) {
  return(PosteriorDraw.mvnormal(mdObj, x, n, ...))
}

#' @export
#' @rdname PosteriorDraw
PosteriorDraw.mvnormal.EII <- function(mdObj, x, n = 1, ...) {
  return(PosteriorDraw.mvnormal(mdObj, x, n, ...))
}

#' @export
#' @rdname PosteriorDraw
PosteriorDraw.mvnormal.VII <- function(mdObj, x, n = 1, ...) {
  return(PosteriorDraw.mvnormal(mdObj, x, n, ...))
}

#' @export
#' @rdname PosteriorDraw
PosteriorDraw.mvnormal.EEI <- function(mdObj, x, n = 1, ...) {
  return(PosteriorDraw.mvnormal(mdObj, x, n, ...))
}

#' @export
#' @rdname PosteriorDraw
PosteriorDraw.mvnormal.VEI <- function(mdObj, x, n = 1, ...) {
  return(PosteriorDraw.mvnormal(mdObj, x, n, ...))
}

#' @export
#' @rdname PosteriorDraw
PosteriorDraw.mvnormal.EVI <- function(mdObj, x, n = 1, ...) {
  return(PosteriorDraw.mvnormal(mdObj, x, n, ...))
}

#' @export
#' @rdname PosteriorDraw
PosteriorDraw.mvnormal.VVI <- function(mdObj, x, n = 1, ...) {
  return(PosteriorDraw.mvnormal(mdObj, x, n, ...))
}

# Covariance model-specific PriorDraw methods
#' @export
#' @rdname PriorDraw
PriorDraw.mvnormal.E <- function(mdObj, n = 1, ...) {
  return(PriorDraw.mvnormal(mdObj, n, ...))
}

#' @export
#' @rdname PriorDraw
PriorDraw.mvnormal.V <- function(mdObj, n = 1, ...) {
  return(PriorDraw.mvnormal(mdObj, n, ...))
}

#' @export
#' @rdname PriorDraw
PriorDraw.mvnormal.EII <- function(mdObj, n = 1, ...) {
  return(PriorDraw.mvnormal(mdObj, n, ...))
}

#' @export
#' @rdname PriorDraw
PriorDraw.mvnormal.VII <- function(mdObj, n = 1, ...) {
  return(PriorDraw.mvnormal(mdObj, n, ...))
}

#' @export
#' @rdname PriorDraw
PriorDraw.mvnormal.EEI <- function(mdObj, n = 1, ...) {
  return(PriorDraw.mvnormal(mdObj, n, ...))
}

#' @export
#' @rdname PriorDraw
PriorDraw.mvnormal.VEI <- function(mdObj, n = 1, ...) {
  return(PriorDraw.mvnormal(mdObj, n, ...))
}

#' @export
#' @rdname PriorDraw
PriorDraw.mvnormal.EVI <- function(mdObj, n = 1, ...) {
  return(PriorDraw.mvnormal(mdObj, n, ...))
}

#' @export
#' @rdname PriorDraw
PriorDraw.mvnormal.VVI <- function(mdObj, n = 1, ...) {
  return(PriorDraw.mvnormal(mdObj, n, ...))
}
