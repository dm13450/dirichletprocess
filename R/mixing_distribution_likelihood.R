#' Mixing Distribution Likelihood
#'
#' Evaluate the Likelihood of some data \eqn{x} for some parameter \eqn{\theta}.
#'
#' @param mdObj Mixing Distribution
#' @param x Data
#' @param theta Parameters of distribution
#' @return Likelihood of the data
#' @export
Likelihood <- function(mdObj, x, theta) {
  # For MVNormal2, always use S3 dispatch (has its own C++ integration)
  if (any(class(mdObj) == "mvnormal2")) {
    return(UseMethod("Likelihood", mdObj))
  }
  
  if (using_cpp()) {
    # Get the distribution type
    dist_type <- class(mdObj)[class(mdObj) != "list" & class(mdObj) != "MixingDistribution"][1]

    tryCatch({
      # Convert x to appropriate format based on distribution type
      if (dist_type == "mvnormal" || any(grepl("mvnormal", class(mdObj)))) {
        # For multivariate data, keep as matrix
        if (!is.matrix(x)) {
          x <- matrix(x, nrow = 1)
        }
      } else {
        # For univariate data, convert to numeric vector
        x <- as.numeric(x)
      }

      # Dispatch to the appropriate C++ function
      if (dist_type == "weibull") {
        # Extract parameter arrays
        alpha_array <- theta[[1]]
        lambda_array <- theta[[2]]

        # Get the number of clusters from the third dimension
        num_clusters <- dim(alpha_array)[3]
        if (is.null(num_clusters)) num_clusters <- 1

        # If there's only one cluster, call the C++ function once
        if (num_clusters == 1) {
          alpha <- as.numeric(alpha_array[1, 1, 1])
          lambda <- as.numeric(lambda_array[1, 1, 1])
          return(weibull_likelihood_cpp(x, alpha, lambda))
        } else {
          # Multiple clusters: return likelihood for each cluster
          result <- numeric(num_clusters)
          for (k in 1:num_clusters) {
            alpha <- as.numeric(alpha_array[1, 1, k])
            lambda <- as.numeric(lambda_array[1, 1, k])
            # For each cluster, calculate likelihood for all data points,
            # then take the first value (for single data point case)
            lik_values <- weibull_likelihood_cpp(x, alpha, lambda)
            result[k] <- lik_values[1]
          }
          return(result)
        }
      } else if (dist_type == "normal" || dist_type == "gaussian") {
        # Handle both scalar and array parameters for normal distribution
        mu_array <- theta[[1]]
        sigma_array <- theta[[2]]

        # Check if parameters are arrays or scalars
        if (is.array(mu_array) && length(dim(mu_array)) == 3) {
          # 3D array case - multiple clusters
          num_clusters <- dim(mu_array)[3]
          if (num_clusters == 1) {
            mu <- as.numeric(mu_array[1, 1, 1])
            sigma <- as.numeric(sigma_array[1, 1, 1])
            return(normal_likelihood_cpp(x, mu, sigma))
          } else {
            result <- numeric(num_clusters)
            for (k in 1:num_clusters) {
              mu <- as.numeric(mu_array[1, 1, k])
              sigma <- as.numeric(sigma_array[1, 1, k])
              lik_values <- normal_likelihood_cpp(x, mu, sigma)
              result[k] <- lik_values[1]
            }
            return(result)
          }
        } else {
          # Scalar or simple vector case
          mu <- as.numeric(mu_array)
          sigma <- as.numeric(sigma_array)
          return(normal_likelihood_cpp(x, mu, sigma))
        }
      } else if (dist_type == "exponential") {
        # Handle exponential distribution
        lambda_array <- theta[[1]]

        num_clusters <- dim(lambda_array)[3]
        if (is.null(num_clusters)) num_clusters <- 1

        if (num_clusters == 1) {
          lambda <- as.numeric(lambda_array[1, 1, 1])
          return(exponential_likelihood_cpp(x, lambda))
        } else {
          result <- numeric(num_clusters)
          for (k in 1:num_clusters) {
            lambda <- as.numeric(lambda_array[1, 1, k])
            lik_values <- exponential_likelihood_cpp(x, lambda)
            result[k] <- lik_values[1]
          }
          return(result)
        }
      } else if (dist_type == "beta") {
        # Handle beta distribution
        alpha_array <- theta[[1]]
        beta_array <- theta[[2]]
        maxT <- ifelse(is.null(mdObj$maxT), 1, mdObj$maxT)

        num_clusters <- dim(alpha_array)[3]
        if (is.null(num_clusters)) num_clusters <- 1

        if (num_clusters == 1) {
          alpha <- as.numeric(alpha_array[1, 1, 1])
          beta <- as.numeric(beta_array[1, 1, 1])
          return(beta_likelihood_cpp(x, alpha, beta, maxT))
        } else {
          result <- numeric(num_clusters)
          for (k in 1:num_clusters) {
            alpha <- as.numeric(alpha_array[1, 1, k])
            beta <- as.numeric(beta_array[1, 1, k])
            lik_values <- beta_likelihood_cpp(x, alpha, beta, maxT)
            result[k] <- lik_values[1]
          }
          return(result)
        }
      } else if (dist_type == "mvnormal" || any(grepl("mvnormal", class(mdObj)) & !grepl("mvnormal2", class(mdObj)))) {
        # Handle mvnormal distributions only (mvnormal2 has its own S3 method)
        # Handle mvnormal distribution
          mu_array <- theta$mu
          sig_array <- theta$sig
          
          # Get dimensions
          mu_dim <- dim(mu_array)
          sig_dim <- dim(sig_array)
          
          # Extract number of clusters
          if (is.null(mu_dim) || length(mu_dim) < 3) {
            num_clusters <- 1
          } else {
            num_clusters <- mu_dim[3]
          }
          
          if (num_clusters == 1) {
            # Single cluster case
            cluster_theta <- list(mu = as.vector(mu_array), sig = sig_array)
            return(mvnormal_likelihood_wrapper_cpp(x, cluster_theta, mdObj$priorParameters))
          } else {
            # Multi-cluster case
            result <- numeric(num_clusters)
            d <- length(x)
            
            for (k in 1:num_clusters) {
              # Extract parameters for cluster k
              if (length(mu_dim) == 2) {
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
              } else {
                # For 3D mu_array case, similar logic might be needed
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
              }
              
              cluster_theta <- list(mu = cluster_mu, sig = cluster_sig)
              result[k] <- mvnormal_likelihood_wrapper_cpp(x, cluster_theta, mdObj$priorParameters)
            }
            return(result)
          }
      } else {
        # For other distributions, fall back to R
        stop("C++ implementation not available for distribution: ", 
             class(mdObj)[class(mdObj) != "list" & class(mdObj) != "MixingDistribution"][1])
      }
    }, error = function(e) {
      # Silently fall back to R implementation for other distributions
      # MVNormal2 now has its own S3 method with proper C++ integration
    })
  }

  # Handle multi-cluster case for specific distributions before falling back
  dist_type <- class(mdObj)[class(mdObj) != "list" & class(mdObj) != "MixingDistribution"][1]
  
  if (dist_type == "normal" || dist_type == "gaussian") {
    # Handle normal distribution with multiple clusters
    if (is.list(theta) && length(theta) >= 2) {
      mu_params <- theta[[1]]
      sigma_params <- theta[[2]]
      
      # Determine number of clusters from parameter structure
      if (is.array(mu_params) && length(dim(mu_params)) == 3) {
        num_clusters <- dim(mu_params)[3]
      } else if (is.matrix(mu_params)) {
        num_clusters <- ncol(mu_params)
      } else if (is.vector(mu_params) && length(mu_params) > 1) {
        num_clusters <- length(mu_params)
      } else {
        num_clusters <- 1
      }
      
      if (num_clusters > 1) {
        result <- numeric(num_clusters)
        for (k in 1:num_clusters) {
          # Extract parameters for cluster k
          if (is.array(mu_params) && length(dim(mu_params)) == 3) {
            cluster_mu <- mu_params[1, 1, k]
            cluster_sigma <- sigma_params[1, 1, k]
          } else if (is.matrix(mu_params)) {
            cluster_mu <- mu_params[1, k]
            cluster_sigma <- sigma_params[1, k]
          } else {
            cluster_mu <- mu_params[k]
            cluster_sigma <- sigma_params[k]
          }
          
          # Call Likelihood.normal with proper format
          cluster_theta <- list(cluster_mu, cluster_sigma)
          result[k] <- Likelihood.normal(mdObj, x, cluster_theta)
        }
        return(result)
      }
    }
  }
  
  # Original implementation (falls back to this if C++ is not enabled or fails)
  UseMethod("Likelihood", mdObj)
}
