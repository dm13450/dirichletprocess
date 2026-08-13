# Fix for weighted_function_generator subscript out of bounds error
# This overrides the internal function to handle single cluster cases properly

weighted_function_generator <- function(func, weights, params) {
  weights <- weights / sum(weights)
  
  weightedFunc <- function(y) {
    if (is.matrix(y) || is.data.frame(y)) {
      out <- numeric(nrow(y))
      y <- as.matrix(y)
    } else {
      out <- numeric(length(y))
    }
    
    cumWeight <- 0
    for (i in seq_along(weights)) {
      if (cumWeight > (1 - 1e-06)) {
        break
      }
      
      if (!is.null(names(params)) && all(c("mu", "sig") %in% names(params))) {
        # Handle named parameters (mu, sig) for Gaussian distributions
        mu_dim <- dim(params$mu)[3]
        sig_dim <- dim(params$sig)[3]
        
        # Use min to avoid subscript out of bounds
        cluster_idx <- min(i, mu_dim, sig_dim)
        
        cl_params <- list(
          mu = params$mu[, , cluster_idx, drop = FALSE],
          sig = params$sig[, , cluster_idx, drop = FALSE]
        )
      } else if (!is.null(names(params)) && all(c("mu", "nu") %in% names(params))) {
        # Handle named parameters (mu, nu) for Beta distributions
        mu_dim <- dim(params$mu)[3]
        nu_dim <- dim(params$nu)[3]
        
        # Use min to avoid subscript out of bounds
        cluster_idx <- min(i, mu_dim, nu_dim)
        
        cl_params <- list(
          mu = as.numeric(params$mu[, , cluster_idx]),
          nu = as.numeric(params$nu[, , cluster_idx])
        )
      } else {
        # Handle unnamed parameters or other named parameter combinations
        cl_params <- vector("list", length = length(params))
        for (j in seq_along(params)) {
          # Handle different parameter structures safely
          param_dims <- length(dim(params[[j]]))
          
          if (param_dims == 3) {
            # Standard 3D array structure - use cluster_idx
            param_dim <- dim(params[[j]])[3]
            cluster_idx <- min(i, param_dim)
            cl_params[[j]] <- params[[j]][, , cluster_idx, drop = FALSE]
          } else if (param_dims == 2) {
            # 2D matrix structure - use the entire parameter (cluster-specific data)
            cl_params[[j]] <- params[[j]]
          } else {
            # Other structures - use as-is
            cl_params[[j]] <- params[[j]]
          }
        }
        # Preserve parameter names if they exist
        if (!is.null(names(params))) {
          names(cl_params) <- names(params)
        }
      }
      
      out <- out + weights[i] * func(y, cl_params)
      cumWeight <- cumWeight + weights[i]
    }
    return(out)
  }
  
  return(weightedFunc)
}