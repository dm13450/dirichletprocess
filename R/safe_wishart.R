# Safe rWishart wrapper with regularization
# This provides numerical stability for edge cases where rWishart fails
# Internal function - not exported

# Internal safe rWishart function for numerical stability
safe_rWishart <- function(n, nu, Lambda, max_attempts = 3, lambda_reg = 1e-6) {
  
  # Validate inputs
  if (!is.matrix(Lambda)) {
    Lambda <- as.matrix(Lambda)
  }
  
  # Check if Lambda is positive definite
  eigen_vals <- eigen(Lambda, only.values = TRUE)$values
  
  if (any(eigen_vals <= 0)) {
    # Regularize the matrix
    d <- nrow(Lambda)
    Lambda <- Lambda + lambda_reg * diag(d)
    warning("Scale matrix regularized for numerical stability")
  }
  
  # Ensure sufficient degrees of freedom
  if (nu <= nrow(Lambda) - 1) {
    nu <- nrow(Lambda) + 2
    warning("Degrees of freedom adjusted for numerical stability")
  }
  
  # Try to generate samples
  attempt <- 1
  while (attempt <= max_attempts) {
    tryCatch({
      result <- stats::rWishart(n, nu, Lambda)
      return(result)
    }, error = function(e) {
      if (attempt == max_attempts) {
        stop("Failed to generate Wishart samples after ", max_attempts, " attempts: ", e$message)
      }
      # Increase regularization and try again
      d <- nrow(Lambda)
      Lambda <<- Lambda + lambda_reg * attempt * diag(d)
      attempt <<- attempt + 1
    })
  }
}