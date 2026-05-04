#' C++ Normal Fixed Variance Prior Draw
#' @keywords internal
cpp_normal_fixed_variance_prior_draw <- function(mu0, sigma0, sigma, n) {
  .Call("_dirichletprocess_cpp_normal_fixed_variance_prior_draw", mu0, sigma0, sigma, n)
}

#' C++ Normal Fixed Variance Posterior Draw
#' @keywords internal
cpp_normal_fixed_variance_posterior_draw <- function(data, mu0, sigma0, sigma, n) {
  .Call("_dirichletprocess_cpp_normal_fixed_variance_posterior_draw",
        as.matrix(data), mu0, sigma0, sigma, n)
}

#' C++ Normal Fixed Variance Likelihood
#' @keywords internal
cpp_normal_fixed_variance_likelihood <- function(x, mu, sigma) {
  .Call("_dirichletprocess_cpp_normal_fixed_variance_likelihood", x, mu, sigma)
}

#' C++ Normal Fixed Variance Posterior Parameters
#' @keywords internal
cpp_normal_fixed_variance_posterior_parameters <- function(data, mu0, sigma0, sigma) {
  .Call("_dirichletprocess_cpp_normal_fixed_variance_posterior_parameters",
        as.matrix(data), mu0, sigma0, sigma)
}
