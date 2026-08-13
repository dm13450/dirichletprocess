#' C++ Beta2 Prior Draw
#' @keywords internal
cpp_beta2_prior_draw <- function(gamma_prior, maxT, n) {
  .Call("_dirichletprocess_cpp_beta2_prior_draw", gamma_prior, maxT, n)
}

#' C++ Beta2 Posterior Draw
#' @keywords internal
cpp_beta2_posterior_draw <- function(data, gamma_prior, maxT, mh_step_size, n, mh_draws) {
  .Call("_dirichletprocess_cpp_beta2_posterior_draw",
        as.matrix(data), gamma_prior, maxT, mh_step_size, n, mh_draws)
}

#' C++ Beta2 Likelihood
#' @keywords internal
cpp_beta2_likelihood <- function(x, mu, nu, maxT) {
  .Call("_dirichletprocess_cpp_beta2_likelihood", x, mu, nu, maxT)
}
