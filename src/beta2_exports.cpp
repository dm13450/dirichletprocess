#include <RcppArmadillo.h>
#include "beta2_mixing.h"

using namespace dirichletprocess;

// [[Rcpp::export]]
Rcpp::NumericVector cpp_beta2_prior_draw(double gamma_prior, double maxT, int n) {
  Beta2Mixing beta2(gamma_prior, maxT);
  Rcpp::NumericVector result(n * 2);

  for (int i = 0; i < n; ++i) {
    arma::vec params = beta2.prior_draw();
    result[i] = params[0];          // mu
    result[i + n] = params[1];      // nu
  }

  return result;
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_beta2_posterior_draw(arma::mat data, double gamma_prior,
                                             double maxT, arma::vec mh_step_size,
                                             int n, int mh_draws) {
  Beta2Mixing beta2(gamma_prior, maxT, mh_step_size, mh_draws);
  Rcpp::NumericVector result(n * 2);

  for (int i = 0; i < n; ++i) {
    arma::vec params = beta2.posterior_draw(data, arma::vec());
    result[i] = params[0];          // mu
    result[i + n] = params[1];      // nu
  }

  return result;
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_beta2_likelihood(arma::vec x, double mu, double nu, double maxT) {
  Beta2Mixing beta2(2.0, maxT);
  arma::vec params(2);
  params[0] = mu;
  params[1] = nu;

  Rcpp::NumericVector result(x.n_elem);
  for (arma::uword i = 0; i < x.n_elem; ++i) {
    result[i] = std::exp(beta2.log_likelihood(x.row(i).t(), params));
  }

  return result;
}
