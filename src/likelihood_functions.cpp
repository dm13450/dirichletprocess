#include <RcppArmadillo.h>
#include <cmath>

// [[Rcpp::export]]
Rcpp::NumericVector likelihood_normal_cpp(
    Rcpp::List mdObj,
    Rcpp::NumericVector x,
    Rcpp::List theta) {

  // Extract parameters from theta
  double mu = Rcpp::as<double>(theta[0]);
  double sigma = Rcpp::as<double>(theta[1]);

  int n = x.size();
  Rcpp::NumericVector result(n);

  // Constant part of the normal PDF
  double log_const = -0.5 * std::log(2.0 * M_PI) - std::log(sigma);

  // Calculate likelihood for each value in x
  for (int i = 0; i < n; i++) {
    double z = (x[i] - mu) / sigma;
    result[i] = std::exp(log_const - 0.5 * z * z);
  }

  return result;
}
