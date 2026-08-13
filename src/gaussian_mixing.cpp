// src/gaussian_mixing.cpp
#include <RcppArmadillo.h>
#include "gaussian_mixing.h"

namespace dirichletprocess {

GaussianMixing::GaussianMixing(double mu0, double kappa0,
                               double alpha0, double beta0)
  : mu0(mu0), kappa0(kappa0), alpha0(alpha0), beta0(beta0) {}

double GaussianMixing::log_likelihood(const arma::vec& data_point,
                                      const arma::vec& params) const {
  if (params.n_elem < 2) {
    return -std::numeric_limits<double>::infinity();
  }

  double mean = params[0];
  double variance = params[1];

  // Ensure variance is positive
  if (variance <= 0) {
    return -std::numeric_limits<double>::infinity();
  }

  // Compute log likelihood with numerical stability
  double standardized = (data_point[0] - mean) / std::sqrt(variance);
  double log_lik = -0.5 * (std::log(2 * M_PI) + std::log(variance) + standardized * standardized);

  // Check for numerical issues
  if (!std::isfinite(log_lik)) {
    return -std::numeric_limits<double>::infinity();
  }

  return log_lik;
}

arma::vec GaussianMixing::posterior_draw(const arma::mat& cluster_data,
                                         const arma::vec& prior_params) const {
  int n = cluster_data.n_rows;
  if (n == 0) {
    return prior_draw();
  }

  double data_mean = arma::mean(cluster_data.col(0));
  double sumsq = arma::accu(arma::square(cluster_data.col(0) - data_mean));

  // Posterior parameters (Normal-Inverse-Gamma)
  double kappa_n = kappa0 + n;
  double mu_n = (kappa0 * mu0 + n * data_mean) / kappa_n;
  double alpha_n = alpha0 + n / 2.0;
  double beta_n = beta0 + 0.5 * sumsq +
    0.5 * kappa0 * n * std::pow(data_mean - mu0, 2) / kappa_n;

  // Sample variance from Inverse-Gamma
  double variance = 1.0 / R::rgamma(alpha_n, 1.0 / beta_n);

  // Sample mean from Normal
  double mean = R::rnorm(mu_n, std::sqrt(variance / kappa_n));

  arma::vec params(2);
  params[0] = mean;
  params[1] = variance;

  return params;
}

arma::vec GaussianMixing::prior_draw() const {
  double variance = 1.0 / R::rgamma(alpha0, 1.0 / beta0);
  double mean = R::rnorm(mu0, std::sqrt(variance / kappa0));

  arma::vec params(2);
  params[0] = mean;
  params[1] = variance;

  return params;
}

double GaussianMixing::predictive_probability(const arma::vec& data_point) const {
  // R implementation uses marginal likelihood ratio: p(x|prior) / p(prior)
  // This matches the formula in R/normal_inverse_gamma.R, Predictive.normal
  double x = data_point[0];
  
  // Calculate posterior parameters after observing this single data point
  // Using the same logic as in posteriorParameters method
  int n_x = 1;
  double ybar = x;

  // mu_n calculated but not needed for predictive probability
  // double mu_n = (kappa0 * mu0 + n_x * ybar) / (kappa0 + n_x);
  double kappa_n = kappa0 + n_x;
  double alpha_n = alpha0 + n_x / 2.0;
  // For single data point, ybar - ybar = 0, so first term vanishes
  double beta_n = beta0 +
    kappa0 * n_x * std::pow(ybar - mu0, 2) / (2.0 * (kappa0 + n_x));
  
  // Calculate the predictive probability using R's formula:
  // (Γ(α_n)/Γ(α_0)) * (β_0^α_0/β_n^α_n) * √(κ_0/κ_n)
  double predictive = (R::gammafn(alpha_n) / R::gammafn(alpha0)) *
                     (std::pow(beta0, alpha0) / std::pow(beta_n, alpha_n)) *
                     std::sqrt(kappa0 / kappa_n);
  
  return predictive;
}

} // namespace dirichletprocess
