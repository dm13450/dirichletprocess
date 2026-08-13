// src/exponential_mixing.cpp
#include "exponential_mixing.h"
#include <RcppArmadillo.h>

namespace dirichletprocess {

double ExponentialMixing::log_likelihood(const arma::vec& data_point,
                                         const arma::vec& params) const {
  double x = data_point[0];
  double lambda = params[0];

  // Bounds checking
  if (x < 0.0) return -std::numeric_limits<double>::infinity();
  if (lambda <= 0.0) return -std::numeric_limits<double>::infinity();

  // Log exponential density: log(λ * exp(-λx)) = log(λ) - λx
  return std::log(lambda) - lambda * x;
}

arma::vec ExponentialMixing::posterior_draw(const arma::mat& cluster_data,
                                            const arma::vec& prior_params) const {
  // Extract data
  int n = cluster_data.n_rows;
  double sum_x = arma::sum(cluster_data.col(0));

  // Conjugate posterior: λ ~ Gamma(α + n, β + Σx)
  double alpha_post = alpha0 + n;
  double beta_post = beta0 + sum_x;

  // Sample from posterior Gamma distribution
  // Note: R::rgamma uses scale parameterization, so we use 1/rate
  arma::vec params(1);
  params[0] = R::rgamma(alpha_post, 1.0 / beta_post);

  return params;
}

arma::vec ExponentialMixing::prior_draw() const {
  // Sample from prior Gamma(α₀, β₀)
  arma::vec params(1);
  params[0] = R::rgamma(alpha0, 1.0 / beta0);
  return params;
}

arma::vec ExponentialMixing::posterior_parameters(const arma::mat& cluster_data) const {
  int n = cluster_data.n_rows;
  double sum_x = arma::sum(cluster_data.col(0));

  arma::vec post_params(2);
  post_params[0] = alpha0 + n;      // posterior shape
  post_params[1] = beta0 + sum_x;   // posterior rate

  return post_params;
}

double ExponentialMixing::predictive_probability(const arma::vec& data_point) const {
  double x = data_point[0];
  if (x < 0.0) return 0.0;

  // Exponential-Gamma predictive distribution (Lomax/Pareto Type II)
  // p(x|prior) = α₀ * β₀^α₀ / (β₀ + x)^(α₀ + 1)
  
  return alpha0 * std::pow(beta0, alpha0) / std::pow(beta0 + x, alpha0 + 1.0);
}

double ExponentialMixing::predictive_density(double x, const arma::mat& cluster_data) const {
  if (x < 0.0) return 0.0;

  // Get posterior parameters
  arma::vec post_params = posterior_parameters(cluster_data);
  double alpha_post = post_params[0];
  double beta_post = post_params[1];

  // Predictive distribution is Lomax (Pareto Type II)
  // p(x|data) = (α/(β+x))^(α+1) * β^α / B(α,1)
  // Simplified: p(x|data) = α * β^α / (β+x)^(α+1)

  return alpha_post * std::pow(beta_post, alpha_post) /
    std::pow(beta_post + x, alpha_post + 1.0);
}

} // namespace dirichletprocess
