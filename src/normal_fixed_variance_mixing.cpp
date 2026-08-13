#include "normal_fixed_variance_mixing.h"
#include <Rcpp.h>
#include <cmath>

namespace dirichletprocess {

// Constructor
NormalFixedVarianceMixing::NormalFixedVarianceMixing(double mu0, double sigma0, double sigma)
  : mu0(mu0), sigma0(sigma0), sigma(sigma) {}

// Log likelihood implementation
double NormalFixedVarianceMixing::log_likelihood(const arma::vec& data_point,
                                                 const arma::vec& params) const {
  double mu = params[0];
  double x = data_point[0];

  return R::dnorm(x, mu, sigma, 1);  // 1 for log
}

// Prior draw
arma::vec NormalFixedVarianceMixing::prior_draw() const {
  arma::vec params(1);

  // Draw normal values and handle potential NAs
  params[0] = R::rnorm(mu0, sigma);

  // Handle NA values that can occur with extreme parameters
  if (std::isnan(params[0])) {
    params[0] = mu0;  // Default to prior mean
  }

  return params;
}

// Posterior parameters (conjugate case)
arma::vec NormalFixedVarianceMixing::posterior_parameters(const arma::mat& cluster_data) const {
  int n = cluster_data.n_rows;

  if (n == 0) {
    // Return prior parameters
    arma::vec params(2);
    params[0] = mu0;
    params[1] = sigma0;
    return params;
  }

  // ybar = arma::mean(cluster_data.col(0)); // Not needed, using sum directly

  // Posterior precision and mean
  double sigma_posterior_sq = 1.0 / (1.0 / (sigma0 * sigma0) + n / (sigma * sigma));
  double mu_posterior = sigma_posterior_sq * (mu0 / (sigma0 * sigma0) +
                                              arma::sum(cluster_data.col(0)) / (sigma * sigma));

  arma::vec params(2);
  params[0] = mu_posterior;
  // Match the repaired R implementation exactly: it returns the posterior
  // variance-like quantity and passes it directly to rnorm() as the scale.
  params[1] = sigma_posterior_sq;

  return params;
}

// Posterior draw (conjugate case)
arma::vec NormalFixedVarianceMixing::posterior_draw(const arma::mat& cluster_data,
                                                    const arma::vec& prior_params) const {
  arma::vec post_params = posterior_parameters(cluster_data);

  arma::vec params(1);
  params[0] = R::rnorm(post_params[0], post_params[1]);

  return params;
}

// Predictive density
double NormalFixedVarianceMixing::predictive_density(double x) const {
  // Predictive variance
  double pred_var = sigma0 * sigma0 + sigma * sigma;

  return R::dnorm(x, mu0, std::sqrt(pred_var), 0);
}

double NormalFixedVarianceMixing::predictive_probability(const arma::vec& data_point) const {
  arma::mat single_obs(1, 1);
  single_obs(0, 0) = data_point[0];
  arma::vec post_params = posterior_parameters(single_obs);

  return R::dnorm(data_point[0],
                  post_params[0],
                  sigma0 * sigma0 + sigma * sigma,
                  0);
}

} // namespace dirichletprocess
