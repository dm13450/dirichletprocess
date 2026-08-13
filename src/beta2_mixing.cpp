#include "beta2_mixing.h"
#include <Rcpp.h>
#include <cmath>
#include <algorithm>

namespace dirichletprocess {

// Constructor
Beta2Mixing::Beta2Mixing(double gamma_prior, double maxT,
                         const arma::vec& mh_step_size, int mh_draws)
  : gamma_prior(gamma_prior), maxT(maxT), mh_step_size(mh_step_size), mh_draws(mh_draws) {

  if (this->mh_step_size.n_elem != 2) {
    this->mh_step_size.resize(2);
    this->mh_step_size.fill(1.0);
  }
}

// Helper function for Pareto distribution
double Beta2Mixing::rpareto(double xm, double alpha) const {
  double u = R::runif(0, 1);
  return xm / std::pow(1 - u, 1.0 / alpha);
}

double Beta2Mixing::dpareto(double x, double xm, double alpha) const {
  if (x < xm) return 0.0;
  return alpha * std::pow(xm, alpha) / std::pow(x, alpha + 1);
}

// Log likelihood implementation
double Beta2Mixing::log_likelihood(const arma::vec& data_point,
                                   const arma::vec& params) const {
  double mu = params[0];
  double nu = params[1];

  // Convert to standard Beta parameters
  double a = (mu / maxT) * nu;
  double b = (1.0 - mu / maxT) * nu;

  double x = data_point[0];

  // Check bounds
  if (x < 0 || x > maxT || a <= 0 || b <= 0 || nu <= 0) {
    return -std::numeric_limits<double>::infinity();
  }

  // Beta likelihood (same as regular Beta)
  return R::dbeta(x / maxT, a, b, 1) - std::log(maxT);
}

// Prior draw
arma::vec Beta2Mixing::prior_draw() const {
  arma::vec params(2);

  // mu ~ Uniform(0, maxT)
  params[0] = R::runif(0, maxT);

  // Handle NA values
  if (std::isnan(params[0])) {
    params[0] = maxT / 2.0;
  }

  // Calculate mu limit for Pareto distribution
  double mu_lim = std::max(1.0 / (params[0] / maxT),
                           1.0 / (1.0 - params[0] / maxT));

  // Handle potential NA or infinite values
  if (std::isnan(mu_lim) || std::isinf(mu_lim)) {
    mu_lim = 10.0;
  }

  // nu ~ Pareto(mu_lim, gamma_prior)
  params[1] = rpareto(mu_lim, gamma_prior);

  // Handle NA values
  if (std::isnan(params[1])) {
    params[1] = 1.0;
  }

  // Ensure nu doesn't have zero values
  if (params[1] == 0) {
    params[1] = 1e-4;
  }

  return params;
}

// Log prior density
double Beta2Mixing::log_prior_density(const arma::vec& params) const {
  double mu = params[0];
  double nu = params[1];

  // mu density: Uniform(0, maxT)
  double log_mu_density = -std::log(maxT);

  // Calculate mu limit
  double mu_lim = std::max(1.0 / (mu / maxT),
                           1.0 / (1.0 - mu / maxT));

  // nu density: Pareto(mu_lim, gamma_prior)
  double log_nu_density = std::log(dpareto(nu, mu_lim, gamma_prior));

  return log_mu_density + log_nu_density;
}

// MH parameter proposal
arma::vec Beta2Mixing::mh_parameter_proposal(const arma::vec& current_params) const {
  arma::vec new_params = current_params;

  // Propose new mu
  double new_mu = current_params[0] + mh_step_size[0] * R::rnorm(0, 2.4);

  if (new_mu > maxT || new_mu < 0) {
    new_mu = current_params[0];
  }

  // Handle NA values
  if (std::isnan(new_mu)) {
    new_mu = current_params[0];
  }

  new_params[0] = new_mu;

  // Propose new nu (ensure positive)
  double new_nu = std::abs(current_params[1] + mh_step_size[1] * R::rnorm(0, 2.4));

  // Handle NA values and ensure minimum values
  if (std::isnan(new_nu) || new_nu == 0) {
    new_nu = 1e-4;
  }

  new_params[1] = new_nu;

  return new_params;
}

// Posterior draw using Metropolis-Hastings
arma::vec Beta2Mixing::posterior_draw(const arma::mat& cluster_data,
                                      const arma::vec& prior_params) const {
  // Handle empty cluster case
  if (cluster_data.n_rows == 0) {
    return prior_draw();
  }

  arma::vec current_params;
  if (prior_params.n_elem == 2 && prior_params.is_finite()) {
    current_params = prior_params;
  } else {
    current_params = prior_draw();
  }

  // Run MH algorithm
  double current_log_prior = log_prior_density(current_params);
  double current_log_lik = 0.0;

  for (arma::uword i = 0; i < cluster_data.n_rows; ++i) {
    current_log_lik += log_likelihood(cluster_data.row(i).t(), current_params);
  }

  // Match the repaired R MH semantics: the incoming state is sample 1 and
  // only the remaining mh_draws - 1 steps generate proposals.
  int n_proposals = std::max(0, mh_draws - 1);
  for (int iter = 0; iter < n_proposals; ++iter) {
    // Propose new parameters
    arma::vec proposed_params = mh_parameter_proposal(current_params);

    // Calculate proposed log prior and likelihood
    double proposed_log_prior = log_prior_density(proposed_params);
    double proposed_log_lik = 0.0;

    for (arma::uword i = 0; i < cluster_data.n_rows; ++i) {
      proposed_log_lik += log_likelihood(cluster_data.row(i).t(), proposed_params);
    }

    // Calculate acceptance ratio
    double log_ratio = (proposed_log_prior + proposed_log_lik) -
      (current_log_prior + current_log_lik);
    double accept_prob = 0.0;
    if (std::isnan(log_ratio)) {
      accept_prob = 0.0;
    } else if (std::isinf(log_ratio) && log_ratio > 0) {
      accept_prob = 1.0;
    } else {
      accept_prob = std::min(1.0, std::exp(log_ratio));
      if (std::isnan(accept_prob) || !std::isfinite(accept_prob)) {
        accept_prob = 0.0;
      }
    }

    // Accept or reject
    if (R::runif(0, 1) < accept_prob) {
      current_params = proposed_params;
      current_log_prior = proposed_log_prior;
      current_log_lik = proposed_log_lik;
    }
  }

  return current_params;
}

} // namespace dirichletprocess
