#include "beta_mixing.h"
#include <Rcpp.h>
#include <cmath>
#include <algorithm>
#include <limits>

namespace dirichletprocess {

namespace {

double beta_log_prior_density(double mu, double nu, double alpha0, double beta0,
                              double maxT) {
  if (!std::isfinite(mu) || !std::isfinite(nu) || mu < 0.0 || mu > maxT || nu <= 0.0) {
    return -std::numeric_limits<double>::infinity();
  }

  double log_mu_density = -std::log(maxT);
  double inv_nu = 1.0 / nu;
  double log_nu_density = alpha0 * std::log(beta0) - std::lgamma(alpha0) +
    (alpha0 - 1.0) * std::log(inv_nu) - beta0 * inv_nu -
    2.0 * std::log(nu);

  return log_mu_density + log_nu_density;
}

double beta_log_likelihood_sum(const arma::mat& cluster_data, double mu, double tau,
                               double maxT) {
  if (!std::isfinite(mu) || !std::isfinite(tau) || tau <= 0.0) {
    return -std::numeric_limits<double>::infinity();
  }

  double a = (mu * tau) / maxT;
  double b = (1.0 - mu / maxT) * tau;

  if (!std::isfinite(a) || !std::isfinite(b) || a <= 0.0 || b <= 0.0) {
    return -std::numeric_limits<double>::infinity();
  }

  double log_lik = 0.0;
  for (arma::uword i = 0; i < cluster_data.n_rows; ++i) {
    double x = cluster_data(i, 0);
    if (x < 0.0 || x > maxT) {
      return -std::numeric_limits<double>::infinity();
    }

    double value = std::log(1.0 / maxT) + R::dbeta(x / maxT, a, b, true);
    if (!std::isfinite(value)) {
      return -std::numeric_limits<double>::infinity();
    }
    log_lik += value;
  }

  return log_lik;
}

bool valid_beta_start(const arma::vec& params, double maxT) {
  return params.n_elem >= 2 &&
    std::isfinite(params[0]) &&
    std::isfinite(params[1]) &&
    params[0] >= 0.0 &&
    params[0] <= maxT &&
    params[1] > 0.0;
}

} // namespace

// Constructor
BetaMixing::BetaMixing(double alpha0, double beta0, double maxT,
                       double mh_step_mu, double mh_step_nu, int mh_draws)
  : alpha0(alpha0), beta0(beta0), maxT(maxT),
    mh_step_mu(mh_step_mu), mh_step_nu(mh_step_nu), mh_draws(mh_draws) {}

// Log likelihood implementation
double BetaMixing::log_likelihood(const arma::vec& data_point,
                                  const arma::vec& params) const {
  double mu = params[0];
  double tau = params[1];

  // Convert to standard Beta parameters
  double a = (mu * tau) / maxT;
  double b = (1.0 - mu/maxT) * tau;

  double x = data_point[0];

  // Check bounds
  if (x < 0 || x > maxT || a <= 0 || b <= 0 || tau <= 0) {
    return -std::numeric_limits<double>::infinity();
  }

  // Log likelihood: log(1/maxT) + log(dbeta(x/maxT, a, b))
  return std::log(1.0/maxT) + R::dbeta(x/maxT, a, b, 1);
}

// Posterior draw - for non-conjugate, we typically use MH or return prior draw
arma::vec BetaMixing::posterior_draw(const arma::mat& cluster_data,
                                     const arma::vec& prior_params) const {
  // Handle empty cluster case
  if (cluster_data.n_rows == 0) {
    return prior_draw();
  }

  arma::vec current_params = valid_beta_start(prior_params, maxT) ? prior_params : prior_draw();
  double current_mu = current_params[0];
  double current_nu = current_params[1];

  double current_log_prior = beta_log_prior_density(current_mu, current_nu,
                                                    alpha0, beta0, maxT);
  double current_log_lik = beta_log_likelihood_sum(cluster_data, current_mu, current_nu, maxT);

  int draws = std::max(1, mh_draws);

  for (int iter = 1; iter < draws; ++iter) {
    double proposed_mu = current_mu + mh_step_mu * R::rnorm(0.0, 2.4);
    if (proposed_mu > maxT || proposed_mu < 0.0 || !std::isfinite(proposed_mu)) {
      proposed_mu = current_mu;
    }

    double proposed_nu = std::abs(current_nu + mh_step_nu * R::rnorm(0.0, 2.4));
    if (!std::isfinite(proposed_nu) || proposed_nu <= 0.0) {
      proposed_nu = current_nu;
    }

    double proposed_log_prior = beta_log_prior_density(proposed_mu, proposed_nu,
                                                       alpha0, beta0, maxT);
    double proposed_log_lik = beta_log_likelihood_sum(cluster_data, proposed_mu,
                                                      proposed_nu, maxT);

    double log_ratio = proposed_log_prior + proposed_log_lik -
      current_log_prior - current_log_lik;

    double accept_prob = std::numeric_limits<double>::quiet_NaN();
    if (std::isnan(log_ratio)) {
      accept_prob = std::numeric_limits<double>::quiet_NaN();
    } else if (log_ratio >= 0.0) {
      accept_prob = 1.0;
    } else if (std::isfinite(log_ratio)) {
      accept_prob = std::exp(log_ratio);
    } else {
      accept_prob = 0.0;
    }

    if (std::isnan(accept_prob)) {
      accept_prob = 0.0;
    }

    if (R::runif(0.0, 1.0) < accept_prob) {
      current_mu = proposed_mu;
      current_nu = proposed_nu;
      current_log_prior = proposed_log_prior;
      current_log_lik = proposed_log_lik;
    }
  }

  arma::vec params(2);
  params[0] = current_mu;
  params[1] = current_nu;
  return params;
}

// Prior draw
arma::vec BetaMixing::prior_draw() const {
  arma::vec params(2);

  // mu ~ Uniform(0, maxT)
  params[0] = R::runif(0, maxT);

  // tau ~ InverseGamma(alpha0, beta0), so 1/tau ~ Gamma(alpha0, beta0)
  double gamma_draw = R::rgamma(alpha0, 1.0/beta0);  // R uses scale, not rate
  params[1] = 1.0 / gamma_draw;                      // tau = 1/gamma

  return params;
}

} // namespace dirichletprocess
