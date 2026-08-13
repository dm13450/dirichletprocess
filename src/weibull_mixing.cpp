#include "weibull_mixing.h"
#include <Rcpp.h>
#include <cmath>
#include <algorithm>
#include <limits>

namespace dirichletprocess {

namespace {

bool valid_weibull_start(const arma::vec& params, double phi) {
  return params.n_elem >= 2 &&
    std::isfinite(params[0]) &&
    std::isfinite(params[1]) &&
    params[0] > 0.0 &&
    params[0] <= phi &&
    params[1] > 0.0;
}

double gibbs_lambda_draw(const arma::mat& cluster_data, double alpha,
                         double alpha0, double beta0) {
  double sum_x_alpha = 0.0;
  for (arma::uword i = 0; i < cluster_data.n_rows; ++i) {
    double x = cluster_data(i, 0);
    if (x > 0.0) {
      double term = std::pow(x, alpha);
      if (!std::isfinite(term)) {
        return std::numeric_limits<double>::infinity();
      }
      sum_x_alpha += term;
    }
  }

  double gamma_draw = R::rgamma(cluster_data.n_rows + alpha0,
                                1.0 / (sum_x_alpha + beta0));
  return 1.0 / gamma_draw;
}

double weibull_log_likelihood_sum(const arma::mat& cluster_data,
                                  double alpha, double lambda) {
  if (!std::isfinite(alpha) || !std::isfinite(lambda) || alpha <= 0.0 || lambda <= 0.0) {
    return -std::numeric_limits<double>::infinity();
  }

  double log_lik = 0.0;
  for (arma::uword i = 0; i < cluster_data.n_rows; ++i) {
    double x = cluster_data(i, 0);
    if (x < 0.0) {
      return -std::numeric_limits<double>::infinity();
    }
    if (x == 0.0) {
      if (alpha < 1.0) {
        return std::numeric_limits<double>::infinity();
      }
      if (alpha == 1.0) {
        log_lik += std::log(alpha) - std::log(lambda);
        continue;
      }
      return -std::numeric_limits<double>::infinity();
    } else {
      double term = std::pow(x, alpha);
      if (!std::isfinite(term)) {
        return -std::numeric_limits<double>::infinity();
      }
      double value = -std::log(lambda) + std::log(alpha) +
        (alpha - 1.0) * std::log(x) - term / lambda;
      if (!std::isfinite(value)) {
        return -std::numeric_limits<double>::infinity();
      }
      log_lik += value;
    }
  }

  return log_lik;
}

} // namespace

// Constructor
WeibullMixing::WeibullMixing(double phi, double alpha0, double beta0,
                             double hyper_a1, double hyper_a2,
                             double hyper_b1, double hyper_b2,
                             double mh_step_alpha, int mh_draws)
  : phi(phi), alpha0(alpha0), beta0(beta0),
    hyper_a1(hyper_a1), hyper_a2(hyper_a2),
    hyper_b1(hyper_b1), hyper_b2(hyper_b2),
    mh_step_alpha(mh_step_alpha), mh_draws(mh_draws) {}

// Optimized log likelihood implementation
double WeibullMixing::log_likelihood(const arma::vec& data_point,
                                     const arma::vec& params) const {
  double x = data_point[0];
  double alpha = params[0];
  double lambda = params[1];

  // Check bounds
  if (x < 0 || alpha <= 0 || lambda <= 0) {
    return -std::numeric_limits<double>::infinity();
  }

  if (x == 0.0) {
    if (alpha < 1.0) {
      return std::numeric_limits<double>::infinity();
    }
    if (alpha == 1.0) {
      return std::log(alpha) - std::log(lambda);
    }
    return -std::numeric_limits<double>::infinity();
  }

  // Optimized Weibull log-likelihood
  // Use log(x) to avoid expensive pow() call
  double log_x = std::log(x);
  double log_lik = -std::log(lambda) + std::log(alpha) +
    (alpha - 1.0) * log_x -
    std::exp(alpha * log_x) / lambda;

  return log_lik;
}

// Prior draw
arma::vec WeibullMixing::prior_draw() const {
  arma::vec params(2);

  // alpha ~ Uniform(0, phi)
  params[0] = R::runif(0, phi);

  // lambda = 1/Gamma(alpha0, beta0)
  double gamma_draw = R::rgamma(alpha0, 1.0 / beta0);
  params[1] = 1.0 / gamma_draw;

  return params;
}

// Optimized posterior draw using Metropolis-Hastings
arma::vec WeibullMixing::posterior_draw(const arma::mat& cluster_data,
                                        const arma::vec& prior_params) const {
  if (cluster_data.n_rows == 0) {
    return prior_draw();
  }

  arma::vec current_params = valid_weibull_start(prior_params, phi) ? prior_params : prior_draw();
  current_params[1] = gibbs_lambda_draw(cluster_data, current_params[0], alpha0, beta0);

  int draws = std::max(1, mh_draws);
  for (int iter = 1; iter < draws; ++iter) {
    current_params[1] = gibbs_lambda_draw(cluster_data, current_params[0], alpha0, beta0);

    double current_log_prior = log_prior_density(current_params);
    double current_log_lik = weibull_log_likelihood_sum(cluster_data,
                                                        current_params[0],
                                                        current_params[1]);

    arma::vec proposed_params = current_params;
    proposed_params[0] = current_params[0] + mh_step_alpha * R::rnorm(0.0, 1.7);

    double proposed_log_prior = log_prior_density(proposed_params);
    double proposed_log_lik = weibull_log_likelihood_sum(cluster_data,
                                                         proposed_params[0],
                                                         proposed_params[1]);

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
      current_params = proposed_params;
    }
  }

  return current_params;
}

// Log prior density
double WeibullMixing::log_prior_density(const arma::vec& params) const {
  double alpha = params[0];
  double lambda = params[1];

  // Check bounds
  if (alpha <= 0 || alpha > phi || lambda <= 0) {
    return -std::numeric_limits<double>::infinity();
  }

  // Log prior for alpha: log(1/phi) = -log(phi)
  double log_prior_alpha = -std::log(phi);

  // Log prior for lambda: Inverse-Gamma(alpha0, beta0)
  double log_prior_lambda = alpha0 * std::log(beta0) - std::lgamma(alpha0) -
    (alpha0 + 1.0) * std::log(lambda) - beta0 / lambda;

  return log_prior_alpha + log_prior_lambda;
}

// MH parameter proposal
arma::vec WeibullMixing::mh_parameter_proposal(const arma::vec& current_params) const {
  arma::vec proposed = current_params;

  // Propose new alpha
  double alpha_current = current_params[0];
  double alpha_proposal = std::abs(alpha_current + mh_step_alpha * R::rnorm(0, 1.7));

  // Ensure alpha stays within bounds
  if (alpha_proposal > phi) {
    alpha_proposal = phi;
  }

  proposed[0] = alpha_proposal;
  // Lambda will be updated analytically

  return proposed;
}

// Update hyperparameters
void WeibullMixing::update_hyperparameters(const std::vector<arma::vec>& all_params) {
  int K = all_params.size();
  if (K == 0) return;

  // Find maximum alpha and sum of inverse lambdas
  double max_alpha = 0.0;
  double sum_inv_lambda = 0.0;

  for (const auto& params : all_params) {
    max_alpha = std::max(max_alpha, params[0]);
    if (params[1] > 1e-10) {
      sum_inv_lambda += 1.0 / params[1];
    }
  }

  // Update phi using Pareto posterior
  double xm = std::max(max_alpha, hyper_a1);
  double shape = hyper_a2 + K;
  double U = R::runif(0, 1);
  phi = qpareto(U, xm, shape);

  // Update beta0 using Gamma posterior
  double post_shape = hyper_b1 + alpha0 * K;
  double post_rate = hyper_b2 + sum_inv_lambda;
  beta0 = R::rgamma(post_shape, 1.0 / post_rate);
}

// Pareto quantile function
double WeibullMixing::qpareto(double p, double xm, double alpha) const {
  if (p <= 0 || p >= 1) {
    Rcpp::stop("p must be in (0,1) for qpareto");
  }
  return xm * std::pow(1 - p, -1.0 / alpha);
}

} // namespace dirichletprocess
