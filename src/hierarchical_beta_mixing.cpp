#include "hierarchical_beta_mixing.h"
#include <Rcpp.h>
#include <cmath>
#include <algorithm>

namespace dirichletprocess {

HierarchicalBetaMixing::HierarchicalBetaMixing(double alpha0, double beta0,
                                               double maxT,
                                               double gamma_prior_shape,
                                               double gamma_prior_rate,
                                               int m_auxiliary)
  : alpha0(alpha0), beta0(beta0), maxT(maxT),
    gamma_prior_shape(gamma_prior_shape),
    gamma_prior_rate(gamma_prior_rate),
    m_auxiliary(m_auxiliary) {

  // Initialize gamma from prior
  gamma = R::rgamma(gamma_prior_shape, 1.0 / gamma_prior_rate);

  // Initialize with small number of global clusters
  int initial_clusters = 5;
  global_stick_weights = arma::zeros(initial_clusters);

  // Stick-breaking construction for initial weights
  double remaining = 1.0;
  for (int i = 0; i < initial_clusters - 1; i++) {
    double v = R::rbeta(1.0, gamma);
    global_stick_weights[i] = v * remaining;
    remaining *= (1.0 - v);
  }
  global_stick_weights[initial_clusters - 1] = remaining;

  // Draw initial global parameters
  global_params.resize(initial_clusters);
  for (int i = 0; i < initial_clusters; i++) {
    global_params[i] = prior_draw();
  }
}

double HierarchicalBetaMixing::log_likelihood(const arma::vec& data_point,
                                              const arma::vec& params) const {
  double x = data_point[0];
  double mu = params[0];
  double tau = params[1];

  // Convert to standard Beta parameters
  double a = (mu * tau) / maxT;
  double b = ((maxT - mu) * tau) / maxT;

  // Check bounds
  if (x < 0 || x > maxT || a <= 0 || b <= 0 || mu < 0 || mu > maxT || tau <= 0) {
    return -std::numeric_limits<double>::infinity();
  }

  // Scaled Beta likelihood
  double x_scaled = x / maxT;
  return -std::log(maxT) + R::dbeta(x_scaled, a, b, 1);
}

arma::vec HierarchicalBetaMixing::posterior_draw(const arma::mat& cluster_data,
                                                 const arma::vec& prior_params) const {
  if (cluster_data.n_rows == 0) {
    return draw_from_g0();
  }

  // For hierarchical model, we use Metropolis-Hastings within Gibbs
  // This is based on Escobar & West (1995) approach

  arma::vec x = cluster_data.col(0);
  int n = x.n_elem;

  // Current parameters (use prior_params as starting point if provided)
  arma::vec current_params = prior_params;
  if (current_params.n_elem != 2) {
    current_params = arma::vec(2);
    current_params[0] = arma::mean(x);  // mu initialized to data mean
    current_params[1] = 10.0;           // tau initialized to moderate precision
  }

  // Metropolis-Hastings sampling
  int mh_steps = 10;  // Number of MH steps
  double step_size_mu = 0.1 * maxT;
  double step_size_tau = 0.1;

  for (int step = 0; step < mh_steps; step++) {
    // Propose new parameters
    arma::vec proposed = current_params;
    proposed[0] += R::rnorm(0, step_size_mu);
    proposed[1] *= std::exp(R::rnorm(0, step_size_tau));  // Log-normal proposal for tau

    // Ensure bounds
    proposed[0] = std::max(0.01, std::min(maxT - 0.01, proposed[0]));
    proposed[1] = std::max(0.1, proposed[1]);

    // Calculate acceptance ratio
    double log_ratio = 0.0;

    // Likelihood ratio
    for (int i = 0; i < n; i++) {
      arma::vec xi = x.row(i).t();
      log_ratio += log_likelihood(xi, proposed) - log_likelihood(xi, current_params);
    }

    // Prior ratio (using G0)
    double log_prior_ratio = 0.0;
    for (size_t k = 0; k < global_params.size(); k++) {
      double log_g0_proposed = -0.5 * arma::sum(arma::square(proposed - global_params[k]));
      double log_g0_current = -0.5 * arma::sum(arma::square(current_params - global_params[k]));
      log_prior_ratio += global_stick_weights[k] * (log_g0_proposed - log_g0_current);
    }
    log_ratio += log_prior_ratio;

    // Accept/reject
    if (std::log(R::runif(0, 1)) < log_ratio) {
      current_params = proposed;
    }
  }

  return current_params;
}

arma::vec HierarchicalBetaMixing::prior_draw() const {
  arma::vec params(2);

  // mu ~ Uniform(0, maxT)
  params[0] = R::runif(0, maxT);

  // tau ~ InverseGamma(alpha0, beta0)
  double gamma_draw = R::rgamma(alpha0, 1.0 / beta0);
  params[1] = 1.0 / std::max(1e-10, gamma_draw);

  return params;
}

arma::vec HierarchicalBetaMixing::draw_from_g0() const {
  // Sample from G0 using stick-breaking representation
  double u = R::runif(0, 1);
  double cumsum = 0.0;

  for (size_t k = 0; k < global_params.size(); k++) {
    cumsum += global_stick_weights[k];
    if (u <= cumsum) {
      // Add small noise for continuous G0
      arma::vec params = global_params[k];
      params[0] += R::rnorm(0, 0.01 * maxT);
      params[1] *= std::exp(R::rnorm(0, 0.01));

      // Ensure bounds
      params[0] = std::max(0.01, std::min(maxT - 0.01, params[0]));
      params[1] = std::max(0.1, params[1]);

      return params;
    }
  }

  // Fallback: return last global parameter
  return global_params.back();
}

void HierarchicalBetaMixing::update_global_parameters(
    const std::vector<arma::mat>& all_cluster_data,
    const std::vector<arma::vec>& all_cluster_params) {

  // Following Teh et al. (2006) for hierarchical DP
  // Update each global parameter using all data assigned to it

  for (size_t k = 0; k < global_params.size(); k++) {
    // Collect all data assigned to global cluster k
    arma::mat pooled_data;
    int n_assigned = 0;

    for (size_t j = 0; j < all_cluster_data.size(); j++) {
      // Check if cluster j is assigned to global cluster k
      // This requires tracking assignments (simplified here)
      if (n_assigned == 0) {
        pooled_data = all_cluster_data[j];
      } else {
        pooled_data = arma::join_cols(pooled_data, all_cluster_data[j]);
      }
      n_assigned++;
    }

    if (n_assigned > 0) {
      // Update global parameter k using pooled data
      global_params[k] = posterior_draw(pooled_data, global_params[k]);
    }
  }
}

void HierarchicalBetaMixing::update_global_stick_weights(int n_global_clusters) {
  // Update stick-breaking weights using Beta distribution
  // Based on Section 4.1 of Teh et al. (2006)

  if (static_cast<arma::uword>(n_global_clusters) != global_stick_weights.n_elem) {
    global_stick_weights.resize(n_global_clusters);
    global_params.resize(n_global_clusters);
  }

  // Recompute stick-breaking weights
  double remaining = 1.0;
  for (int k = 0; k < n_global_clusters - 1; k++) {
    double v = R::rbeta(1.0, gamma);
    global_stick_weights[k] = v * remaining;
    remaining *= (1.0 - v);
  }
  global_stick_weights[n_global_clusters - 1] = remaining;
}

void HierarchicalBetaMixing::update_gamma(int n_unique_clusters, int n_total_obs) {
  // Update gamma using auxiliary variable method from Escobar & West (1995)

  // Sample auxiliary variable
  double eta = R::rbeta(gamma + 1.0, n_total_obs);

  // Calculate mixing probabilities
  double pi_eta = (gamma_prior_shape + n_unique_clusters - 1.0) /
    (gamma_prior_shape + n_unique_clusters - 1.0 +
    n_total_obs * (gamma_prior_rate - std::log(eta)));

  // Sample gamma from mixture of Gammas
  if (R::runif(0, 1) < pi_eta) {
    gamma = R::rgamma(gamma_prior_shape + n_unique_clusters,
                      1.0 / (gamma_prior_rate - std::log(eta)));
  } else {
    gamma = R::rgamma(gamma_prior_shape + n_unique_clusters - 1.0,
                      1.0 / (gamma_prior_rate - std::log(eta)));
  }
}

} // namespace dirichletprocess
