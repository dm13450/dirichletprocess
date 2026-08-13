// src/mcmc_runner.cpp
#include "mcmc_runner.h"
#include "mixing_distribution_base.h"
#include "beta_mixing.h"
#include "beta2_mixing.h"
#include "exponential_mixing.h"
#include "gaussian_mixing.h"
#include "mvnormal_mixing.h"
#include "mvnormal2_mixing.h"
#include "normal_fixed_variance_mixing.h"
#include "weibull_mixing.h"
#include "utilities.h"
#include <set>
#include <algorithm>
#include <numeric>
#include <memory>

namespace dirichletprocess {

MCMCRunner::MCMCRunner(const arma::mat& data,
                       const Rcpp::List& mixing_dist_params,
                       const Rcpp::List& mcmc_params)
  : data(data) {

  // Extract MCMC parameters
  n_iter = Rcpp::as<int>(mcmc_params["n_iter"]);
  n_burn = Rcpp::as<int>(mcmc_params["n_burn"]);
  thin = Rcpp::as<int>(mcmc_params["thin"]);
  update_concentration_flag = Rcpp::as<bool>(mcmc_params["update_concentration"]);
  if (mcmc_params.containsElementNamed("store_history")) {
    store_history = Rcpp::as<bool>(mcmc_params["store_history"]);
  } else {
    store_history = true;
  }

  // Set m_auxiliary (default to 3 for Algorithm 8)
  if (mcmc_params.containsElementNamed("m_auxiliary")) {
    m_auxiliary = Rcpp::as<int>(mcmc_params["m_auxiliary"]);
  } else {
    m_auxiliary = 3;
  }

  use_initial_state = false;
  if (mcmc_params.containsElementNamed("initial_cluster_labels") &&
      mcmc_params.containsElementNamed("initial_cluster_params")) {
    initial_cluster_labels =
      Rcpp::as<std::vector<int>>(mcmc_params["initial_cluster_labels"]);

    Rcpp::List initial_params_list =
      Rcpp::as<Rcpp::List>(mcmc_params["initial_cluster_params"]);
    initial_cluster_params.reserve(initial_params_list.size());
    for (int i = 0; i < initial_params_list.size(); ++i) {
      initial_cluster_params.push_back(Rcpp::as<arma::vec>(initial_params_list[i]));
    }

    use_initial_state = !initial_cluster_labels.empty() &&
      !initial_cluster_params.empty();
  }

  // Extract alpha prior parameters
  if (mcmc_params.containsElementNamed("alpha_prior_shape")) {
    alpha_prior_shape = Rcpp::as<double>(mcmc_params["alpha_prior_shape"]);
  } else {
    alpha_prior_shape = 1.0;  // Default Gamma(1,1)
  }

  if (mcmc_params.containsElementNamed("alpha_prior_rate")) {
    alpha_prior_rate = Rcpp::as<double>(mcmc_params["alpha_prior_rate"]);
  } else {
    alpha_prior_rate = 1.0;  // Default Gamma(1,1)
  }

  // Validate inputs
  if (data.n_rows < 2) {
    Rcpp::warning("Data has fewer than 2 observations. Results may be unreliable.");
  }

  // Create mixing distribution
  std::string dist_type = Rcpp::as<std::string>(mixing_dist_params["type"]);
  mixing_dist = MixingDistribution::create(dist_type, mixing_dist_params);

  // Initialize state
  double initial_alpha = Rcpp::as<double>(mcmc_params["alpha"]);
  if (initial_alpha <= 0) {
    Rcpp::stop("alpha must be positive");
  }

  state = std::unique_ptr<DPState>(new DPState(data.n_rows, initial_alpha));

  // Pre-allocate storage only when the caller will actually consume it.
  if (store_history) {
    alpha_samples.reserve(n_iter);
    cluster_samples.reserve(n_iter);
    theta_samples.reserve(n_iter);
    likelihood_samples.reserve(n_iter);
  }
}

void MCMCRunner::initialize_state() {
  if (use_initial_state) {
    if (initial_cluster_labels.size() != data.n_rows) {
      Rcpp::stop("Initial cluster labels do not match the number of observations");
    }

    state->cluster_labels = initial_cluster_labels;
    state->cluster_params = initial_cluster_params;
    state->n_clusters = initial_cluster_params.size();
    state->cluster_sizes.set_size(state->n_clusters);
    state->cluster_sizes.zeros();

    for (int label : state->cluster_labels) {
      if (label < 0 || label >= state->n_clusters) {
        Rcpp::stop("Initial cluster labels reference missing cluster parameters");
      }
      state->cluster_sizes[label]++;
    }

    cleanup_empty_clusters();
    return;
  }

  // Initialize with one cluster containing all data
  std::fill(state->cluster_labels.begin(), state->cluster_labels.end(), 0);
  state->n_clusters = 1;
  state->cluster_sizes.set_size(1);
  state->cluster_sizes[0] = data.n_rows;

  // Initialize cluster parameters with validation
  state->cluster_params.resize(1);
  try {
    state->cluster_params[0] = mixing_dist->prior_draw();
  } catch (const std::exception& e) {
    Rcpp::stop("Failed to initialize cluster parameters: " + std::string(e.what()));
  }
}

void MCMCRunner::single_iteration_update() {
  // Validate state before iteration
  if (state->n_clusters <= 0 || state->cluster_params.empty()) {
    Rcpp::stop("Invalid cluster state in single_iteration_update");
  }
  
  // Pre-compute predictive probabilities for conjugate distributions
  std::vector<double> predictive_probs;
  if (mixing_dist->is_conjugate()) {
    predictive_probs.resize(data.n_rows);
    for (size_t i = 0; i < data.n_rows; ++i) {
      try {
        arma::vec data_point = data.row(i).t();
        double pred_prob = mixing_dist->predictive_probability(data_point);
        if (std::isfinite(pred_prob) && pred_prob > 0) {
          predictive_probs[i] = pred_prob;
        } else {
          predictive_probs[i] = 1e-10;  // Small but positive probability
        }
      } catch (const std::exception& e) {
        predictive_probs[i] = 1e-10;  // Fallback value
      }
    }
  }
  
  // Update cluster assignments - choose algorithm based on conjugacy
  if (mixing_dist->is_conjugate()) {
    update_cluster_assignments_algorithm4(predictive_probs);
  } else {
    update_cluster_assignments_algorithm8();
  }

  // Update cluster parameters
  update_cluster_parameters();

  // Update concentration parameter
  if (update_concentration_flag) {
    update_concentration();
  }
}

int MCMCRunner::sample_categorical(const std::vector<double>& probs) {
  double u = R::runif(0, 1);
  double cumsum = 0.0;

  for (size_t i = 0; i < probs.size(); ++i) {
    cumsum += probs[i];
    if (u <= cumsum) {
      return i;
    }
  }

  return probs.size() - 1;  // Fallback to last category
}

std::vector<double> MCMCRunner::stable_probs_from_log(const std::vector<double>& log_probs) const {
  std::vector<double> safe_log_probs(log_probs);
  const double neg_inf = -std::numeric_limits<double>::infinity();

  for (double& value : safe_log_probs) {
    if (std::isnan(value)) {
      value = neg_inf;
    }
  }

  int n_pos_inf = 0;
  for (double value : safe_log_probs) {
    if (std::isinf(value) && value > 0) {
      n_pos_inf++;
    }
  }

  if (n_pos_inf > 0) {
    std::vector<double> probs(safe_log_probs.size(), 0.0);
    const double mass = 1.0 / static_cast<double>(n_pos_inf);
    for (size_t i = 0; i < safe_log_probs.size(); ++i) {
      if (std::isinf(safe_log_probs[i]) && safe_log_probs[i] > 0) {
        probs[i] = mass;
      }
    }
    return probs;
  }

  bool any_finite = false;
  double max_log_prob = neg_inf;
  for (double value : safe_log_probs) {
    if (std::isfinite(value)) {
      any_finite = true;
      if (value > max_log_prob) {
        max_log_prob = value;
      }
    }
  }

  if (!any_finite) {
    return std::vector<double>(safe_log_probs.size(),
                               1.0 / static_cast<double>(safe_log_probs.size()));
  }

  std::vector<double> probs(safe_log_probs.size(), 0.0);
  double prob_sum = 0.0;
  for (size_t i = 0; i < safe_log_probs.size(); ++i) {
    if (std::isfinite(safe_log_probs[i])) {
      probs[i] = std::exp(safe_log_probs[i] - max_log_prob);
      if (!std::isfinite(probs[i])) {
        probs[i] = 0.0;
      }
    }
    prob_sum += probs[i];
  }

  if (!std::isfinite(prob_sum) || prob_sum <= 0.0) {
    return std::vector<double>(safe_log_probs.size(),
                               1.0 / static_cast<double>(safe_log_probs.size()));
  }

  for (double& value : probs) {
    value /= prob_sum;
  }

  return probs;
}

void MCMCRunner::cleanup_empty_clusters() {
  // Safety check: ensure state is valid
  if (!state || state->cluster_labels.empty()) {
    return;
  }
  
  std::vector<int> new_labels(state->cluster_labels.size());
  std::vector<arma::vec> new_params;

  int new_idx = 0;
  std::map<int, int> old_to_new;

  // Ensure cluster_sizes is properly sized
  if (state->cluster_sizes.n_elem < static_cast<size_t>(state->n_clusters)) {
    state->cluster_sizes.resize(state->n_clusters);
    state->cluster_sizes.zeros();
  }

  // Build mapping and new parameters with comprehensive bounds checking
  for (int k = 0; k < state->n_clusters; ++k) {
    if (k >= 0 && k < static_cast<int>(state->cluster_sizes.n_elem) && 
        k < static_cast<int>(state->cluster_params.size()) &&
        state->cluster_sizes[k] > 0) {
      old_to_new[k] = new_idx;
      new_params.push_back(state->cluster_params[k]);
      new_idx++;
    }
  }

  // If no non-empty clusters, create one default cluster
  if (new_params.empty()) {
    try {
      arma::vec default_param = mixing_dist->prior_draw();
      if (default_param.is_finite()) {
        new_params.push_back(default_param);
        old_to_new[0] = 0;
        new_idx = 1;
      }
    } catch (...) {
      // If prior_draw fails, we have a more serious problem
      // But don't let it crash the cleanup
      return;
    }
  }

  // Update labels with bounds checking
  for (size_t i = 0; i < state->cluster_labels.size(); ++i) {
    int current_label = state->cluster_labels[i];
    if (current_label >= 0 && old_to_new.count(current_label)) {
      new_labels[i] = old_to_new[current_label];
    } else {
      // Assign to first available cluster (0-indexed)
      new_labels[i] = 0;
    }
  }

  // Update state
  state->cluster_labels = new_labels;
  state->cluster_params = new_params;
  state->n_clusters = new_params.size();
  state->update_cluster_counts();
}

Rcpp::List MCMCRunner::run() {
  // Validate data dimensions
  if (data.n_rows == 0 || data.n_cols == 0) {
    Rcpp::stop("Data matrix has invalid dimensions");
  }
  
  // Initialize state
  initialize_state();
  double last_pre_update_loglik = R_NegInf;

  // MCMC loop with bounds checking
  for (int iter = 0; iter < n_iter; ++iter) {
    try {
      if (!store_history) {
        last_pre_update_loglik = compute_repaired_r_loglikelihood();
      }

      // Single iteration update
      single_iteration_update();

      // Store current iteration only when history is requested.
      if (store_history) {
        store_iteration(iter);
      }
      
    } catch (const std::exception& e) {
      Rcpp::stop("Error at MCMC iteration " + std::to_string(iter) + ": " + std::string(e.what()));
    }
  }

  if (!store_history) {
    Rcpp::IntegerVector final_labels(state->cluster_labels.begin(),
                                     state->cluster_labels.end());
    Rcpp::List final_params(state->cluster_params.size());
    for (size_t j = 0; j < state->cluster_params.size(); ++j) {
      final_params[j] = state->cluster_params[j];
    }

    arma::vec alpha_vector(1);
    alpha_vector[0] = state->alpha;

    arma::vec likelihood_vector(1);
    likelihood_vector[0] = last_pre_update_loglik;

    return Rcpp::List::create(
      Rcpp::Named("alpha_chain") = alpha_vector,
      Rcpp::Named("likelihood_chain") = likelihood_vector,
      Rcpp::Named("cluster_labels") = Rcpp::List::create(final_labels),
      Rcpp::Named("theta") = Rcpp::List::create(final_params)
    );
  }

  // Convert stored samples to proper format for R
  int n_stored = 0;
  std::vector<arma::vec> alpha_chain;
  std::vector<std::vector<int>> labels_chain;
  std::vector<std::vector<arma::vec>> theta_chain;
  std::vector<int> n_clusters_chain;
  std::vector<double> likelihood_chain;

  for (int iter = n_burn; iter < n_iter; iter += thin) {
    alpha_chain.push_back(alpha_samples[iter]);
    labels_chain.push_back(cluster_samples[iter]);
    theta_chain.push_back(theta_samples[iter]);
    likelihood_chain.push_back(likelihood_samples[iter]);

    std::set<int> unique_labels(cluster_samples[iter].begin(),
                                cluster_samples[iter].end());
    n_clusters_chain.push_back(unique_labels.size());
    n_stored++;
  }

  // Convert to matrices for R
  arma::mat labels_matrix(n_stored, data.n_rows);
  arma::vec alpha_vector(n_stored);
  arma::vec likelihood_vector(n_stored);

  for (int i = 0; i < n_stored; ++i) {
    alpha_vector[i] = alpha_chain[i][0];
    likelihood_vector[i] = likelihood_chain[i];
    for (size_t j = 0; j < data.n_rows; ++j) {
      labels_matrix(i, j) = labels_chain[i][j] + 1;  // Convert to 1-indexed for R
    }
  }

  // Convert theta to list format
  Rcpp::List theta_list(n_stored);
  for (int i = 0; i < n_stored; ++i) {
    Rcpp::List iter_params(theta_chain[i].size());
    for (size_t j = 0; j < theta_chain[i].size(); ++j) {
      iter_params[j] = theta_chain[i][j];
    }
    theta_list[i] = iter_params;
  }

  // Convert n_clusters to vector
  Rcpp::IntegerVector n_clusters_vector(n_clusters_chain.begin(),
                                        n_clusters_chain.end());

  return Rcpp::List::create(
    Rcpp::Named("labels_chain") = labels_matrix,
    Rcpp::Named("alpha_chain") = alpha_vector,
    Rcpp::Named("theta_chain") = theta_list,
    Rcpp::Named("n_clusters") = n_clusters_vector,
    Rcpp::Named("likelihood_chain") = likelihood_vector,
    Rcpp::Named("cluster_labels") = labels_chain,
    Rcpp::Named("alpha") = alpha_chain,
    Rcpp::Named("theta") = theta_chain
  );
}

void MCMCRunner::update_cluster_assignments_algorithm4(const std::vector<double>& predictive_probs) {
  const double neg_inf = -std::numeric_limits<double>::infinity();
  const bool gaussian_immediate_cleanup =
    dynamic_cast<GaussianMixing*>(mixing_dist.get()) != nullptr;
  const bool normal_fixed_variance_immediate_cleanup =
    dynamic_cast<NormalFixedVarianceMixing*>(mixing_dist.get()) != nullptr;

  for (size_t i = 0; i < data.n_rows; ++i) {
    arma::vec obs = data.row(i).t();
    int current_cluster = state->cluster_labels[i];

    // Remove observation from current cluster with comprehensive bounds checking
    if (current_cluster >= 0 && 
        current_cluster < static_cast<int>(state->cluster_sizes.n_elem) &&
        current_cluster < static_cast<int>(state->cluster_params.size()) &&
        current_cluster < state->n_clusters) {
      if (state->cluster_sizes[current_cluster] > 0) {
        state->cluster_sizes[current_cluster]--;
      }
    }

    std::vector<double> log_probs(state->n_clusters + 1, neg_inf);
    for (int k = 0; k < state->n_clusters; ++k) {
      if (k >= 0 && 
          k < static_cast<int>(state->cluster_sizes.n_elem) && 
          k < static_cast<int>(state->cluster_params.size()) &&
          state->cluster_sizes[k] > 0) {
        double log_lik = mixing_dist->log_likelihood(obs, state->cluster_params[k]);
        if (std::isfinite(log_lik)) {
          log_probs[k] = std::log(state->cluster_sizes[k]) + log_lik;
        }
      }
    }

    if (state->alpha > 0.0 &&
        i < predictive_probs.size() &&
        std::isfinite(predictive_probs[i]) &&
        predictive_probs[i] > 0.0) {
      log_probs[state->n_clusters] = std::log(state->alpha) + std::log(predictive_probs[i]);
    }

    int chosen_idx = sample_categorical(stable_probs_from_log(log_probs));

    if (chosen_idx < state->n_clusters) {
      if (chosen_idx >= 0 && chosen_idx < static_cast<int>(state->cluster_sizes.n_elem)) {
        state->cluster_labels[i] = chosen_idx;
        state->cluster_sizes[chosen_idx]++;

        // Match the repaired R Gaussian conjugate sweep, which removes an empty
        // source cluster immediately inside ClusterLabelChange() rather than
        // deferring compaction until the end of the sweep.
        if ((gaussian_immediate_cleanup ||
             normal_fixed_variance_immediate_cleanup) &&
            current_cluster >= 0 &&
            current_cluster < static_cast<int>(state->cluster_sizes.n_elem) &&
            current_cluster < static_cast<int>(state->cluster_params.size()) &&
            current_cluster != chosen_idx &&
            state->cluster_sizes[current_cluster] == 0) {
          cleanup_empty_clusters();
        }
      }
    } else {
      arma::mat singleton_data(1, data.n_cols);
      singleton_data.row(0) = data.row(i);

      if (current_cluster >= 0 &&
          current_cluster < state->n_clusters &&
          current_cluster < static_cast<int>(state->cluster_sizes.n_elem) &&
          current_cluster < static_cast<int>(state->cluster_params.size()) &&
          state->cluster_sizes[current_cluster] == 0) {
        state->cluster_params[current_cluster] =
          mixing_dist->posterior_draw(singleton_data, state->cluster_params[current_cluster]);
        state->cluster_labels[i] = current_cluster;
        state->cluster_sizes[current_cluster] = 1;
      } else {
        arma::vec new_param = mixing_dist->posterior_draw(singleton_data, arma::vec());
        int new_cluster_idx = state->n_clusters;
        state->cluster_params.push_back(new_param);

        arma::vec new_cluster_sizes(state->cluster_sizes.n_elem + 1);
        if (state->cluster_sizes.n_elem > 0) {
          new_cluster_sizes.head(state->cluster_sizes.n_elem) = state->cluster_sizes;
        }
        new_cluster_sizes(state->cluster_sizes.n_elem) = 1;
        state->cluster_sizes = new_cluster_sizes;

        state->cluster_labels[i] = new_cluster_idx;
        state->n_clusters++;
      }
    }
  }
  
  // Match the repaired R fixed-variance normal sweep more closely: once empty
  // clusters are compacted immediately during reassignment, there is no
  // separate end-of-sweep cleanup step for this model.
  if (!normal_fixed_variance_immediate_cleanup) {
    cleanup_empty_clusters();
  }
}

void MCMCRunner::update_cluster_assignments_algorithm8() {
  cleanup_empty_clusters();  // First clean up any empty clusters
  const double neg_inf = -std::numeric_limits<double>::infinity();
  bool immediate_cleanup_nonconjugate =
    dynamic_cast<BetaMixing*>(mixing_dist.get()) != nullptr ||
    dynamic_cast<Beta2Mixing*>(mixing_dist.get()) != nullptr ||
    dynamic_cast<WeibullMixing*>(mixing_dist.get()) != nullptr ||
    dynamic_cast<MVNormal2Mixing*>(mixing_dist.get()) != nullptr;

  for (size_t i = 0; i < data.n_rows; ++i) {
    arma::vec obs = data.row(i).t();
    int current_cluster = state->cluster_labels[i];

    // Remove observation from current cluster with bounds checking
    if (current_cluster >= 0 && current_cluster < static_cast<int>(state->cluster_sizes.n_elem)) {
      if (state->cluster_sizes[current_cluster] > 0) {
        state->cluster_sizes[current_cluster]--;
      }
    }

    bool reuse_empty_cluster =
      current_cluster >= 0 &&
      current_cluster < state->n_clusters &&
      current_cluster < static_cast<int>(state->cluster_sizes.n_elem) &&
      current_cluster < static_cast<int>(state->cluster_params.size()) &&
      state->cluster_sizes[current_cluster] == 0;

    std::vector<arma::vec> auxiliary_params;
    auxiliary_params.reserve(m_auxiliary);

    if (reuse_empty_cluster) {
      auxiliary_params.push_back(state->cluster_params[current_cluster]);
    }

    int n_prior_draws = reuse_empty_cluster ? std::max(0, m_auxiliary - 1) : m_auxiliary;
    for (int j = 0; j < n_prior_draws; ++j) {
      auxiliary_params.push_back(mixing_dist->prior_draw());
    }

    std::vector<double> log_probs(state->n_clusters + auxiliary_params.size(), neg_inf);

    for (int k = 0; k < state->n_clusters; ++k) {
      if (k >= 0 && k < static_cast<int>(state->cluster_params.size()) && 
          k < static_cast<int>(state->cluster_sizes.n_elem) &&
          state->cluster_sizes[k] > 0) {
        double log_lik = mixing_dist->log_likelihood(obs, state->cluster_params[k]);
        if (std::isfinite(log_lik)) {
          log_probs[k] = std::log(state->cluster_sizes[k]) + log_lik;
        }
      }
    }

    double aux_log_weight = neg_inf;
    if (state->alpha > 0.0 && m_auxiliary > 0) {
      aux_log_weight = std::log(state->alpha) - std::log(static_cast<double>(m_auxiliary));
    }

    for (size_t j = 0; j < auxiliary_params.size(); ++j) {
      double log_lik = mixing_dist->log_likelihood(obs, auxiliary_params[j]);
      if (std::isfinite(log_lik) && std::isfinite(aux_log_weight)) {
        log_probs[state->n_clusters + j] = aux_log_weight + log_lik;
      }
    }

    int chosen_idx = sample_categorical(stable_probs_from_log(log_probs));

    if (chosen_idx < state->n_clusters) {
      state->cluster_labels[i] = chosen_idx;
      state->cluster_sizes[chosen_idx]++;

      if (immediate_cleanup_nonconjugate &&
          reuse_empty_cluster &&
          current_cluster != chosen_idx) {
        cleanup_empty_clusters();
      }
    } else {
      int aux_idx = chosen_idx - state->n_clusters;
      arma::vec new_param = auxiliary_params[aux_idx];

      if (reuse_empty_cluster) {
        state->cluster_params[current_cluster] = new_param;
        state->cluster_labels[i] = current_cluster;
        state->cluster_sizes[current_cluster] = 1;
      } else {
        int new_cluster_idx = state->n_clusters;
        state->cluster_params.push_back(new_param);

        arma::vec new_cluster_sizes(state->cluster_sizes.n_elem + 1);
        if (state->cluster_sizes.n_elem > 0) {
          new_cluster_sizes.head(state->cluster_sizes.n_elem) = state->cluster_sizes;
        }
        new_cluster_sizes(state->cluster_sizes.n_elem) = 1;
        state->cluster_sizes = new_cluster_sizes;

        state->cluster_labels[i] = new_cluster_idx;
        state->n_clusters++;
      }
    }
  }

  cleanup_empty_clusters();  // Clean up after all assignments
}

void MCMCRunner::update_cluster_parameters() {
  for (int k = 0; k < state->n_clusters; ++k) {
    if (k >= 0 && k < static_cast<int>(state->cluster_sizes.n_elem) && 
        k < static_cast<int>(state->cluster_params.size()) &&
        state->cluster_sizes[k] > 0) {
      
      // Collect indices for cluster k with bounds checking
      std::vector<int> cluster_indices;
      for (size_t i = 0; i < data.n_rows; ++i) {
        if (i < state->cluster_labels.size() && state->cluster_labels[i] == k) {
          cluster_indices.push_back(i);
        }
      }

      if (!cluster_indices.empty()) {
        try {
          // Extract cluster data with bounds checking
          arma::mat cluster_data(cluster_indices.size(), data.n_cols);
          for (size_t idx = 0; idx < cluster_indices.size(); ++idx) {
            int data_idx = cluster_indices[idx];
            if (data_idx >= 0 && data_idx < static_cast<int>(data.n_rows)) {
              cluster_data.row(idx) = data.row(data_idx);
            }
          }

          // Draw from posterior
          arma::vec new_param = mixing_dist->posterior_draw(cluster_data, state->cluster_params[k]);
          if (new_param.is_finite()) {
            state->cluster_params[k] = new_param;
          }
        } catch (const std::exception& e) {
          // Keep existing parameter if update fails
          continue;
        }
      }
    }
  }
}

void MCMCRunner::update_concentration() {
  // Match the repaired R implementation of update_concentration() directly.
  double x = R::rbeta(state->alpha + 1.0, data.n_rows);
  double log_x = std::log(x);

  double pi1 = alpha_prior_shape + state->n_clusters - 1.0;
  double pi2 = data.n_rows * (alpha_prior_rate - log_x);
  double mixing_prob = pi1 / (pi1 + pi2);
  double post_shape = alpha_prior_shape + state->n_clusters;
  double post_rate = alpha_prior_rate - log_x;

  if (R::runif(0, 1) > mixing_prob) {
    post_shape -= 1.0;
  }

  state->alpha = R::rgamma(post_shape, 1.0 / post_rate);
}

double MCMCRunner::compute_repaired_r_loglikelihood() const {
  const double neg_inf = -std::numeric_limits<double>::infinity();

  if (!state || data.n_rows == 0 || state->n_clusters <= 0) {
    return neg_inf;
  }

  if (dynamic_cast<const MVNormalMixing*>(mixing_dist.get()) != nullptr ||
      dynamic_cast<const ExponentialMixing*>(mixing_dist.get()) != nullptr ||
      dynamic_cast<const Beta2Mixing*>(mixing_dist.get()) != nullptr) {
    arma::vec raw(data.n_rows * state->n_clusters, arma::fill::zeros);
    arma::vec weights(state->n_clusters, arma::fill::zeros);

    for (int k = 0; k < state->n_clusters; ++k) {
      if (k < static_cast<int>(state->cluster_sizes.n_elem)) {
        weights[k] = state->cluster_sizes[k] / static_cast<double>(data.n_rows);
      }
    }

    arma::uword idx = 0;
    for (arma::uword i = 0; i < data.n_rows; ++i) {
      arma::vec obs = data.row(i).t();

      for (int k = 0; k < state->n_clusters; ++k) {
        double log_lik = neg_inf;

        if (k >= 0 &&
            k < static_cast<int>(state->cluster_sizes.n_elem) &&
            k < static_cast<int>(state->cluster_params.size()) &&
            state->cluster_sizes[k] > 0) {
          log_lik = mixing_dist->log_likelihood(obs, state->cluster_params[k]);
        }

        if (std::isnan(log_lik)) {
          return NA_REAL;
        }

        raw[idx++] = std::isfinite(log_lik) ? std::exp(log_lik) : 0.0;
      }
    }

    arma::mat likelihood_values(raw.memptr(),
                                data.n_rows,
                                state->n_clusters,
                                false,
                                true);
    arma::vec mixture = likelihood_values * weights;
    double total_loglik = 0.0;

    for (arma::uword i = 0; i < mixture.n_elem; ++i) {
      if (Rcpp::NumericVector::is_na(mixture[i]) || std::isnan(mixture[i])) {
        return NA_REAL;
      }
      if (mixture[i] <= 0.0 || !std::isfinite(mixture[i])) {
        return neg_inf;
      }
      total_loglik += std::log(mixture[i]);
    }

    return total_loglik;
  }

  double total_loglik = 0.0;
  const double log_n = std::log(static_cast<double>(data.n_rows));

  for (size_t i = 0; i < data.n_rows; ++i) {
    arma::vec obs = data.row(i).t();
    double max_log_term = neg_inf;
    double sum_exp = 0.0;
    bool any_finite = false;
    bool any_pos_inf = false;
    bool any_nan = false;

    for (int k = 0; k < state->n_clusters; ++k) {
      if (k < 0 ||
          k >= static_cast<int>(state->cluster_sizes.n_elem) ||
          k >= static_cast<int>(state->cluster_params.size()) ||
          state->cluster_sizes[k] <= 0) {
        continue;
      }

      double log_lik = mixing_dist->log_likelihood(obs, state->cluster_params[k]);
      if (std::isnan(log_lik)) {
        any_nan = true;
        break;
      }

      double log_term = std::log(state->cluster_sizes[k]) - log_n + log_lik;
      if (std::isnan(log_term)) {
        any_nan = true;
        break;
      }

      if (std::isinf(log_term) && log_term > 0) {
        any_pos_inf = true;
        continue;
      }

      if (std::isfinite(log_term)) {
        max_log_term = std::max(max_log_term, log_term);
        any_finite = true;
      }
    }

    if (any_nan) {
      return NA_REAL;
    }

    if (any_pos_inf) {
      return R_PosInf;
    }

    if (!any_finite) {
      return neg_inf;
    }

    for (int k = 0; k < state->n_clusters; ++k) {
      if (k < 0 ||
          k >= static_cast<int>(state->cluster_sizes.n_elem) ||
          k >= static_cast<int>(state->cluster_params.size()) ||
          state->cluster_sizes[k] <= 0) {
        continue;
      }

      double log_lik = mixing_dist->log_likelihood(obs, state->cluster_params[k]);
      double log_term = std::log(state->cluster_sizes[k]) - log_n + log_lik;

      if (std::isfinite(log_term)) {
        sum_exp += std::exp(log_term - max_log_term);
      }
    }

    if (!std::isfinite(sum_exp) || sum_exp <= 0.0) {
      return neg_inf;
    }

    total_loglik += max_log_term + std::log(sum_exp);
  }

  return total_loglik;
}

void MCMCRunner::store_iteration(int iter) {
  // Store alpha
  arma::vec alpha_vec(1);
  alpha_vec[0] = state->alpha;
  alpha_samples.push_back(alpha_vec);

  // Store cluster labels (copy to avoid reference issues)
  std::vector<int> labels_copy(state->cluster_labels.begin(),
                               state->cluster_labels.end());
  cluster_samples.push_back(labels_copy);

  // Store cluster parameters (deep copy)
  std::vector<arma::vec> params_copy;
  for (const auto& param : state->cluster_params) {
    params_copy.push_back(arma::vec(param));
  }
  theta_samples.push_back(params_copy);

  // Calculate and store likelihood
  double log_lik = 0.0;
  // Changed loop counter from 'int' to 'size_t' to avoid signed/unsigned comparison warnings
  for (size_t i = 0; i < data.n_rows; ++i) {
    arma::vec obs = data.row(i).t();
    int cluster = state->cluster_labels[i];
    if (cluster >= 0 && cluster < static_cast<int>(state->cluster_params.size())) {
      log_lik += mixing_dist->log_likelihood(obs, state->cluster_params[cluster]);
    }
  }
  likelihood_samples.push_back(log_lik);
}

} // namespace dirichletprocess
