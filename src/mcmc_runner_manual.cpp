// src/mcmc_runner_manual.cpp
#include "mcmc_runner_manual.h"
#include "utilities.h"
#include <cmath>
#include <algorithm>

namespace dirichletprocess {

void MCMCRunnerManual::initialize_manual_storage() {
  // Initialize with at least one cluster
  state->n_clusters = 1;
  state->cluster_sizes = arma::zeros(1);  // Use arma::zeros(1) instead of arma::zeros<arma::uvec>(1)
  state->cluster_sizes[0] = data.n_rows;

  // All observations start in cluster 0
  state->cluster_labels = std::vector<int>(data.n_rows, 0);

  // Draw initial parameters for the first cluster
  state->cluster_params.clear();
  state->cluster_params.push_back(mixing_dist->prior_draw());

  // Reserve space for diagnostic chains
  log_posterior_chain.reserve(n_iter);
  entropy_chain.reserve(n_iter);

  // Initialize auxiliary parameters
  auxiliary_params.clear();
  for (int j = 0; j < m_auxiliary; j++) {
    auxiliary_params.push_back(mixing_dist->prior_draw());
  }
}

void MCMCRunnerManual::step_cluster_assignments() {
  if (!update_clusters_flag) return;

  // Update auxiliary parameters for Algorithm 8
  update_auxiliary_parameters();

  // Call parent class method with temperature adjustment
  if (temperature != 1.0) {
    // Implement tempered sampling
    for (arma::uword i = 0; i < data.n_rows; ++i) {
      arma::vec obs = data.row(i).t();
      int current_cluster = state->cluster_labels[i];

      // Remove from current cluster
      if (current_cluster < static_cast<int>(state->cluster_sizes.n_elem)) {
        state->cluster_sizes[current_cluster]--;
      }

      // Calculate tempered probabilities
      std::vector<double> probs;

      // Existing clusters
      for (int k = 0; k < state->n_clusters; ++k) {
        double log_lik = mixing_dist->log_likelihood(obs, state->cluster_params[k]);
        double weight = (k == current_cluster && state->cluster_sizes[k] == 0) ?
        state->alpha / m_auxiliary : state->cluster_sizes[k];
        probs.push_back(weight * std::exp(log_lik / temperature));
      }

      // Auxiliary parameters
      for (int j = 0; j < m_auxiliary; ++j) {
        double log_lik = mixing_dist->log_likelihood(obs, auxiliary_params[j]);
        probs.push_back((state->alpha / m_auxiliary) * std::exp(log_lik / temperature));
      }

      // Sample new cluster
      int new_cluster = sample_categorical(probs);
      state->cluster_labels[i] = new_cluster;

      // Update cluster sizes
      if (new_cluster < state->n_clusters) {
        state->cluster_sizes[new_cluster]++;
      } else {
        // New cluster
        state->n_clusters++;
        state->cluster_sizes.resize(state->n_clusters);
        state->cluster_sizes[state->n_clusters - 1] = 1;
        state->cluster_params.push_back(auxiliary_params[new_cluster - state->n_clusters]);
      }
    }

    cleanup_empty_clusters();
  } else {
    // Use parent class method
    update_cluster_assignments_algorithm8();
  }
}

void MCMCRunnerManual::step_cluster_parameters() {
  if (!update_params_flag) return;

  for (int k = 0; k < state->n_clusters; ++k) {
    if (state->cluster_sizes[k] > 0) {
      // Get data points in this cluster
      arma::mat cluster_data;
      std::vector<int> cluster_indices;

      for (arma::uword i = 0; i < data.n_rows; ++i) {
        if (state->cluster_labels[i] == k) {
          cluster_indices.push_back(i);
        }
      }

      cluster_data.set_size(cluster_indices.size(), data.n_cols);
      for (size_t idx = 0; idx < cluster_indices.size(); ++idx) {
        cluster_data.row(idx) = data.row(cluster_indices[idx]);
      }

      // Draw from posterior
      arma::vec new_params = mixing_dist->posterior_draw(
        cluster_data, state->cluster_params[k]);

      // Apply bounds if set
      if (param_lower_bounds.n_elem > 0 && check_parameter_bounds(new_params)) {
        state->cluster_params[k] = new_params;
      }
    }
  }
}

void MCMCRunnerManual::step_concentration() {
  if (update_concentration_flag) {
    update_concentration();
  }
}

void MCMCRunnerManual::perform_iteration() {
  step_cluster_assignments();
  step_cluster_parameters();
  step_concentration();

  // Store diagnostics
  log_posterior_chain.push_back(get_log_posterior());
  entropy_chain.push_back(get_clustering_entropy());

  if (current_iteration >= n_burn &&
      (current_iteration - n_burn) % thin == 0) {
    store_iteration(current_iteration);
  }

  current_iteration++;
}

Rcpp::List MCMCRunnerManual::get_current_state() const {
  Rcpp::List params_list;
  for (const auto& param : state->cluster_params) {
    params_list.push_back(param);
  }

  double log_lik = 0.0;
  for (arma::uword i = 0; i < data.n_rows; i++) {
    arma::vec obs = data.row(i).t();
    int label = state->cluster_labels[i];
    if (label < static_cast<int>(state->cluster_params.size())) {
      log_lik += mixing_dist->log_likelihood(obs, state->cluster_params[label]);
    }
  }

  return Rcpp::List::create(
    Rcpp::Named("cluster_labels") = state->cluster_labels,
    Rcpp::Named("cluster_params") = params_list,
    Rcpp::Named("cluster_sizes") = state->cluster_sizes,
    Rcpp::Named("alpha") = state->alpha,
    Rcpp::Named("n_clusters") = state->n_clusters,
    Rcpp::Named("iteration") = current_iteration,
    Rcpp::Named("log_likelihood") = log_lik,
    Rcpp::Named("log_posterior") = get_log_posterior(),
    Rcpp::Named("temperature") = temperature
  );
}

void MCMCRunnerManual::set_cluster_labels(const std::vector<int>& new_labels) {
  if (new_labels.size() != state->cluster_labels.size()) {
    Rcpp::stop("New labels must have same length as data");
  }

  state->cluster_labels = new_labels;
  state->update_cluster_counts();

  // Ensure we have parameters for all clusters
  int max_label = *std::max_element(new_labels.begin(), new_labels.end());
  while (static_cast<int>(state->cluster_params.size()) <= max_label) {
    state->cluster_params.push_back(mixing_dist->prior_draw());
  }
}

void MCMCRunnerManual::set_cluster_params(const Rcpp::List& new_params) {
  state->cluster_params.clear();
  for (int i = 0; i < new_params.size(); i++) {
    state->cluster_params.push_back(Rcpp::as<arma::vec>(new_params[i]));
  }
}

void MCMCRunnerManual::set_parameter_bounds(const arma::vec& lower, const arma::vec& upper) {
  if (lower.n_elem != upper.n_elem) {
    Rcpp::stop("Lower and upper bounds must have same dimension");
  }

  param_lower_bounds = lower;
  param_upper_bounds = upper;
}

Rcpp::List MCMCRunnerManual::get_auxiliary_params() const {
  Rcpp::List aux_list;
  for (const auto& param : auxiliary_params) {
    aux_list.push_back(param);
  }
  return aux_list;
}

void MCMCRunnerManual::set_update_flags(bool update_clusters, bool update_params, bool update_alpha) {
  update_clusters_flag = update_clusters;
  update_params_flag = update_params;
  update_concentration_flag = update_alpha;
}

arma::vec MCMCRunnerManual::get_cluster_likelihoods() const {
  arma::vec likelihoods(state->n_clusters);

  for (int k = 0; k < state->n_clusters; k++) {
    double log_lik = 0.0;
    int count = 0;

    for (arma::uword i = 0; i < data.n_rows; i++) {
      if (state->cluster_labels[i] == k) {
        arma::vec obs = data.row(i).t();
        log_lik += mixing_dist->log_likelihood(obs, state->cluster_params[k]);
        count++;
      }
    }

    likelihoods[k] = count > 0 ? log_lik : -INFINITY;
  }

  return likelihoods;
}

arma::mat MCMCRunnerManual::get_cluster_membership_matrix() const {
  arma::mat membership(data.n_rows, state->n_clusters, arma::fill::zeros);

  for (arma::uword i = 0; i < data.n_rows; i++) {
    int label = state->cluster_labels[i];
    if (label >= 0 && label < state->n_clusters) {
      membership(i, label) = 1.0;
    }
  }

  return membership;
}

Rcpp::List MCMCRunnerManual::get_cluster_statistics() const {
  Rcpp::List stats;

  for (int k = 0; k < state->n_clusters; k++) {
    int count = 0;
    double sum_log_lik = 0.0;

    for (arma::uword i = 0; i < data.n_rows; i++) {
      if (state->cluster_labels[i] == k) {
        arma::vec obs = data.row(i).t();
        sum_log_lik += mixing_dist->log_likelihood(obs, state->cluster_params[k]);
        count++;
      }
    }

    stats.push_back(Rcpp::List::create(
        Rcpp::Named("size") = count,
        Rcpp::Named("parameters") = state->cluster_params[k],
                                                         Rcpp::Named("log_likelihood") = sum_log_lik,
                                                         Rcpp::Named("mean_log_likelihood") = count > 0 ? sum_log_lik / count : -INFINITY
    ));
  }

  return stats;
}

void MCMCRunnerManual::merge_clusters(int cluster1, int cluster2) {
  if (cluster1 < 0 || cluster1 >= state->n_clusters ||
      cluster2 < 0 || cluster2 >= state->n_clusters) {
    Rcpp::stop("Invalid cluster indices");
  }

  if (cluster1 == cluster2) return;

  // Ensure cluster1 < cluster2 for consistency
  if (cluster1 > cluster2) {
    std::swap(cluster1, cluster2);
  }

  // Merge data from cluster2 into cluster1
  arma::mat merged_data;
  std::vector<int> merged_indices;

  for (arma::uword i = 0; i < data.n_rows; i++) {
    if (state->cluster_labels[i] == cluster1 || state->cluster_labels[i] == cluster2) {
      merged_indices.push_back(i);
    }
  }

  merged_data.set_size(merged_indices.size(), data.n_cols);
  for (size_t idx = 0; idx < merged_indices.size(); ++idx) {
    merged_data.row(idx) = data.row(merged_indices[idx]);
  }

  // Update parameters for merged cluster
  state->cluster_params[cluster1] = mixing_dist->posterior_draw(
    merged_data, state->cluster_params[cluster1]);

  // Update labels
  for (arma::uword i = 0; i < data.n_rows; i++) {
    if (state->cluster_labels[i] == cluster2) {
      state->cluster_labels[i] = cluster1;
    } else if (state->cluster_labels[i] > cluster2) {
      state->cluster_labels[i]--;
    }
  }

  // Remove cluster2
  state->cluster_params.erase(state->cluster_params.begin() + cluster2);
  state->n_clusters--;
  state->update_cluster_counts();
}

void MCMCRunnerManual::split_cluster(int cluster_id, double split_prob) {
  if (cluster_id < 0 || cluster_id >= state->n_clusters) {
    Rcpp::stop("Invalid cluster index");
  }

  if (state->cluster_sizes[cluster_id] < 2) {
    Rcpp::warning("Cannot split cluster with less than 2 observations");
    return;
  }

  // Create new cluster
  int new_cluster_id = state->n_clusters;
  state->n_clusters++;
  state->cluster_params.push_back(mixing_dist->prior_draw());

  // Randomly split observations
  for (arma::uword i = 0; i < data.n_rows; i++) {
    if (state->cluster_labels[i] == cluster_id) {
      if (R::runif(0, 1) < split_prob) {
        state->cluster_labels[i] = new_cluster_id;
      }
    }
  }

  state->update_cluster_counts();

  // Update parameters for both clusters
  step_cluster_parameters();
}

void MCMCRunnerManual::set_temperature(double temp) {
  if (temp <= 0) {
    Rcpp::stop("Temperature must be positive");
  }
  temperature = temp;
}

void MCMCRunnerManual::set_auxiliary_parameter_count(int m) {
  if (m < 1) {
    Rcpp::stop("Number of auxiliary parameters must be at least 1");
  }
  m_auxiliary = m;
  auxiliary_params.resize(m);
  update_auxiliary_parameters();
}

Rcpp::List MCMCRunnerManual::sample_posterior_predictive(int n_samples) {
  Rcpp::List samples;

  for (int s = 0; s < n_samples; s++) {
    // Sample cluster with Chinese Restaurant Process
    std::vector<double> probs;
    for (int k = 0; k < state->n_clusters; k++) {
      probs.push_back(state->cluster_sizes[k]);
    }
    probs.push_back(state->alpha);

    int chosen_cluster = sample_categorical(probs);

    arma::vec params;
    if (chosen_cluster < state->n_clusters) {
      params = state->cluster_params[chosen_cluster];
    } else {
      params = mixing_dist->prior_draw();
    }

    // Sample from likelihood - this would need to be added to MixingDistribution
    // For now, return the parameters
    samples.push_back(params);
  }

  return samples;
}

double MCMCRunnerManual::get_log_posterior() const {
  double log_post = 0.0;

  // Likelihood term
  for (arma::uword i = 0; i < data.n_rows; i++) {
    arma::vec obs = data.row(i).t();
    int label = state->cluster_labels[i];
    if (label < static_cast<int>(state->cluster_params.size())) {
      log_post += mixing_dist->log_likelihood(obs, state->cluster_params[label]);
    }
  }

  // Prior on cluster assignments (CRP)
  for (int k = 0; k < state->n_clusters; k++) {
    if (state->cluster_sizes[k] > 0) {
      log_post += std::log(state->cluster_sizes[k]);
    }
  }
  log_post += state->n_clusters * std::log(state->alpha);

  // Prior on alpha
  log_post += (alpha_prior_shape - 1) * std::log(state->alpha) -
    alpha_prior_rate * state->alpha;

  return log_post;
}

arma::vec MCMCRunnerManual::get_cluster_entropies() const {
  arma::vec entropies(state->n_clusters);

  for (int k = 0; k < state->n_clusters; k++) {
    if (state->cluster_sizes[k] > 0) {
      double p = state->cluster_sizes[k] / static_cast<double>(data.n_rows);
      entropies[k] = -p * std::log(p);
    } else {
      entropies[k] = 0.0;
    }
  }

  return entropies;
}

double MCMCRunnerManual::get_clustering_entropy() const {
  double entropy = 0.0;

  for (int k = 0; k < state->n_clusters; k++) {
    if (state->cluster_sizes[k] > 0) {
      double p = state->cluster_sizes[k] / static_cast<double>(data.n_rows);
      entropy -= p * std::log(p);
    }
  }

  return entropy;
}

Rcpp::List MCMCRunnerManual::get_convergence_diagnostics() const {
  // Calculate running statistics
  int n = log_posterior_chain.size();

  if (n < 100) {
    return Rcpp::List::create(
      Rcpp::Named("message") = "Not enough iterations for convergence diagnostics"
    );
  }

  // Split chain for Gelman-Rubin diagnostic
  int split_point = n / 2;

  // First half statistics
  double mean1 = 0.0, var1 = 0.0;
  for (int i = 0; i < split_point; i++) {
    mean1 += log_posterior_chain[i];
  }
  mean1 /= split_point;

  for (int i = 0; i < split_point; i++) {
    var1 += std::pow(log_posterior_chain[i] - mean1, 2);
  }
  var1 /= (split_point - 1);

  // Second half statistics
  double mean2 = 0.0, var2 = 0.0;
  for (int i = split_point; i < n; i++) {
    mean2 += log_posterior_chain[i];
  }
  mean2 /= (n - split_point);

  for (int i = split_point; i < n; i++) {
    var2 += std::pow(log_posterior_chain[i] - mean2, 2);
  }
  var2 /= (n - split_point - 1);

  // Approximate R-hat
  double W = (var1 + var2) / 2.0;
  double B = n * std::pow(mean1 - mean2, 2) / 2.0;
  double var_est = W + B / n;
  double R_hat = std::sqrt(var_est / W);

  // Effective sample size (rough approximation)
  double autocorr = 0.0;
  double overall_mean = (mean1 + mean2) / 2.0;
  for (int i = 1; i < n - 1; i++) {
    autocorr += (log_posterior_chain[i] - overall_mean) *
      (log_posterior_chain[i-1] - overall_mean);
  }
  autocorr /= ((n - 1) * var_est);
  double ess = n / (1 + 2 * autocorr);

  // Calculate mean of n_clusters_chain manually
  double mean_clusters = 0.0;
  if (!n_clusters_chain.empty()) {
    for (int val : n_clusters_chain) {
      mean_clusters += val;
    }
    mean_clusters /= n_clusters_chain.size();
  }

  // Calculate mean of entropy_chain manually
  double mean_entropy = 0.0;
  if (!entropy_chain.empty()) {
    for (double val : entropy_chain) {
      mean_entropy += val;
    }
    mean_entropy /= entropy_chain.size();
  }

  return Rcpp::List::create(
    Rcpp::Named("iterations_completed") = current_iteration,
    Rcpp::Named("log_posterior_mean") = (mean1 + mean2) / 2.0,
    Rcpp::Named("log_posterior_sd") = std::sqrt(var_est),
    Rcpp::Named("R_hat") = R_hat,
    Rcpp::Named("effective_sample_size") = ess,
    Rcpp::Named("acceptance_rate") = 1.0, // Would need to track this
    Rcpp::Named("mean_clusters") = mean_clusters,
    Rcpp::Named("mean_entropy") = mean_entropy
  );
}

Rcpp::List MCMCRunnerManual::get_results() const {
  int n_samples = alpha_samples.size();

  Rcpp::NumericMatrix labels_matrix(n_samples, data.n_rows);
  for (int i = 0; i < n_samples; i++) {
    for (arma::uword j = 0; j < data.n_rows; j++) {
      labels_matrix(i, j) = cluster_samples[i][j] + 1;
    }
  }

  Rcpp::List theta_list;
  for (int i = 0; i < n_samples; i++) {
    Rcpp::List iter_params;
    for (size_t j = 0; j < theta_samples[i].size(); j++) {
      iter_params.push_back(theta_samples[i][j]);
    }
    theta_list.push_back(iter_params);
  }

  return Rcpp::List::create(
    Rcpp::Named("cluster_labels") = state->cluster_labels,
    Rcpp::Named("cluster_params") = state->cluster_params,
    Rcpp::Named("alpha") = state->alpha,
    Rcpp::Named("n_clusters") = state->n_clusters,
    Rcpp::Named("labels_chain") = labels_matrix,
    Rcpp::Named("alpha_chain") = alpha_samples,
    Rcpp::Named("theta_chain") = theta_list,
    Rcpp::Named("n_clusters_chain") = n_clusters_chain,
    Rcpp::Named("likelihood_chain") = likelihood_samples,
    Rcpp::Named("log_posterior_chain") = log_posterior_chain,
    Rcpp::Named("entropy_chain") = entropy_chain,
    Rcpp::Named("iterations_completed") = current_iteration,
    Rcpp::Named("convergence_diagnostics") = get_convergence_diagnostics()
  );
}

void MCMCRunnerManual::update_auxiliary_parameters() {
  for (int j = 0; j < m_auxiliary; j++) {
    auxiliary_params[j] = mixing_dist->prior_draw();
  }
}

bool MCMCRunnerManual::check_parameter_bounds(const arma::vec& params) const {
  if (param_lower_bounds.n_elem == 0) return true;

  if (params.n_elem != param_lower_bounds.n_elem) {
    Rcpp::warning("Parameter dimension mismatch with bounds");
    return false;
  }

  for (arma::uword i = 0; i < params.n_elem; i++) {
    if (params[i] < param_lower_bounds[i] || params[i] > param_upper_bounds[i]) {
      return false;
    }
  }

  return true;
}

} // namespace dirichletprocess
