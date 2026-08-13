// inst/include/mcmc_runner.h
#ifndef MCMC_RUNNER_H
#define MCMC_RUNNER_H

#include <RcppArmadillo.h>
#include <memory>
#include <vector>
#include <map>
#include "mixing_distribution_base.h"

namespace dirichletprocess {

// Forward declarations
class DPState;

// Main MCMC runner class
class MCMCRunner {
  protected:  // CHANGED FROM private TO protected
    // Data
    arma::mat data;

    // Model components
    std::unique_ptr<MixingDistribution> mixing_dist;

    // MCMC state
    std::unique_ptr<DPState> state;

    // Parameters
    int n_iter;
    int n_burn;
    int thin;
    bool update_concentration_flag;
    int m_auxiliary; // Number of auxiliary parameters for Algorithm 8
    bool store_history;
    bool use_initial_state;
    std::vector<int> initial_cluster_labels;
    std::vector<arma::vec> initial_cluster_params;

    // Alpha prior parameters
    double alpha_prior_shape;
    double alpha_prior_rate;

    // Storage for results
    std::vector<arma::vec> alpha_samples;
    std::vector<std::vector<int>> cluster_samples;
    std::vector<std::vector<arma::vec>> theta_samples;
    std::vector<double> likelihood_samples;  // For tracking likelihood

    // ADDED: Storage for n_clusters chain (needed by MCMCRunnerManual)
    std::vector<int> n_clusters_chain;

public:
  MCMCRunner(const arma::mat& data,
             const Rcpp::List& mixing_dist_params,
             const Rcpp::List& mcmc_params);
  
  // Virtual destructor to ensure proper cleanup
  virtual ~MCMCRunner() = default;

  // Main MCMC loop
  Rcpp::List run();
  
  // Single iteration methods for hierarchical use
  void single_iteration_update();
  void initialize_state();
  
  // Getters for hierarchical access
  const std::unique_ptr<DPState>& get_state() const { return state; }
  const std::unique_ptr<MixingDistribution>& get_mixing_dist() const { return mixing_dist; }

  protected:  // CHANGED FROM private TO protected
    // MCMC steps
    void update_cluster_assignments_algorithm4(const std::vector<double>& predictive_probs); // Algorithm 4 (conjugate)
    void update_cluster_assignments_algorithm8(); // Algorithm 8 (non-conjugate) implementation
    void update_cluster_parameters();
    void update_concentration();
    void store_iteration(int iter);
    void cleanup_empty_clusters();
    double compute_repaired_r_loglikelihood() const;

    // Helper function for categorical sampling
    int sample_categorical(const std::vector<double>& probs);
    std::vector<double> stable_probs_from_log(const std::vector<double>& log_probs) const;
};

// State container for DP
class DPState {
public:
  std::vector<int> cluster_labels;
  std::vector<arma::vec> cluster_params;
  arma::vec cluster_sizes;
  double alpha;
  int n_clusters;

  DPState(int n_obs, double initial_alpha)
    : cluster_labels(n_obs), alpha(initial_alpha), n_clusters(0) {
    cluster_sizes.set_size(0);
  }

  void update_cluster_counts() {
    // Count actual clusters
    std::map<int, int> label_counts;
    for (int label : cluster_labels) {
      label_counts[label]++;
    }

    n_clusters = label_counts.size();

    // Update cluster sizes
    cluster_sizes.zeros(n_clusters);
    int idx = 0;
    for (const auto& pair : label_counts) {
      cluster_sizes[idx] = pair.second;
      idx++;
    }
  }
};

} // namespace dirichletprocess

#endif
