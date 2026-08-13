// inst/include/mcmc_runner_manual.h
#ifndef MCMC_RUNNER_MANUAL_H
#define MCMC_RUNNER_MANUAL_H

#include "mcmc_runner.h"

namespace dirichletprocess {

class MCMCRunnerManual : public MCMCRunner {
public:
  MCMCRunnerManual(const arma::mat& data,
                   const Rcpp::List& mixing_dist_params,
                   const Rcpp::List& mcmc_params)
    : MCMCRunner(data, mixing_dist_params, mcmc_params),
      current_iteration(0),
      update_clusters_flag(true),
      update_params_flag(true) {
    // Initialize storage for manual mode
    initialize_manual_storage();

    // Initialize auxiliary parameters storage
    auxiliary_params.resize(m_auxiliary);
  }

  // Core MCMC steps
  void step_cluster_assignments();
  void step_cluster_parameters();
  void step_concentration();
  void perform_iteration();

  // State access and modification
  Rcpp::List get_current_state() const;
  void set_cluster_labels(const std::vector<int>& new_labels);
  void set_cluster_params(const Rcpp::List& new_params);

  // Additional features
  void set_parameter_bounds(const arma::vec& lower, const arma::vec& upper);
  Rcpp::List get_auxiliary_params() const;
  void set_update_flags(bool update_clusters, bool update_params, bool update_alpha);
  arma::vec get_cluster_likelihoods() const;
  arma::mat get_cluster_membership_matrix() const;
  Rcpp::List get_cluster_statistics() const;
  void merge_clusters(int cluster1, int cluster2);
  void split_cluster(int cluster_id, double split_prob = 0.5);

  // Advanced sampling controls
  void set_temperature(double temp);
  void set_auxiliary_parameter_count(int m);
  Rcpp::List sample_posterior_predictive(int n_samples);

  // Diagnostic methods
  double get_log_posterior() const;
  arma::vec get_cluster_entropies() const;
  double get_clustering_entropy() const;
  Rcpp::List get_convergence_diagnostics() const;

  // Get results
  Rcpp::List get_results() const;

  // Status
  bool is_complete() const { return current_iteration >= n_iter; }
  int get_current_iteration() const { return current_iteration; }

private:
  int current_iteration;

  // Additional control flags
  bool update_clusters_flag;
  bool update_params_flag;

  // Parameter bounds
  arma::vec param_lower_bounds;
  arma::vec param_upper_bounds;

  // Auxiliary parameters for Algorithm 8
  std::vector<arma::vec> auxiliary_params;

  // Temperature for annealed sampling
  double temperature = 1.0;

  // Storage for convergence diagnostics
  std::vector<double> log_posterior_chain;
  std::vector<double> entropy_chain;

  void initialize_manual_storage();
  void update_auxiliary_parameters();
  bool check_parameter_bounds(const arma::vec& params) const;
};

} // namespace dirichletprocess

#endif
