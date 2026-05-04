#ifndef HIERARCHICAL_MVNORMAL_MIXING_H
#define HIERARCHICAL_MVNORMAL_MIXING_H

#include "mvnormal_mixing.h"
#include <RcppArmadillo.h>
#include <memory>
#include <vector>
#include <set>  // Add this include

namespace dirichletprocess {

// Structure to hold hierarchical MVNormal parameters
struct HierarchicalMVNormalParams {
  // Global parameters (G0)
  arma::vec global_mu;
  arma::mat global_sigma;
  std::vector<double> stick_weights;  // beta_k

  // Hyperparameters
  double gamma;  // Concentration for global DP
  arma::vec gamma_prior;  // Shape and rate for gamma

  // Local parameters for each group
  std::vector<double> alphas;  // Local concentration parameters
  std::vector<std::vector<double>> pi_k;  // Local weights

  // Prior parameters for G0
  arma::vec mu0;
  double kappa0;
  arma::mat Lambda;
  double nu;
};

class HierarchicalMVNormalMixing {
private:
  int n_groups;
  int n_sticks;
  HierarchicalMVNormalParams params;
  std::vector<std::unique_ptr<MVNormalMixing>> base_dists;

  // Helper functions
  std::vector<double> stick_breaking(double gamma, int n_sticks);
  std::vector<double> draw_gj(double alpha, const std::vector<double>& beta_k);

public:
  HierarchicalMVNormalMixing(int n_groups, int n_sticks,
                             const Rcpp::List& prior_params,
                             const arma::vec& alpha_prior,
                             const arma::vec& gamma_prior);

  // Main MCMC update functions
  void update_local_clusters(std::vector<arma::mat>& data,
                             std::vector<std::vector<int>>& labels,
                             std::vector<std::vector<arma::vec>>& local_params);

  void update_global_parameters(const std::vector<arma::mat>& data,
                                const std::vector<std::vector<int>>& labels,
                                const std::vector<std::vector<arma::vec>>& local_params);

  void update_stick_weights();
  void update_gamma();
  void update_local_alphas(const std::vector<std::vector<int>>& labels);

  // Accessors
  Rcpp::List get_state() const;
  void set_state(const Rcpp::List& state);

  // Friend class declaration to allow access to private members
  friend class HierarchicalMVNormalRunner;
};

// MCMCRunner specialized for hierarchical MVNormal
class HierarchicalMVNormalRunner {
private:
  std::vector<arma::mat> data_list;
  std::unique_ptr<HierarchicalMVNormalMixing> hdp_model;

  // MCMC state for each group
  std::vector<std::vector<int>> cluster_labels;
  std::vector<std::vector<arma::vec>> cluster_params;
  std::vector<int> n_clusters;

  // MCMC parameters
  int n_iter;
  int n_burn;
  int thin;
  bool update_prior;
  bool show_progress;

  // Storage for samples
  std::vector<Rcpp::List> state_samples;

public:
  HierarchicalMVNormalRunner(const std::vector<arma::mat>& data_list,
                             const Rcpp::List& hdp_params,
                             const Rcpp::List& mcmc_params);

  Rcpp::List run();

private:
  void initialize_clusters();
  void update_cluster_assignments_algorithm8(int group_idx);
  void store_iteration(int iter);
};

} // namespace dirichletprocess

#endif
