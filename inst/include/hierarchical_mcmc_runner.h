#ifndef HIERARCHICAL_MCMC_RUNNER_H
#define HIERARCHICAL_MCMC_RUNNER_H

#include "mcmc_runner.h"
#include "hierarchical_beta_mixing.h"
#include <vector>

namespace dirichletprocess {

class HierarchicalMCMCRunner {
private:
  // Multiple datasets
  std::vector<arma::mat> datasets;

  // Individual MCMC runners for each dataset
  std::vector<std::unique_ptr<MCMCRunner>> runners;

  // Shared hierarchical mixing distribution
  std::unique_ptr<HierarchicalBetaMixing> hierarchical_mixing_dist;

  // MCMC parameters
  int n_iter;
  int n_burn;
  int thin;
  bool update_prior;

  // Storage for hierarchical results
  std::vector<double> gamma_samples;
  std::vector<std::vector<arma::vec>> global_param_samples;

public:
  HierarchicalMCMCRunner(const std::vector<arma::mat>& datasets,
                         const Rcpp::List& mixing_dist_params,
                         const Rcpp::List& mcmc_params);

  // Main hierarchical MCMC loop
  Rcpp::List run();

private:
  // Hierarchical MCMC steps
  void update_local_clusters();
  void update_global_parameters();
  void update_gamma();
  void propagate_g0_to_local();
  void store_iteration(int iter);
};

} // namespace dirichletprocess

#endif
