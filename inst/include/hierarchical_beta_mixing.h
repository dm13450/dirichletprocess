#ifndef HIERARCHICAL_BETA_MIXING_H
#define HIERARCHICAL_BETA_MIXING_H

#include "mixing_distribution_base.h"
#include <RcppArmadillo.h>

namespace dirichletprocess {

class HierarchicalBetaMixing : public MixingDistribution {
private:
  // Prior parameters for the base distribution G0
  double alpha0;  // Shape parameter for mu prior
  double beta0;   // Shape parameter for tau prior
  double maxT;    // Maximum value for Beta support [0, maxT]

  // Hierarchical structure
  arma::vec global_stick_weights;  // Global stick-breaking weights (beta_k)
  std::vector<arma::vec> global_params;  // Global parameters (theta_k)
  double gamma;  // Concentration parameter for G0

  // Prior parameters for gamma
  double gamma_prior_shape;
  double gamma_prior_rate;

  // Number of auxiliary parameters for Algorithm 8 (reserved for future use)
  [[maybe_unused]] int m_auxiliary;

public:
  HierarchicalBetaMixing(double alpha0, double beta0, double maxT,
                         double gamma_prior_shape = 2.0,
                         double gamma_prior_rate = 4.0,
                         int m_auxiliary = 3);

  // Core interface methods
  double log_likelihood(const arma::vec& data_point,
                        const arma::vec& params) const override;

  arma::vec posterior_draw(const arma::mat& cluster_data,
                           const arma::vec& prior_params) const override;

  arma::vec prior_draw() const override;

  int param_dim() const override { return 2; }  // [mu, tau]
  
  bool is_conjugate() const override { return false; }

  // Hierarchical-specific methods
  void update_global_parameters(const std::vector<arma::mat>& all_cluster_data,
                                const std::vector<arma::vec>& all_cluster_params);

  void update_global_stick_weights(int n_global_clusters);

  void update_gamma(int n_unique_clusters, int n_total_obs);

  arma::vec draw_from_g0() const;

  // Getters for hierarchical structure
  const arma::vec& get_global_weights() const { return global_stick_weights; }
  const std::vector<arma::vec>& get_global_params() const { return global_params; }
  double get_gamma() const { return gamma; }
};

} // namespace dirichletprocess

#endif
