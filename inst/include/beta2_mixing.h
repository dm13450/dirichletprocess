#ifndef BETA2_MIXING_H
#define BETA2_MIXING_H

#include "mixing_distribution_base.h"
#include <RcppArmadillo.h>

namespace dirichletprocess {

class Beta2Mixing : public MixingDistribution {
private:
  // Prior parameter for Pareto distribution
  double gamma_prior;

  // Maximum value for Beta distribution
  double maxT;

  // MH parameters
  arma::vec mh_step_size;
  int mh_draws;

public:
  Beta2Mixing(double gamma_prior = 2.0, double maxT = 1.0,
              const arma::vec& mh_step_size = arma::vec({1.0, 1.0}),
              int mh_draws = 250);

  // Override virtual methods from base class
  double log_likelihood(const arma::vec& data_point,
                        const arma::vec& params) const override;

  arma::vec posterior_draw(const arma::mat& cluster_data,
                           const arma::vec& prior_params) const override;

  arma::vec prior_draw() const override;

  int param_dim() const override { return 2; }  // [mu, nu]
  
  bool is_conjugate() const override { return false; }

  // Beta2-specific methods
  double log_prior_density(const arma::vec& params) const;
  arma::vec mh_parameter_proposal(const arma::vec& current_params) const;

private:
  // Helper function for Pareto distribution
  double rpareto(double xm, double alpha) const;
  double dpareto(double x, double xm, double alpha) const;
};

} // namespace dirichletprocess

#endif // BETA2_MIXING_H
