// inst/include/ExponentialDistribution.h
#ifndef EXPONENTIAL_DISTRIBUTION_H
#define EXPONENTIAL_DISTRIBUTION_H

#include "mixing_distribution_base.h"
#include <RcppArmadillo.h>

namespace dirichletprocess {

class ExponentialMixing : public MixingDistribution {
private:
  double alpha0;  // Shape parameter for Gamma prior
  double beta0;   // Rate parameter for Gamma prior

public:
  ExponentialMixing(double alpha0, double beta0)
    : alpha0(alpha0), beta0(beta0) {}

  // Core interface methods
  double log_likelihood(const arma::vec& data_point,
                        const arma::vec& params) const override;

  arma::vec posterior_draw(const arma::mat& cluster_data,
                           const arma::vec& prior_params) const override;

  arma::vec prior_draw() const override;

  int param_dim() const override { return 1; }
  
  bool is_conjugate() const override { return true; }
  
  double predictive_probability(const arma::vec& data_point) const override;

  // Additional helper methods
  arma::vec posterior_parameters(const arma::mat& cluster_data) const;
  double predictive_density(double x, const arma::mat& cluster_data) const;
};

} // namespace dirichletprocess

#endif // EXPONENTIAL_DISTRIBUTION_H
