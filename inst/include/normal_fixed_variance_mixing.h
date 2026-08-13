#ifndef NORMAL_FIXED_VARIANCE_MIXING_H
#define NORMAL_FIXED_VARIANCE_MIXING_H

#include "mixing_distribution_base.h"
#include <RcppArmadillo.h>

namespace dirichletprocess {

class NormalFixedVarianceMixing : public MixingDistribution {
private:
  // Prior parameters for the mean
  double mu0;      // Prior mean
  double sigma0;   // Prior standard deviation

  // Fixed variance of the model
  double sigma;

public:
  NormalFixedVarianceMixing(double mu0 = 0.0, double sigma0 = 1.0, double sigma = 1.0);

  // Override virtual methods from base class
  double log_likelihood(const arma::vec& data_point,
                        const arma::vec& params) const override;

  arma::vec posterior_draw(const arma::mat& cluster_data,
                           const arma::vec& prior_params) const override;

  arma::vec prior_draw() const override;

  int param_dim() const override { return 1; }  // Only mu is unknown
  
  bool is_conjugate() const override { return true; }

  // Conjugate specific methods
  arma::vec posterior_parameters(const arma::mat& cluster_data) const;
  double predictive_density(double x) const;
  double predictive_probability(const arma::vec& data_point) const override;
};

} // namespace dirichletprocess

#endif // NORMAL_FIXED_VARIANCE_MIXING_H
