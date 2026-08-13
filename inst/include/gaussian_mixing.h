// inst/include/gaussian_mixing.h
#ifndef GAUSSIAN_MIXING_H
#define GAUSSIAN_MIXING_H

#include "mixing_distribution_base.h"

namespace dirichletprocess {

class GaussianMixing : public MixingDistribution {
private:
  // Prior parameters (Normal-Inverse-Gamma)
  double mu0;      // prior mean
  double kappa0;   // prior precision scaling
  double alpha0;   // shape parameter
  double beta0;    // rate parameter

public:
  GaussianMixing(double mu0, double kappa0, double alpha0, double beta0);

  double log_likelihood(const arma::vec& data_point,
                        const arma::vec& params) const override;

  arma::vec posterior_draw(const arma::mat& cluster_data,
                           const arma::vec& prior_params) const override;

  arma::vec prior_draw() const override;

  int param_dim() const override { return 2; } // mean and variance
  
  bool is_conjugate() const override { return true; }
  
  double predictive_probability(const arma::vec& data_point) const override;
};

} // namespace dirichletprocess

#endif
