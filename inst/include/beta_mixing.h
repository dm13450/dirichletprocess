#ifndef BETA_MIXING_H
#define BETA_MIXING_H

#include "mixing_distribution_base.h"
#include <RcppArmadillo.h>

namespace dirichletprocess {

class BetaMixing : public MixingDistribution {
private:
  double alpha0;  // Prior shape parameter for alpha
  double beta0;   // Prior shape parameter for beta
  double maxT;    // Upper bound of Beta distribution (default 1)
  double mh_step_mu;
  double mh_step_nu;
  int mh_draws;

public:
  BetaMixing(double alpha0, double beta0, double maxT = 1.0,
             double mh_step_mu = 1.0, double mh_step_nu = 1.0,
             int mh_draws = 250);

  // Override virtual methods from base class
  double log_likelihood(const arma::vec& data_point,
                        const arma::vec& params) const override;

  arma::vec posterior_draw(const arma::mat& cluster_data,
                           const arma::vec& prior_params) const override;

  arma::vec prior_draw() const override;

  int param_dim() const override { return 2; }
  
  bool is_conjugate() const override { return false; }
};

} // namespace dirichletprocess

#endif // BETA_MIXING_H
