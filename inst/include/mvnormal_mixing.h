#ifndef MVNORMAL_MIXING_H
#define MVNORMAL_MIXING_H

#include "mixing_distribution_base.h"
#include <RcppArmadillo.h>

namespace dirichletprocess {

class MVNormalMixing : public MixingDistribution {
private:
  // Prior parameters for Normal-Wishart distribution
  arma::vec mu0;      // Prior mean
  double kappa0;      // Prior precision scaling
  arma::mat Lambda;   // Prior scale matrix (precision)
  double nu;          // Prior degrees of freedom
  int d;              // Dimension

  // Helper function to ensure matrix symmetry
  arma::mat ensureSymmetric(const arma::mat& A) const {
    return 0.5 * (A + A.t());
  }

public:
  MVNormalMixing(const arma::vec& mu0, double kappa0,
                 const arma::mat& Lambda, double nu);

  double log_likelihood(const arma::vec& data_point,
                        const arma::vec& params) const override;

  arma::vec posterior_draw(const arma::mat& cluster_data,
                           const arma::vec& prior_params) const override;

  arma::vec prior_draw() const override;

  int param_dim() const override;
  
  bool is_conjugate() const override { return true; }
  
  double predictive_probability(const arma::vec& data_point) const override;

  // Helper methods
  arma::vec flatten_params(const arma::vec& mu, const arma::mat& Sigma) const;
  void unflatten_params(const arma::vec& params, arma::vec& mu, arma::mat& Sigma) const;
};

} // namespace dirichletprocess

#endif
