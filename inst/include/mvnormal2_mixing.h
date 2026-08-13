#ifndef MVNORMAL2_MIXING_H
#define MVNORMAL2_MIXING_H

#include "mixing_distribution_base.h"
#include <RcppArmadillo.h>

namespace dirichletprocess {

class MVNormal2Mixing : public MixingDistribution {
private:
  // Prior parameters for MVNormal2 semi-conjugate distribution
  arma::mat mu0;      // Prior mean (as matrix)
  arma::mat sigma0;   // Prior covariance for mu
  arma::mat phi0;     // Prior scale matrix for Wishart
  double nu0;         // Prior degrees of freedom
  int d;              // Dimension
  int mh_draws;

  // Helper function to ensure matrix symmetry
  arma::mat ensureSymmetric(const arma::mat& A) const {
    return 0.5 * (A + A.t());
  }

public:
  MVNormal2Mixing(const arma::mat& mu0, const arma::mat& sigma0,
                  const arma::mat& phi0, double nu0, int mh_draws = 250);

  double log_likelihood(const arma::vec& data_point,
                        const arma::vec& params) const override;

  arma::vec posterior_draw(const arma::mat& cluster_data,
                           const arma::vec& prior_params) const override;

  arma::vec prior_draw() const override;

  int param_dim() const override;
  
  bool is_conjugate() const override { return false; }

  // Helper methods
  arma::vec flatten_params(const arma::vec& mu, const arma::mat& Sigma) const;
  void unflatten_params(const arma::vec& params, arma::vec& mu, arma::mat& Sigma) const;
};

} // namespace dirichletprocess

#endif
