#ifndef MVNORMAL_COVARIANCE_MIXING_H
#define MVNORMAL_COVARIANCE_MIXING_H

#include "mixing_distribution_base.h"
#include "MVNormalDistribution.h"
#include <RcppArmadillo.h>

namespace dirichletprocess {

// Enhanced MVNormal mixing distribution supporting all covariance models
class MVNormalCovarianceMixing : public MixingDistribution {
private:
  // Use the existing MVNormalMixingDistribution from MVNormalDistribution.h
  std::unique_ptr<dp::MVNormalMixingDistribution> mvn_dist;
  std::string covModel;
  int d; // Dimension
  
  // Helper functions
  arma::vec extractParameters(const Rcpp::List& theta, int cluster_idx = 0) const;
  Rcpp::List createClusterParameters(const arma::vec& mu, const arma::vec& sig) const;

public:
  MVNormalCovarianceMixing(const arma::vec& mu0, double kappa0,
                          const arma::mat& Lambda, double nu, 
                          const std::string& covModel);

  double log_likelihood(const arma::vec& data_point,
                        const arma::vec& params) const override;

  arma::vec posterior_draw(const arma::mat& cluster_data,
                           const arma::vec& prior_params) const override;

  arma::vec prior_draw() const override;

  int param_dim() const override;
  
  bool is_conjugate() const override { return true; }
  
  double predictive_probability(const arma::vec& data_point) const override;

  // Helper methods for parameter conversion
  arma::vec flattenParams(const arma::vec& mu, const arma::vec& sig) const;
  void unflattenParams(const arma::vec& params, arma::vec& mu, arma::vec& sig) const;
};

} // namespace dirichletprocess

#endif
