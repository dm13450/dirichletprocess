#ifndef WEIBULL_MIXING_H
#define WEIBULL_MIXING_H

#include "mixing_distribution_base.h"
#include <RcppArmadillo.h>

namespace dirichletprocess {

class WeibullMixing : public MixingDistribution {
private:
  // Prior parameters
  double phi;         // Upper bound for shape parameter alpha
  double alpha0;      // Shape parameter for Gamma prior on 1/lambda
  double beta0;       // Rate parameter for Gamma prior on 1/lambda

  // Hyperprior parameters
  double hyper_a1;    // For phi (Pareto distribution)
  double hyper_a2;    // For phi (Pareto distribution)
  double hyper_b1;    // For beta0 (Gamma distribution)
  double hyper_b2;    // For beta0 (Gamma distribution)

  // MH parameters
  double mh_step_alpha;  // Step size for alpha proposals
  int mh_draws;          // Number of MH iterations

public:
  WeibullMixing(double phi, double alpha0, double beta0,
                double hyper_a1 = 6.0, double hyper_a2 = 2.0,
                double hyper_b1 = 1.0, double hyper_b2 = 0.5,
                double mh_step_alpha = 0.1, int mh_draws = 100);

  // Override virtual methods from base class
  double log_likelihood(const arma::vec& data_point,
                        const arma::vec& params) const override;

  arma::vec posterior_draw(const arma::mat& cluster_data,
                           const arma::vec& prior_params) const override;

  arma::vec prior_draw() const override;

  int param_dim() const override { return 2; }
  
  bool is_conjugate() const override { return false; }

  // Weibull-specific methods
  double log_prior_density(const arma::vec& params) const;
  arma::vec mh_parameter_proposal(const arma::vec& current_params) const;
  void update_hyperparameters(const std::vector<arma::vec>& all_params);

private:
  // Helper function for Pareto quantile
  double qpareto(double p, double xm, double alpha) const;
};

} // namespace dirichletprocess

#endif // WEIBULL_MIXING_H
