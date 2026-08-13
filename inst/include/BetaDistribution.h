// inst/include/BetaDistribution.h
#ifndef BETA_DISTRIBUTION_H
#define BETA_DISTRIBUTION_H

#include "DirichletProcessBase.h"
#include <memory>

namespace dp {

class BetaMixingDistribution : public MixingDistribution {
public:
  BetaMixingDistribution(const Rcpp::NumericVector& priorParams);
  virtual ~BetaMixingDistribution();

  double maxT; // Upper bound for the Beta distribution

  // Implement required virtual methods
  Rcpp::NumericVector likelihood(const arma::vec& x, const Rcpp::List& theta) const override;
  Rcpp::List priorDraw(int n) const override;
  Rcpp::List posteriorDraw(const arma::mat& x, int n = 1) const override;

  // Specific methods for Beta distribution
  double priorDensity(const Rcpp::List& theta) const;
  Rcpp::List mhParameterProposal(const Rcpp::List& oldParams) const;
  void updatePriorParameters(const Rcpp::List& clusterParameters, int n = 1);

  // This method is now public
  Rcpp::List metropolisHastings(const arma::mat& x, const Rcpp::List& startPos, int noDraws) const;

  // Static methods for direct testing
  static Rcpp::List priorDrawStatic(const Rcpp::NumericVector& priorParams, double maxT, int n);
  static Rcpp::List posteriorDrawStatic(const Rcpp::NumericVector& priorParams, double maxT,
                                        const Rcpp::NumericVector& mhStepSize,
                                        const arma::mat& x, int n, int mhDraws = 250);
  static Rcpp::NumericVector likelihoodStatic(const arma::vec& x, double mu, double nu, double maxT);

private:
  // Helper methods (this block might now be empty, which is fine)
};

class NonConjugateBetaDP : public DirichletProcess {
public:
  NonConjugateBetaDP();
  virtual ~NonConjugateBetaDP();

  std::unique_ptr<BetaMixingDistribution> mixingDistribution;

  // Cluster information
  arma::uvec clusterLabels;
  arma::uvec pointsPerCluster;
  int numberClusters;
  Rcpp::List clusterParameters;
  int m; // Number of auxiliary parameters

  // Implementation of core MCMC methods
  void clusterComponentUpdate() override;
  void clusterParameterUpdate() override;
  void updateAlpha() override;

  // Additional methods specific to non-conjugate Beta
  Rcpp::List clusterLabelChange(int i, int newLabel, int currentLabel, const Rcpp::List& aux);
  Rcpp::List toR() const override;

  // Override getMixingDistribution
  MixingDistribution* getMixingDistribution() override { return mixingDistribution.get(); }
};

} // namespace dp

#endif
