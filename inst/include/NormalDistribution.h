// inst/include/NormalDistribution.h
#ifndef NORMAL_DISTRIBUTION_H
#define NORMAL_DISTRIBUTION_H

#include "DirichletProcessBase.h"

namespace dp {

class NormalMixingDistribution : public MixingDistribution {
public:
  NormalMixingDistribution(const Rcpp::NumericVector& priorParams);
  virtual ~NormalMixingDistribution();

  // Implement required virtual methods
  Rcpp::NumericVector likelihood(const arma::vec& x, const Rcpp::List& theta) const override;
  Rcpp::List priorDraw(int n) const override;
  Rcpp::List posteriorDraw(const arma::mat& x, int n = 1) const override;

  // Specific methods for Normal distribution
  Rcpp::NumericMatrix posteriorParameters(const arma::mat& x) const;
  Rcpp::NumericVector predictive(const arma::vec& x) const;

  // Export these methods for direct testing
  static Rcpp::List priorDrawStatic(const Rcpp::NumericVector& priorParams, int n);
  static Rcpp::List posteriorDrawStatic(const Rcpp::NumericVector& priorParams, const arma::mat& x, int n);
};

class ConjugateNormalDP : public DirichletProcess {
public:
  ConjugateNormalDP();
  virtual ~ConjugateNormalDP();

  NormalMixingDistribution* mixingDistribution;

  // Cluster information
  arma::uvec clusterLabels;
  arma::uvec pointsPerCluster;
  int numberClusters;
  Rcpp::List clusterParameters;
  arma::vec predictiveArray;

  // Implementation of core MCMC methods
  void clusterComponentUpdate() override;
  void clusterParameterUpdate() override;
  void updateAlpha() override;

  // Additional methods specific to conjugate normal
  Rcpp::List clusterLabelChange(int i, int newLabel, int currentLabel);
  void initialisePredictive();
};

} // namespace dp

#endif
