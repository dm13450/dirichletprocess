// inst/include/MVNormal2Distribution.h
#ifndef MVNORMAL2_DISTRIBUTION_H
#define MVNORMAL2_DISTRIBUTION_H

#include <RcppArmadillo.h>
#include "dirichletprocess.h" // Assumes MixingDistribution is also included/forward-declared via this or another header

namespace dp {

class MVNormal2MixingDistribution : public MixingDistribution {
public:
  MVNormal2MixingDistribution(const Rcpp::List& priorParams);
  virtual ~MVNormal2MixingDistribution();

  std::string distribution;
  bool conjugate;
  Rcpp::List priorParameters;
  Rcpp::NumericVector mhStepSize;

  // Prior parameters specific to MVNormal2
  arma::rowvec mu0;
  arma::mat sigma0;
  arma::mat phi0;
  double nu0;

  // Implement required virtual methods
  Rcpp::NumericVector likelihood(const arma::vec& x, const Rcpp::List& theta) const override;
  Rcpp::List priorDraw(int n) const override;
  Rcpp::List posteriorDraw(const arma::mat& x, int n = 1) const override;
  Rcpp::List toR() const override; // <<< Added declaration
};

class NonConjugateMVNormal2DP : public DirichletProcess {
public:
  NonConjugateMVNormal2DP();
  virtual ~NonConjugateMVNormal2DP();

  MVNormal2MixingDistribution* mixingDistribution;

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

  // Add the getMixingDistribution override
  MixingDistribution* getMixingDistribution() override;
  Rcpp::List toR() const override; // <<< Added declaration

  // Additional methods
  Rcpp::List clusterLabelChange(int i, int newLabel, int currentLabel, const Rcpp::List& aux);
};

} // namespace dp

#endif // MVNORMAL2_DISTRIBUTION_H
