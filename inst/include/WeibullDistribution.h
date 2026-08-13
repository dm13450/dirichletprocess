// inst/include/WeibullDistribution.h
#ifndef WEIBULL_DISTRIBUTION_H
#define WEIBULL_DISTRIBUTION_H

#include "dirichletprocess.h"
#include <map>

namespace dp {

class WeibullMixingDistribution : public MixingDistribution {
public:
  WeibullMixingDistribution(const Rcpp::NumericVector& priorParams,
                            const Rcpp::NumericVector& mhStepSize,
                            const Rcpp::NumericVector& hyperPriorParams = Rcpp::NumericVector::create());
  virtual ~WeibullMixingDistribution();

  // Implement required virtual methods
  Rcpp::NumericVector likelihood(const arma::vec& x, const Rcpp::List& theta) const override;
  Rcpp::List priorDraw(int n) const override;
  Rcpp::List posteriorDraw(const arma::mat& x, int n = 1) const override;

  // Weibull specific methods
  Rcpp::NumericVector priorDensity(const Rcpp::List& theta) const;
  Rcpp::List mhParameterProposal(const Rcpp::List& oldParams) const;
  void updatePriorParameters(const Rcpp::List& clusterParameters, int n = 1);

private:
  double qpareto(double p, double xm, double alpha) const;
};

class NonConjugateWeibullDP : public DirichletProcess {
public:
  NonConjugateWeibullDP();
  virtual ~NonConjugateWeibullDP();

  WeibullMixingDistribution* mixingDistribution;

  // Data and cluster information
  arma::mat data;
  int n;  // Number of data points
  double alpha;  // Concentration parameter
  Rcpp::NumericVector alphaPriorParameters;
  arma::uvec clusterLabels;
  arma::uvec pointsPerCluster;
  int numberClusters;
  Rcpp::List clusterParameters;
  int m; // Number of auxiliary parameters

  // Implementation of core MCMC methods
  void clusterComponentUpdate() override;
  void clusterParameterUpdate() override;
  void updateAlpha() override;

  // Additional methods
  Rcpp::List clusterLabelChange(int i, int newLabel, int currentLabel, const Rcpp::List& aux);
};

} // namespace dp

#endif
