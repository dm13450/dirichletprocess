// inst/include/ExponentialDistribution.h
#ifndef EXPONENTIAL_DISTRIBUTION_H
#define EXPONENTIAL_DISTRIBUTION_H

#include "DirichletProcessBase.h"
#include <RcppArmadillo.h>
#include <cmath>
#include <vector>

namespace dp {

// Forward declaration
class ExponentialMixingDistribution;

// Optimized Exponential Mixing Distribution class
class ExponentialMixingDistribution : public MixingDistribution {
public:
  // Constructor and destructor
  ExponentialMixingDistribution(const Rcpp::NumericVector& priorParams);
  virtual ~ExponentialMixingDistribution();

  // Override virtual methods from base class
  Rcpp::NumericVector likelihood(const arma::vec& x, const Rcpp::List& theta) const override;
  Rcpp::List priorDraw(int n) const override;
  Rcpp::List posteriorDraw(const arma::mat& x, int n = 1) const override;

  // Exponential-specific methods
  Rcpp::NumericVector predictive(const arma::vec& x) const;
  Rcpp::NumericMatrix posteriorParameters(const arma::mat& x) const;

  // Inline fast likelihood calculation for maximum performance
  inline double exponential_pdf(double x, double lambda) const {
    return (x >= 0 && lambda > 0) ? (lambda * std::exp(-lambda * x)) : 0.0;
  }

  // Inline fast log-likelihood
  inline double exponential_log_pdf(double x, double lambda) const {
    return (x >= 0 && lambda > 0) ? (std::log(lambda) - lambda * x) : -INFINITY;
  }
};

// Optimized Conjugate Exponential Dirichlet Process class
class ConjugateExponentialDP : public DirichletProcess {
public:
  // Constructor and destructor
  ConjugateExponentialDP(Rcpp::List dpObj);
  virtual ~ConjugateExponentialDP();

  // Mixing distribution
  ExponentialMixingDistribution* mixingDistribution;

  // Cluster information - redeclare for clarity and to ensure proper memory layout
  arma::uvec clusterLabels;      // 0-indexed cluster assignments
  arma::uvec pointsPerCluster;   // Number of points in each cluster
  int numberClusters;            // Current number of active clusters
  Rcpp::List clusterParameters;  // List containing parameter arrays
  arma::vec predictiveArray;     // Predictive probabilities for each data point

  // Override core MCMC methods from base class
  void clusterComponentUpdate() override;
  void clusterParameterUpdate() override;
  void updateAlpha() override;

  // Additional methods specific to exponential
  void initialisePredictive();
  Rcpp::List clusterLabelChange(int i, int newLabel, int currentLabel);

  // Optimized internal methods for performance
  void clusterLabelChangeOptimized(int i, int newLabel, int currentLabel);

  // Inline accessor methods for fast parameter access
  inline double getClusterLambda(int cluster) const {
    const Rcpp::NumericVector& lambda_vec = clusterParameters[0];
    return (cluster < lambda_vec.size()) ? lambda_vec[cluster] : 0.0;
  }

  inline void setClusterLambda(int cluster, double lambda) {
    Rcpp::NumericVector lambda_vec = Rcpp::clone(Rcpp::as<Rcpp::NumericVector>(clusterParameters[0]));
    if (cluster < lambda_vec.size()) {
      lambda_vec[cluster] = lambda;
      clusterParameters[0] = lambda_vec;
    }
  }

  // Fast likelihood calculation for a single data point and cluster
  inline double calculateLikelihood(double x, int cluster) const {
    const double lambda = getClusterLambda(cluster);
    return (x >= 0 && lambda > 0) ? (lambda * std::exp(-lambda * x)) : 0.0;
  }

  // Pre-compute and cache cluster parameters for efficiency
  std::vector<double> cacheClusterLambdas() const {
    const Rcpp::NumericVector& lambda_vec = clusterParameters[0];
    std::vector<double> cached(numberClusters);
    for (int i = 0; i < numberClusters; ++i) {
      cached[i] = lambda_vec[i];
    }
    return cached;
  }
};

// Global inline utility functions for maximum performance
namespace exponential_utils {

// Fast exponential PDF calculation
inline double fast_exponential_pdf(double x, double lambda) {
  return (x >= 0 && lambda > 0) ? (lambda * std::exp(-lambda * x)) : 0.0;
}

// Fast log PDF calculation
inline double fast_exponential_log_pdf(double x, double lambda) {
  return (x >= 0 && lambda > 0) ? (std::log(lambda) - lambda * x) : -INFINITY;
}

// Vectorized exponential PDF for multiple data points
inline void fast_exponential_pdf_vec(const double* x, int n, double lambda, double* result) {
  if (lambda <= 0) {
    for (int i = 0; i < n; ++i) {
      result[i] = 0.0;
    }
  } else {
    for (int i = 0; i < n; ++i) {
      result[i] = (x[i] >= 0) ? (lambda * std::exp(-lambda * x[i])) : 0.0;
    }
  }
}

// Fast sampling from gamma distribution (for posterior draws)
inline double fast_gamma_draw(double shape, double rate) {
  return R::rgamma(shape, 1.0 / rate);
}

} // namespace exponential_utils

} // namespace dp

#endif // EXPONENTIAL_DISTRIBUTION_H
