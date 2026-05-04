// inst/include/DirichletProcessBase.h
#ifndef DIRICHLETPROCESS_BASE_H
#define DIRICHLETPROCESS_BASE_H

#include <RcppArmadillo.h>

namespace dp {

// Forward declarations
class MixingDistribution;
// class DirichletProcess; // Not needed here, defined below

// Base MixingDistribution class
class MixingDistribution {
public:
  MixingDistribution();
  virtual ~MixingDistribution();

  // Core properties
  std::string distribution;
  bool conjugate;
  Rcpp::RObject priorParameters;
  Rcpp::RObject mhStepSize;
  Rcpp::RObject hyperPriorParameters;

  // Virtual methods
  virtual Rcpp::NumericVector likelihood(const arma::vec& x, const Rcpp::List& theta) const {
    Rcpp::stop("Base MixingDistribution::likelihood() called - must be overridden");
    return Rcpp::NumericVector();
  }
  virtual Rcpp::List priorDraw(int n) const {
    Rcpp::stop("Base MixingDistribution::priorDraw() called - must be overridden");
    return Rcpp::List();
  }
  virtual Rcpp::List posteriorDraw(const arma::mat& x, int n = 1) const {
    Rcpp::stop("Base MixingDistribution::posteriorDraw() called - must be overridden");
    return Rcpp::List();
  }
  virtual Rcpp::List toR() const;
};

// Base DirichletProcess class
class DirichletProcess {
public:
  DirichletProcess();                   // Default constructor
  DirichletProcess(SEXP r_dpObj);       // Constructor from R SEXP
  virtual ~DirichletProcess();

  // Common properties
  arma::mat data;
  int n;                              // Number of data points
  double alpha;
  Rcpp::RObject alphaPriorParameters;   // Using RObject to allow for NULL or specific types

  // Added essential members
  bool verbose;
  int mhDraws;                        // Metropolis-Hastings draws

  // Cluster-related information (common to most DP types)
  arma::uvec clusterLabels;           // 0-indexed in C++, 1-indexed in R
  arma::uvec pointsPerCluster;
  int numberClusters;
  Rcpp::List clusterParameters;       // List of parameters for each cluster

  SEXP rObject;                       // Store the original R SEXP for reference if needed

  // Virtual methods for MCMC algorithms
  virtual void clusterComponentUpdate() {
    Rcpp::warning("Base DirichletProcess::clusterComponentUpdate() called - should be overridden");
  }
  virtual void clusterParameterUpdate() {
    Rcpp::warning("Base DirichletProcess::clusterParameterUpdate() called - should be overridden");
  }
  virtual void updateAlpha() {
    Rcpp::warning("Base DirichletProcess::updateAlpha() called - should be overridden or base implemented");
  }
  virtual void updateG0() { /* Default no-op */ };

  virtual void fit(int iterations, bool use_progress_bar); // Can have a base implementation

  virtual MixingDistribution* getMixingDistribution() {
    Rcpp::stop("Base DirichletProcess::getMixingDistribution() called. Derived class must implement.");
    return nullptr;
  };

  // Conversion methods
  virtual Rcpp::List toR() const;
  // Static factory fromR might be better in a central factory function if creating various DP types
  // For now, ensure derived classes handle their specific 'fromR' if needed, or rely on constructor.
  // static DirichletProcess* fromR(const Rcpp::List& rObj); // Removed as it calls undefined createDPFromR
};

} // namespace dp

#endif
