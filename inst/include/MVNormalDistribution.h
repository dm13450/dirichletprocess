// inst/include/MVNormalDistribution.h
#ifndef MVNORMAL_DISTRIBUTION_H
#define MVNORMAL_DISTRIBUTION_H

#include "DirichletProcessBase.h"
#include "mixing_distribution_base.h"
#include <RcppArmadillo.h>
#include <string>
#include <memory>

namespace dp {

// Enum for covariance model types
enum class CovarianceModel {
  FULL,    // Full covariance matrix (default)
  E,       // Equal variance (univariate)
  V,       // Variable variance (univariate)
  EII,     // Spherical, equal volume
  VII,     // Spherical, unequal volume
  EEI,     // Diagonal, equal volume and shape
  VEI,     // Diagonal, varying volume, equal shape
  EVI,     // Diagonal, equal volume, varying shape
  VVI      // Diagonal, varying volume and shape
};

// Helper function to ensure matrix symmetry with numerical stability
inline arma::mat ensureSymmetric(const arma::mat& M) {
  if (M.n_rows != M.n_cols) {
    Rcpp::stop("Matrix must be square to ensure symmetry");
  }
  
  // Check if already symmetric within tolerance to avoid unnecessary operations
  double max_asymmetry = arma::abs(M - M.t()).max();
  if (max_asymmetry < 1e-10) {  // Stricter tolerance to prevent warnings
    return M;  // Already symmetric enough
  }

  // Initialize symmetric matrix to avoid uninitialized variable warning
  arma::mat symmetric = 0.5 * (M + M.t());

  // Check for NaN or infinite values
  if (!symmetric.is_finite()) {
    return arma::eye<arma::mat>(M.n_rows, M.n_cols);
  }
  
  // Ensure positive definiteness by regularization if needed
  // Use less expensive method: check diagonal elements first
  bool needs_regularization = false;
  for (arma::uword i = 0; i < symmetric.n_rows; ++i) {
    if (symmetric(i, i) <= 1e-10) {  // Stricter threshold
      needs_regularization = true;
      break;
    }
  }
  
  if (needs_regularization) {
    // Try Cholesky decomposition first (faster than eigendecomposition)
    arma::mat L;
    bool is_pd = arma::chol(L, symmetric);
    if (!is_pd) {
      // Fall back to eigenvalue regularization
      arma::vec eigenvals;
      arma::mat eigenvecs;
      if (arma::eig_sym(eigenvals, eigenvecs, symmetric)) {
        double min_eigenval = eigenvals.min();
        if (min_eigenval <= 1e-10) {  // Stricter eigenvalue threshold
          double regularization = std::max(1e-8, -min_eigenval + 1e-8);  // Larger regularization
          symmetric += regularization * arma::eye<arma::mat>(M.n_rows, M.n_cols);
        }
      } else {
        // Eigendecomposition failed, use simple regularization
        symmetric += 1e-8 * arma::eye<arma::mat>(M.n_rows, M.n_cols);
      }
    }
  }
  
  return symmetric;
}

class MVNormalMixingDistribution : public MixingDistribution {
private:
  // Prior parameters
  arma::vec mu0;
  double kappa0;
  arma::mat Lambda;
  double nu;

  // Covariance model
  CovarianceModel covModel;

  // Helper functions for different covariance structures
  arma::mat constructCovarianceMatrix(const arma::vec& params, int d) const;
  arma::vec extractCovarianceParams(const arma::mat& sigma) const;
  // Remove getNumCovParams from here

public:
  MVNormalMixingDistribution(const Rcpp::List& priorParams);
  ~MVNormalMixingDistribution();

  // Core functions
  Rcpp::NumericVector likelihood(const arma::vec& x, const Rcpp::List& theta) const override;
  Rcpp::List posteriorParameters(const arma::mat& x) const;
  Rcpp::List priorDraw(int n) const override;
  Rcpp::List posteriorDraw(const arma::mat& x, int n) const override;
  Rcpp::NumericVector predictive(const arma::mat& x) const;

  // MVNormal specific likelihood
  arma::vec mvnLikelihood(const arma::mat& x, const arma::vec& mu,
                          const arma::mat& sigma) const;

  // Static methods for R interface
  static Rcpp::List priorDrawStatic(const Rcpp::List& priorParams, int n);
  static Rcpp::List posteriorDrawStatic(const Rcpp::List& priorParams,
                                        const arma::mat& x, int n);

  // Get covariance model
  CovarianceModel getCovarianceModel() const { return covModel; }

  // Add this method as public
  int getNumCovParams(int d) const;
};

// Conjugate MVNormal Dirichlet Process
class ConjugateMVNormalDP {
private:
  std::unique_ptr<MVNormalMixingDistribution> mixingDistribution;
  arma::mat data;
  arma::uvec clusterLabels;
  int numberClusters;
  Rcpp::List clusterParameters;
  arma::uvec pointsPerCluster;
  double alpha;
  Rcpp::List alphaPriorParameters;
  int n;
  Rcpp::NumericVector predictiveArray;

  // Internal methods
  void initialisePredictive();
  void clusterComponentUpdate();
  void clusterParameterUpdate();
  void updateAlpha();
  Rcpp::List clusterLabelChange(int i, int newLabel, int currentLabel);

public:
  ConjugateMVNormalDP();
  ~ConjugateMVNormalDP();

  void initialize(const Rcpp::List& dpObj);
  Rcpp::List updateClusterComponents();
  Rcpp::List updateClusterParameters();
};

// Export functions
Rcpp::List conjugate_mvnormal_cluster_component_update_cpp(const Rcpp::List& dpObj);
Rcpp::List conjugate_mvnormal_cluster_parameter_update_cpp(const Rcpp::List& dpObj);

} // namespace dp

#endif
