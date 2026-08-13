#include "mvnormal_covariance_mixing.h"
#include "MVNormalDistribution.h"
#include <RcppArmadillo.h>
#include <cmath>

namespace dirichletprocess {

MVNormalCovarianceMixing::MVNormalCovarianceMixing(const arma::vec& mu0, double kappa0,
                                                   const arma::mat& Lambda, double nu, 
                                                   const std::string& covModel)
  : covModel(covModel), d(mu0.n_elem) {
  
  // Create the prior parameters list for MVNormalMixingDistribution
  Rcpp::List priorParams = Rcpp::List::create(
    Rcpp::Named("mu0") = mu0,
    Rcpp::Named("kappa0") = kappa0,
    Rcpp::Named("Lambda") = Lambda,
    Rcpp::Named("nu") = nu,
    Rcpp::Named("covModel") = covModel
  );
  
  // Create the MVNormalMixingDistribution instance
  mvn_dist = std::unique_ptr<dp::MVNormalMixingDistribution>(
    new dp::MVNormalMixingDistribution(priorParams)
  );
}

double MVNormalCovarianceMixing::log_likelihood(const arma::vec& data_point,
                                                const arma::vec& params) const {
  // Convert flattened params to mu and sig
  arma::vec mu, sig;
  unflattenParams(params, mu, sig);
  
  // Create theta list in the format expected by MVNormalMixingDistribution
  Rcpp::List theta = createClusterParameters(mu, sig);
  
  // Call the existing likelihood function
  Rcpp::NumericVector likelihood_vals = mvn_dist->likelihood(data_point, theta);
  
  // Return log likelihood
  return std::log(likelihood_vals[0]);
}

arma::vec MVNormalCovarianceMixing::posterior_draw(const arma::mat& cluster_data,
                                                   const arma::vec& prior_params) const {
  // Use the existing posteriorDraw function
  Rcpp::List result = mvn_dist->posteriorDraw(cluster_data, 1);
  
  // Extract mu and sig from result
  Rcpp::NumericVector mu_array = result["mu"];
  Rcpp::NumericVector sig_array = result["sig"];
  
  // Convert to arma vectors
  arma::vec mu(mu_array.begin(), mu_array.size());
  arma::vec sig(sig_array.begin(), sig_array.size());
  
  // Flatten and return
  return flattenParams(mu, sig);
}

arma::vec MVNormalCovarianceMixing::prior_draw() const {
  // Use the existing priorDraw function
  Rcpp::List result = mvn_dist->priorDraw(1);
  
  // Extract mu and sig from result
  Rcpp::NumericVector mu_array = result["mu"];
  Rcpp::NumericVector sig_array = result["sig"];
  
  // Handle the array dimensions properly
  Rcpp::IntegerVector mu_dim = mu_array.attr("dim");
  Rcpp::IntegerVector sig_dim = sig_array.attr("dim");
  
  // Extract parameters for the single draw (index 0)
  arma::vec mu(d);
  for (int i = 0; i < d; i++) {
    mu(i) = mu_array[i]; // First cluster, dimension i
  }
  
  // Extract sig parameters based on covariance model
  int nCovParams = mvn_dist->getNumCovParams(d);
  arma::vec sig(nCovParams);
  
  if (covModel == "FULL") {
    // Full precision matrix with bounds checking
    for (int i = 0; i < d; i++) {
      for (int j = 0; j < d; j++) {
        int array_idx = i + j * d;
        int sig_idx = i * d + j;
        if (array_idx < sig_array.size() && sig_idx < static_cast<int>(sig.n_elem)) {
          sig(sig_idx) = sig_array[array_idx]; // Column-major order
        }
      }
    }
  } else {
    // Covariance model parameters with bounds checking
    for (int i = 0; i < nCovParams && i < sig_array.size(); i++) {
      if (i < static_cast<int>(sig.n_elem)) {
        sig(i) = sig_array[i];
      }
    }
  }
  
  return flattenParams(mu, sig);
}

int MVNormalCovarianceMixing::param_dim() const {
  // Mean vector (d) + covariance parameters
  int nCovParams = mvn_dist->getNumCovParams(d);
  return d + nCovParams;
}

double MVNormalCovarianceMixing::predictive_probability(const arma::vec& data_point) const {
  // Use the existing predictive function
  arma::mat data_mat(1, data_point.n_elem);
  data_mat.row(0) = data_point.t();
  
  Rcpp::NumericVector pred_vals = mvn_dist->predictive(data_mat);
  return pred_vals[0];
}

arma::vec MVNormalCovarianceMixing::flattenParams(const arma::vec& mu, const arma::vec& sig) const {
  arma::vec params(param_dim());
  
  // First d elements are the mean
  if (d > 0) {
    params.subvec(0, d-1) = mu;
  }
  
  // Remaining elements are the covariance parameters
  int nCovParams = sig.n_elem;
  if (nCovParams > 0) {
    params.subvec(d, d + nCovParams - 1) = sig;
  }
  
  return params;
}

void MVNormalCovarianceMixing::unflattenParams(const arma::vec& params,
                                               arma::vec& mu, arma::vec& sig) const {
  // Extract mean with bounds checking
  if (d > 0 && params.n_elem >= static_cast<arma::uword>(d)) {
    mu = params.subvec(0, d-1);
  } else {
    mu.set_size(d);
    mu.zeros();
  }

  // Extract covariance parameters with bounds checking
  int nCovParams = mvn_dist->getNumCovParams(d);
  if (nCovParams > 0 && params.n_elem >= static_cast<arma::uword>(d + nCovParams)) {
    sig = params.subvec(d, d + nCovParams - 1);
  } else {
    sig.set_size(nCovParams);
    sig.zeros();
  }
}

Rcpp::List MVNormalCovarianceMixing::createClusterParameters(const arma::vec& mu, 
                                                             const arma::vec& sig) const {
  // Create arrays in the format expected by MVNormalMixingDistribution
  
  // Create mu array (1 x d x 1) with bounds checking
  Rcpp::NumericVector mu_array = Rcpp::NumericVector(Rcpp::Dimension(1, d, 1));
  for (int i = 0; i < d && i < mu_array.size(); i++) {
    if (i < static_cast<int>(mu.n_elem)) {
      mu_array[i] = mu(i);
    }
  }
  
  // Create sig array based on covariance model
  Rcpp::NumericVector sig_array;
  
  if (covModel == "FULL") {
    // Full precision matrix (d x d x 1) with bounds checking
    sig_array = Rcpp::NumericVector(Rcpp::Dimension(d, d, 1));
    for (int i = 0; i < d; i++) {
      for (int j = 0; j < d; j++) {
        int array_idx = i + j * d;
        int sig_idx = i * d + j;
        if (array_idx < sig_array.size() && sig_idx < static_cast<int>(sig.n_elem)) {
          sig_array[array_idx] = sig(sig_idx);
        }
      }
    }
  } else {
    // Covariance model parameters (nParams x 1) with bounds checking
    int nCovParams = sig.n_elem;
    sig_array = Rcpp::NumericVector(Rcpp::Dimension(nCovParams, 1));
    for (int i = 0; i < nCovParams && i < sig_array.size(); i++) {
      if (i < static_cast<int>(sig.n_elem)) {
        sig_array[i] = sig(i);
      }
    }
  }
  
  return Rcpp::List::create(
    Rcpp::Named("mu") = mu_array,
    Rcpp::Named("sig") = sig_array
  );
}

} // namespace dirichletprocess
