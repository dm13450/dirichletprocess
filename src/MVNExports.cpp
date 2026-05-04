// src/MVNExports.cpp
#include "MVNormalDistribution.h"
#include "RcppConversions.h"

// The ensureSymmetric function is already defined as inline in the header, so we don't need to define it here

//' @title Draw from a Multivariate Normal-Wishart prior (C++)
//' @description C++ implementation for drawing from the prior distribution of a
//'   Multivariate Normal-Wishart model.
//' @param priorParams A list containing prior parameters (mu0, kappa0, Lambda, nu).
//' @param n The number of samples to draw.
//' @return A list containing the sampled parameters (mu and sig).
//' @export
 // [[Rcpp::export]]
 Rcpp::List mvnormal_prior_draw_cpp(Rcpp::List priorParams, int n = 1) {
   return dp::MVNormalMixingDistribution::priorDrawStatic(priorParams, n);
 }

//' @title Draw from a Multivariate Normal-Wishart posterior (C++)
//' @description C++ implementation for drawing from the posterior distribution of a
//'   Multivariate Normal-Wishart model.
//' @param priorParams A list containing prior parameters.
//' @param x A numeric matrix of data points.
//' @param n The number of samples to draw.
//' @return A list containing the sampled parameters (mu and sig).
//' @export
 // [[Rcpp::export]]
 Rcpp::List mvnormal_posterior_draw_cpp(Rcpp::List priorParams,
                                        Rcpp::NumericMatrix x,
                                        int n = 1) {
   arma::mat x_arma = Rcpp::as<arma::mat>(x);
   return dp::MVNormalMixingDistribution::posteriorDrawStatic(priorParams, x_arma, n);
 }

//' @title Calculate MVNormal posterior parameters (C++)
//' @description C++ implementation for calculating posterior parameters for a
//'   Multivariate Normal-Wishart model.
//' @param priorParams A list containing prior parameters.
//' @param x A numeric matrix of data.
//' @return A list of posterior parameters.
//' @export
 // [[Rcpp::export]]
 Rcpp::List mvnormal_posterior_parameters_cpp(Rcpp::List priorParams,
                                              Rcpp::NumericMatrix x) {
   dp::MVNormalMixingDistribution md(priorParams);
   arma::mat x_arma = Rcpp::as<arma::mat>(x);
   return md.posteriorParameters(x_arma);
 }

//' @title Calculate MVNormal predictive distribution (C++)
//' @description C++ implementation for calculating the predictive distribution.
//' @param priorParams A list containing prior parameters.
//' @param x A numeric matrix of data.
//' @return A numeric vector of predictive probabilities.
//' @export
 // [[Rcpp::export]]
 Rcpp::NumericVector mvnormal_predictive_cpp(Rcpp::List priorParams,
                                             Rcpp::NumericMatrix x) {
   dp::MVNormalMixingDistribution md(priorParams);
   arma::mat x_arma = Rcpp::as<arma::mat>(x);
   return md.predictive(x_arma);
 }

//' @title Calculate MVNormal likelihood (C++)
//' @description C++ implementation for calculating multivariate normal likelihood.
//' @param x A numeric matrix of data points.
//' @param mu Mean vector.
//' @param sigma Covariance matrix.
//' @return A numeric vector of likelihood values.
//' @export
 // [[Rcpp::export]]
 Rcpp::NumericVector mvnormal_likelihood_cpp(Rcpp::NumericMatrix x,
                                             Rcpp::NumericVector mu,
                                             Rcpp::NumericMatrix sigma) {
   arma::mat x_arma = Rcpp::as<arma::mat>(x);
   arma::vec mu_arma = Rcpp::as<arma::vec>(mu);
   arma::mat sigma_arma = Rcpp::as<arma::mat>(sigma);

   // For this export function, sigma is expected to be a covariance matrix
   // (to match mvtnorm::dmvnorm behavior)
   int n = x_arma.n_rows;
   int d = x_arma.n_cols;
   Rcpp::NumericVector result(n);

   // Ensure sigma is symmetric
   sigma_arma = dp::ensureSymmetric(sigma_arma);

   double log_det_val;
   double sign;
   arma::log_det(log_det_val, sign, sigma_arma);

   if (sign <= 0) {
     result.fill(1e-300);
     return result;
   }

   arma::mat sigma_inv;
   try {
     sigma_inv = arma::inv_sympd(sigma_arma);
   } catch(...) {
     result.fill(1e-300);
     return result;
   }

   double log_const = -0.5 * d * std::log(2.0 * M_PI) - 0.5 * log_det_val;

   for (int i = 0; i < n; i++) {
     arma::vec x_centered = x_arma.row(i).t() - mu_arma;
     double quad_form = arma::as_scalar(x_centered.t() * sigma_inv * x_centered);
     result[i] = std::exp(log_const - 0.5 * quad_form);
   }

   // Ensure result is a plain numeric vector without extra attributes
   result.attr("dim") = R_NilValue;
   return result;
 }

// NOTE: The conjugate_mvnormal_cluster_component_update_cpp and
// conjugate_mvnormal_cluster_parameter_update_cpp functions are
// implemented in MVNormalDistribution.cpp within the dp namespace

//' @title Update alpha for conjugate MVNormal DP (C++)
//' @description C++ implementation of the concentration parameter update for conjugate MVNormal.
//' @param dpObj A list representing the Dirichlet Process object.
//' @return Updated alpha value.
//' @export
// [[Rcpp::export]]
double conjugate_mvnormal_update_alpha_cpp(Rcpp::List dpObj) {
  // Extract necessary components
  double alpha = dpObj["alpha"];
  int n = dpObj["n"];
  int numberClusters = dpObj["numberClusters"];
  Rcpp::NumericVector alphaPriorParameters = dpObj["alphaPriorParameters"];

  // Perform the update using auxiliary variable method (West 1992)
  double x = R::rbeta(alpha + 1.0, n);

  double pi1 = alphaPriorParameters[0] + numberClusters - 1.0;
  double pi2 = n * (alphaPriorParameters[1] - log(x));
  double pi_ratio = pi1 / (pi1 + pi2);

  double postShape, postRate;
  if (R::runif(0, 1) < pi_ratio) {
    postShape = alphaPriorParameters[0] + numberClusters;
  } else {
    postShape = alphaPriorParameters[0] + numberClusters - 1.0;
  }
  postRate = alphaPriorParameters[1] - log(x);

  double new_alpha = R::rgamma(postShape, 1.0/postRate);

  return new_alpha;
}
