// src/WeibullExports.cpp
#include <RcppArmadillo.h>
#include "WeibullDistribution.h"
#include "RcppConversions.h"
#include <memory>

// [[Rcpp::export]]
Rcpp::List weibull_prior_draw_cpp(const Rcpp::NumericVector& priorParams, int n = 1) {
  // Validate inputs
  if (priorParams.size() < 3) {
    Rcpp::stop("priorParams must have at least 3 elements");
  }
  if (n <= 0) {
    Rcpp::stop("n must be positive");
  }

  dp::WeibullMixingDistribution md(priorParams, Rcpp::NumericVector::create(1.0, 1.0));
  return md.priorDraw(n);
}

// [[Rcpp::export]]
Rcpp::NumericVector weibull_likelihood_cpp(const Rcpp::NumericVector& x, double alpha, double lambda) {
  // Validate inputs - handle NaN/Inf by returning zero likelihood
  if (alpha <= 0 || !std::isfinite(alpha) || lambda <= 0 || !std::isfinite(lambda)) {
    // Return zero likelihood for invalid parameters instead of throwing error
    return Rcpp::NumericVector(x.size(), 1e-300);
  }

  arma::vec x_arma = Rcpp::as<arma::vec>(x);

  Rcpp::NumericVector alpha_arr(1);
  Rcpp::NumericVector lambda_arr(1);
  alpha_arr.attr("dim") = Rcpp::IntegerVector::create(1, 1, 1);
  lambda_arr.attr("dim") = Rcpp::IntegerVector::create(1, 1, 1);
  alpha_arr[0] = alpha;
  lambda_arr[0] = lambda;

  Rcpp::List theta = Rcpp::List::create(alpha_arr, lambda_arr);

  dp::WeibullMixingDistribution md(Rcpp::NumericVector::create(10.0, 2.0, 4.0),
                                   Rcpp::NumericVector::create(1.0, 1.0));
  return md.likelihood(x_arma, theta);
}

// [[Rcpp::export]]
double weibull_prior_density_cpp(double alpha, const Rcpp::NumericVector& priorParams) {
  // Validate inputs
  if (priorParams.size() < 3) {
    Rcpp::stop("priorParams must have at least 3 elements");
  }

  dp::WeibullMixingDistribution md(priorParams, Rcpp::NumericVector::create(1.0, 1.0));

  Rcpp::NumericVector alpha_arr(1);
  Rcpp::NumericVector lambda_arr(1);
  alpha_arr.attr("dim") = Rcpp::IntegerVector::create(1, 1, 1);
  lambda_arr.attr("dim") = Rcpp::IntegerVector::create(1, 1, 1);
  alpha_arr[0] = alpha;
  lambda_arr[0] = 1.0; // Lambda doesn't affect uniform prior on alpha

  Rcpp::List theta = Rcpp::List::create(alpha_arr, lambda_arr);
  return md.priorDensity(theta)[0];
}

// [[Rcpp::export]]
Rcpp::List weibull_posterior_draw_cpp(const Rcpp::NumericVector& priorParams,
                                      const Rcpp::NumericVector& mhStepSize,
                                      const Rcpp::NumericMatrix& x,
                                      int n = 1) {
  // Validate inputs
  if (priorParams.size() < 3) {
    Rcpp::stop("priorParams must have at least 3 elements");
  }
  if (mhStepSize.size() < 2) {
    Rcpp::stop("mhStepSize must have at least 2 elements");
  }
  if (x.nrow() == 0) {
    Rcpp::stop("x must have at least one observation");
  }
  if (n <= 0) {
    Rcpp::stop("n must be positive");
  }

  arma::mat x_arma = Rcpp::as<arma::mat>(x);
  dp::WeibullMixingDistribution md(priorParams, mhStepSize);
  return md.posteriorDraw(x_arma, n);
}

// [[Rcpp::export]]
Rcpp::NumericMatrix weibull_prior_parameters_update_cpp(const Rcpp::NumericVector& priorParams,
                                                        const Rcpp::NumericVector& hyperPriorParams,
                                                        const Rcpp::List& clusterParameters,
                                                        int n = 1) {
  // Validate inputs
  if (priorParams.size() < 3) {
    Rcpp::stop("priorParams must have at least 3 elements");
  }
  if (hyperPriorParams.size() < 4) {
    Rcpp::stop("hyperPriorParams must have at least 4 elements");
  }
  if (clusterParameters.size() < 2) {
    Rcpp::stop("clusterParameters must have at least 2 elements");
  }

  dp::WeibullMixingDistribution md(priorParams, Rcpp::NumericVector::create(1.0, 1.0), hyperPriorParams);
  md.updatePriorParameters(clusterParameters, n);
  return Rcpp::as<Rcpp::NumericMatrix>(md.priorParameters);
}

// [[Rcpp::export]]
Rcpp::List nonconjugate_weibull_cluster_parameter_update_cpp(Rcpp::List dp_list) {
  // Validate inputs
  if (!dp_list.containsElementNamed("data") ||
      !dp_list.containsElementNamed("clusterLabels") ||
      !dp_list.containsElementNamed("numberClusters") ||
      !dp_list.containsElementNamed("mixingDistribution") ||
      !dp_list.containsElementNamed("clusterParameters")) {
      Rcpp::stop("Missing required elements in dp_list");
  }

  // Extract necessary components
  arma::mat data = Rcpp::as<arma::mat>(dp_list["data"]);
  arma::uvec clusterLabels = Rcpp::as<arma::uvec>(dp_list["clusterLabels"]);

  // Convert from R's 1-based to C++'s 0-based indexing
  clusterLabels = clusterLabels - 1;

  int numberClusters = dp_list["numberClusters"];
  Rcpp::List mixingDistribution = dp_list["mixingDistribution"];
  Rcpp::NumericVector priorParams = mixingDistribution["priorParameters"];
  Rcpp::NumericVector mhStepSize = mixingDistribution["mhStepSize"];
  Rcpp::List clusterParameters = dp_list["clusterParameters"];

  // Create C++ DP object using smart pointer
  std::unique_ptr<dp::NonConjugateWeibullDP> dp_cpp(new dp::NonConjugateWeibullDP());
  dp_cpp->data = data;
  dp_cpp->n = data.n_rows;
  dp_cpp->clusterLabels = clusterLabels;
  dp_cpp->numberClusters = numberClusters;
  dp_cpp->clusterParameters = clusterParameters;

  Rcpp::NumericVector hyperPriorParams;
  if (mixingDistribution.containsElementNamed("hyperPriorParameters")) {
    hyperPriorParams = Rcpp::as<Rcpp::NumericVector>(mixingDistribution["hyperPriorParameters"]);
  }

  dp_cpp->mixingDistribution = new dp::WeibullMixingDistribution(priorParams, mhStepSize, hyperPriorParams);

  // Perform cluster parameter update
  dp_cpp->clusterParameterUpdate();

  // Extract results
  Rcpp::List result = dp_cpp->clusterParameters;

  return result;
}

// [[Rcpp::export]]
Rcpp::List nonconjugate_weibull_cluster_component_update_cpp(Rcpp::List dp_list) {
  // Validate inputs
  if (!dp_list.containsElementNamed("data") ||
      !dp_list.containsElementNamed("clusterLabels") ||
      !dp_list.containsElementNamed("pointsPerCluster") ||
      !dp_list.containsElementNamed("numberClusters") ||
      !dp_list.containsElementNamed("alpha") ||
      !dp_list.containsElementNamed("mixingDistribution") ||
      !dp_list.containsElementNamed("clusterParameters") ||
      !dp_list.containsElementNamed("alphaPriorParameters") ||
      !dp_list.containsElementNamed("m")) {
      Rcpp::stop("Missing required elements in dp_list");
  }

  // Extract necessary components
  arma::mat data = Rcpp::as<arma::mat>(dp_list["data"]);
  if (data.n_rows == 0) {
    Rcpp::stop("Empty data matrix");
  }

  arma::uvec clusterLabels = Rcpp::as<arma::uvec>(dp_list["clusterLabels"]);

  // Convert from R's 1-based to C++'s 0-based indexing
  clusterLabels = clusterLabels - 1;

  arma::uvec pointsPerCluster = Rcpp::as<arma::uvec>(dp_list["pointsPerCluster"]);
  int numberClusters = dp_list["numberClusters"];
  double alpha = dp_list["alpha"];
  Rcpp::List mixingDistribution = dp_list["mixingDistribution"];
  Rcpp::NumericVector priorParams = mixingDistribution["priorParameters"];
  Rcpp::NumericVector mhStepSize = mixingDistribution["mhStepSize"];
  Rcpp::List clusterParameters = dp_list["clusterParameters"];
  Rcpp::NumericVector alphaPriorParameters = dp_list["alphaPriorParameters"];
  int m = dp_list["m"];

  // Validate dimensions
  if (clusterLabels.size() != data.n_rows) {
    Rcpp::stop("clusterLabels size does not match data rows");
  }

  // Create C++ DP object using smart pointer
  std::unique_ptr<dp::NonConjugateWeibullDP> dp_cpp(new dp::NonConjugateWeibullDP());
  dp_cpp->data = data;
  dp_cpp->n = data.n_rows;
  dp_cpp->alpha = alpha;
  dp_cpp->clusterLabels = clusterLabels;
  dp_cpp->pointsPerCluster = pointsPerCluster;
  dp_cpp->numberClusters = numberClusters;
  dp_cpp->clusterParameters = clusterParameters;
  dp_cpp->alphaPriorParameters = alphaPriorParameters;
  dp_cpp->m = m;

  Rcpp::NumericVector hyperPriorParams;
  if (mixingDistribution.containsElementNamed("hyperPriorParameters")) {
    hyperPriorParams = Rcpp::as<Rcpp::NumericVector>(mixingDistribution["hyperPriorParameters"]);
  }

  dp_cpp->mixingDistribution = new dp::WeibullMixingDistribution(priorParams, mhStepSize, hyperPriorParams);

  // Perform cluster component update
  dp_cpp->clusterComponentUpdate();

  // Convert cluster labels back to 1-indexed for R
  arma::uvec clusterLabels_r = dp_cpp->clusterLabels + 1;

  // Extract results
  Rcpp::List result = Rcpp::List::create(
    Rcpp::Named("clusterLabels") = clusterLabels_r,  // Convert to 1-indexed
    Rcpp::Named("pointsPerCluster") = dp_cpp->pointsPerCluster,
    Rcpp::Named("numberClusters") = dp_cpp->numberClusters,
    Rcpp::Named("clusterParameters") = dp_cpp->clusterParameters
  );

  return result;
}
