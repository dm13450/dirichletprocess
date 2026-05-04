// src/NormalExports.cpp
#include "NormalDistribution.h"
#include "RcppConversions.h"

//' @title Draw from a Normal distribution prior (C++)
//' @description C++ implementation for drawing from the prior distribution of a
//'   Normal/Inverse-Gamma model.
//' @param priorParams A numeric vector of prior parameters.
//' @param n The number of samples to draw.
//' @return A list containing the sampled parameters (mu and sigma^2).
//' @export
 // [[Rcpp::export]]
 Rcpp::List normal_prior_draw_cpp(Rcpp::NumericVector priorParams, int n = 1) {
   return dp::NormalMixingDistribution::priorDrawStatic(priorParams, n);
 }

//' @title Draw from a Normal distribution posterior (C++)
//' @description C++ implementation for drawing from the posterior distribution of a
//'   Normal/Inverse-Gamma model.
//' @param priorParams A numeric vector of prior parameters.
//' @param x A numeric matrix of data points.
//' @param n The number of samples to draw.
//' @return A list containing the sampled parameters (mu and sigma^2).
//' @export
 // [[Rcpp::export]]
 Rcpp::List normal_posterior_draw_cpp(Rcpp::NumericVector priorParams,
                                      Rcpp::NumericMatrix x,
                                      int n = 1) {
   arma::mat x_arma = Rcpp::as<arma::mat>(x);
   return dp::NormalMixingDistribution::posteriorDrawStatic(priorParams, x_arma, n);
 }

//' @title Update cluster components (C++ conjugate)
//' @description C++ implementation of the cluster component update for conjugate models.
//' @param dpObj A list representing the Dirichlet Process object.
//' @return A list with updated cluster assignments and parameters.
//' @export
 // [[Rcpp::export]]
 Rcpp::List conjugate_cluster_component_update_cpp(Rcpp::List dpObj) {
   // Extract necessary components from dpObj
   arma::mat data = Rcpp::as<arma::mat>(dpObj["data"]);
   arma::uvec clusterLabels = Rcpp::as<arma::uvec>(dpObj["clusterLabels"]);
   arma::uvec pointsPerCluster = Rcpp::as<arma::uvec>(dpObj["pointsPerCluster"]);
   int numberClusters = dpObj["numberClusters"];
   double alpha = dpObj["alpha"];
   Rcpp::List mixingDistribution = dpObj["mixingDistribution"];
   Rcpp::NumericVector priorParams = mixingDistribution["priorParameters"];
   Rcpp::List clusterParameters = dpObj["clusterParameters"];
   Rcpp::NumericVector predictiveArray = dpObj["predictiveArray"];

   // Create C++ DP object
   dp::ConjugateNormalDP* dp_cpp = new dp::ConjugateNormalDP();
   dp_cpp->data = data;
   dp_cpp->n = data.n_rows;
   dp_cpp->alpha = alpha;
   dp_cpp->clusterLabels = clusterLabels;
   dp_cpp->pointsPerCluster = pointsPerCluster;
   dp_cpp->numberClusters = numberClusters;
   dp_cpp->clusterParameters = clusterParameters;
   dp_cpp->predictiveArray = Rcpp::as<arma::vec>(predictiveArray);
   dp_cpp->mixingDistribution = new dp::NormalMixingDistribution(priorParams);

   // Perform cluster component update
   dp_cpp->clusterComponentUpdate();

   // Extract results
   Rcpp::List result = Rcpp::List::create(
     Rcpp::Named("clusterLabels") = dp_cpp->clusterLabels,
     Rcpp::Named("pointsPerCluster") = dp_cpp->pointsPerCluster,
     Rcpp::Named("numberClusters") = dp_cpp->numberClusters,
     Rcpp::Named("clusterParameters") = dp_cpp->clusterParameters
   );

   // Clean up
   delete dp_cpp;

   return result;
 }

//' @title Update cluster parameters (C++ conjugate)
//' @description C++ implementation of the cluster parameter update for conjugate models.
//' @param dpObj A list representing the Dirichlet Process object.
//' @return A list containing the updated cluster parameters.
//' @export
 // [[Rcpp::export]]
 Rcpp::List conjugate_cluster_parameter_update_cpp(Rcpp::List dpObj) {
   // Extract necessary components from dpObj
   arma::mat data = Rcpp::as<arma::mat>(dpObj["data"]);
   arma::uvec clusterLabels = Rcpp::as<arma::uvec>(dpObj["clusterLabels"]);
   int numberClusters = dpObj["numberClusters"];
   Rcpp::List mixingDistribution = dpObj["mixingDistribution"];
   Rcpp::NumericVector priorParams = mixingDistribution["priorParameters"];
   Rcpp::List clusterParameters = dpObj["clusterParameters"];

   // Create C++ DP object
   dp::ConjugateNormalDP* dp_cpp = new dp::ConjugateNormalDP();
   dp_cpp->data = data;
   dp_cpp->n = data.n_rows;
   dp_cpp->clusterLabels = clusterLabels;
   dp_cpp->numberClusters = numberClusters;
   dp_cpp->clusterParameters = clusterParameters;
   dp_cpp->mixingDistribution = new dp::NormalMixingDistribution(priorParams);

   // Perform cluster parameter update
   dp_cpp->clusterParameterUpdate();

   // Extract results
   Rcpp::List result = dp_cpp->clusterParameters;

   // Clean up
   delete dp_cpp;

   return result;
 }

//' @title Calculate Normal posterior parameters (C++)
//' @description C++ implementation for calculating posterior parameters for a
//'   Normal/Inverse-Gamma model.
//' @param priorParams A numeric vector of prior parameters.
//' @param x A numeric matrix of data.
//' @return A numeric matrix of posterior parameters.
//' @export
 // [[Rcpp::export]]
 Rcpp::NumericMatrix normal_posterior_parameters_cpp(Rcpp::NumericVector priorParams,
                                                     Rcpp::NumericMatrix x) {
   dp::NormalMixingDistribution md(priorParams);
   arma::mat x_arma = Rcpp::as<arma::mat>(x);
   return md.posteriorParameters(x_arma);
 }
