// src/MVNormal2Exports.cpp
#include "MVNormal2Distribution.h"
#include "HierarchicalDP.h"
#include "RcppConversions.h"

//' @title Draw from a Multivariate Normal semi-conjugate prior (C++)
//' @description C++ implementation for drawing from the prior distribution of a
//'   Multivariate Normal semi-conjugate model.
//' @param priorParams A list containing prior parameters (mu0, sigma0, phi0, nu0).
//' @param n The number of samples to draw.
//' @return A list containing the sampled parameters (mu and sig).
//' @export
 // [[Rcpp::export]]
 Rcpp::List mvnormal2_prior_draw_cpp(Rcpp::List priorParams, int n = 1) {
   dp::MVNormal2MixingDistribution md(priorParams);
   return md.priorDraw(n);
 }

//' @title Draw from a Multivariate Normal semi-conjugate posterior (C++)
//' @description C++ implementation for drawing from the posterior distribution of a
//'   Multivariate Normal semi-conjugate model.
//' @param priorParams A list containing prior parameters.
//' @param x A numeric matrix of data points.
//' @param n The number of samples to draw.
//' @return A list containing the sampled parameters (mu and sig).
//' @export
 // [[Rcpp::export]]
 Rcpp::List mvnormal2_posterior_draw_cpp(Rcpp::List priorParams,
                                         Rcpp::NumericMatrix x,
                                         int n = 1) {
   dp::MVNormal2MixingDistribution md(priorParams);
   arma::mat x_arma = Rcpp::as<arma::mat>(x);
   return md.posteriorDraw(x_arma, n);
 }

//' @title Calculate MVNormal2 likelihood (C++)
//' @description C++ implementation for calculating multivariate normal likelihood.
//' @param x A numeric matrix of data points.
//' @param theta A list containing mu and sig parameters.
//' @return A numeric matrix of likelihood values with one row per observation
//'   and one column per cluster.
//' @export
 // [[Rcpp::export]]
 Rcpp::NumericMatrix mvnormal2_likelihood_cpp(Rcpp::NumericMatrix x,
                                              Rcpp::List theta) {
   dp::MVNormal2MixingDistribution md(Rcpp::List::create());
   arma::mat x_arma = Rcpp::as<arma::mat>(x);

   arma::vec first_row = x_arma.row(0).t();
   Rcpp::NumericVector first_result = md.likelihood(first_row, theta);
   int n_clusters = first_result.size();
   Rcpp::NumericMatrix result(x_arma.n_rows, n_clusters);

   for (int k = 0; k < n_clusters; ++k) {
     result(0, k) = first_result[k];
   }

   for (size_t i = 1; i < x_arma.n_rows; i++) {
     arma::vec x_row = x_arma.row(i).t();
     Rcpp::NumericVector row_result = md.likelihood(x_row, theta);
     for (int k = 0; k < n_clusters; ++k) {
       result(i, k) = row_result[k];
     }
   }
   return result;
 }

//' @title Update cluster components for MVNormal2 (C++ non-conjugate)
//' @description C++ implementation of the cluster component update for MVNormal2 non-conjugate models.
//' @param dpObj A list representing the Dirichlet Process object.
//' @return A list with updated cluster assignments and parameters.
//' @export
 // [[Rcpp::export]]
 Rcpp::List nonconjugate_mvnormal2_cluster_component_update_cpp(Rcpp::List dpObj) {
   // Extract necessary components
   arma::mat data = Rcpp::as<arma::mat>(dpObj["data"]);
   arma::uvec clusterLabels = Rcpp::as<arma::uvec>(dpObj["clusterLabels"]);
   arma::uvec pointsPerCluster = Rcpp::as<arma::uvec>(dpObj["pointsPerCluster"]);
   int numberClusters = dpObj["numberClusters"];
   double alpha = dpObj["alpha"];
   Rcpp::List mixingDistribution = dpObj["mixingDistribution"];
   Rcpp::List priorParams = mixingDistribution["priorParameters"];
   Rcpp::List clusterParameters = dpObj["clusterParameters"];
   Rcpp::NumericVector alphaPriorParameters = dpObj["alphaPriorParameters"];
   int m = dpObj.containsElementNamed("m") ? Rcpp::as<int>(dpObj["m"]) : 3;

   // Create C++ DP object
   dp::NonConjugateMVNormal2DP* dp_cpp = new dp::NonConjugateMVNormal2DP();
   dp_cpp->data = data;
   dp_cpp->n = data.n_rows;
   dp_cpp->alpha = alpha;
   dp_cpp->clusterLabels = clusterLabels;
   dp_cpp->pointsPerCluster = pointsPerCluster;
   dp_cpp->numberClusters = numberClusters;
   dp_cpp->clusterParameters = clusterParameters;
   dp_cpp->alphaPriorParameters = alphaPriorParameters;
   dp_cpp->m = m;
   dp_cpp->mixingDistribution = new dp::MVNormal2MixingDistribution(priorParams);

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

//' @title Update cluster parameters for MVNormal2 (C++ non-conjugate)
//' @description C++ implementation of the cluster parameter update for MVNormal2 non-conjugate models.
//' @param dpObj A list representing the Dirichlet Process object.
//' @return A list containing the updated cluster parameters.
//' @export
 // [[Rcpp::export]]
 Rcpp::List nonconjugate_mvnormal2_cluster_parameter_update_cpp(Rcpp::List dpObj) {
   // Extract necessary components
   arma::mat data = Rcpp::as<arma::mat>(dpObj["data"]);
   arma::uvec clusterLabels = Rcpp::as<arma::uvec>(dpObj["clusterLabels"]);
   int numberClusters = dpObj["numberClusters"];
   Rcpp::List mixingDistribution = dpObj["mixingDistribution"];
   Rcpp::List priorParams = mixingDistribution["priorParameters"];
   Rcpp::List clusterParameters = dpObj["clusterParameters"];
   int mhDraws = dpObj.containsElementNamed("mhDraws") ? Rcpp::as<int>(dpObj["mhDraws"]) : 100;

   // Create C++ DP object
   dp::NonConjugateMVNormal2DP* dp_cpp = new dp::NonConjugateMVNormal2DP();
   dp_cpp->data = data;
   dp_cpp->n = data.n_rows;
   dp_cpp->clusterLabels = clusterLabels;
   dp_cpp->numberClusters = numberClusters;
   dp_cpp->clusterParameters = clusterParameters;
   dp_cpp->mhDraws = mhDraws;
   dp_cpp->mixingDistribution = new dp::MVNormal2MixingDistribution(priorParams);

   // Perform cluster parameter update
   dp_cpp->clusterParameterUpdate();

   // Extract results
   Rcpp::List result = dp_cpp->clusterParameters;

   // Clean up
   delete dp_cpp;

   return result;
 }

//' @title Fit Hierarchical MVNormal2 DP (C++)
//' @description C++ implementation for fitting a Hierarchical MVNormal2 DP.
//' @param dpList An R list representing the hierarchical DP object.
//' @param iterations Number of iterations.
//' @param updatePrior Whether to update prior parameters.
//' @param progressBar Whether to show progress bar.
//' @return Updated hierarchical DP object.
//' @export
 // [[Rcpp::export]]
 Rcpp::List hierarchical_mvnormal2_fit_cpp(Rcpp::List dpList, int iterations,
                                           bool updatePrior = false,
                                           bool progressBar = true) {
   // Create C++ object from R
   dp::HierarchicalMVNormal2DP* hdp = dp::HierarchicalMVNormal2DP::fromR(dpList);

   // Fit the model
   hdp->fit(iterations, updatePrior, progressBar);

   // Convert back to R
   Rcpp::List result = hdp->toR();

   // Add iteration info
   result["iterations"] = iterations;

   // Clean up
   delete hdp;

   return result;
 }

//' @title Create Hierarchical MVNormal2 mixing distributions (C++)
//' @description C++ implementation for creating hierarchical MVNormal2 mixing distributions.
//' @param n Number of datasets.
//' @param priorParameters Prior parameters for the MVNormal2 distribution.
//' @param alphaPrior Alpha prior parameters.
//' @param gammaPrior Gamma prior parameters.
//' @param num_sticks Number of stick breaking values.
//' @return List of mixing distributions.
//' @export
 // [[Rcpp::export]]
 Rcpp::List hierarchical_mvnormal2_mixing_create_cpp(
     int n,
     Rcpp::List priorParameters,
     Rcpp::NumericVector alphaPrior,
     Rcpp::NumericVector gammaPrior,
     int num_sticks) {

   // Create base MVNormal2 mixing distribution
   dp::MVNormal2MixingDistribution baseMD(priorParameters);

   // Draw gamma
   double gamma = R::rgamma(gammaPrior[0], 1.0 / gammaPrior[1]);

   // Draw global parameters
   Rcpp::List theta_k = baseMD.priorDraw(num_sticks);

   // Stick breaking weights
   arma::vec beta_k(num_sticks);
   double remaining = 1.0;
   for (int i = 0; i < num_sticks - 1; i++) {
     double beta = R::rbeta(1.0, gamma);
     beta_k[i] = beta * remaining;
     remaining *= (1.0 - beta);
   }
   beta_k[num_sticks - 1] = remaining;

   // Create individual mixing distributions
   Rcpp::List mdobj_list(n);

   for (int i = 0; i < n; i++) {
     Rcpp::List mdobj;

     // Copy base distribution properties
     mdobj["distribution"] = "mvnormal2";
     mdobj["priorParameters"] = priorParameters;
     mdobj["conjugate"] = false;

     // Hierarchical properties
     mdobj["theta_k"] = theta_k;
     mdobj["beta_k"] = Rcpp::NumericVector(beta_k.begin(), beta_k.end());
     mdobj["gamma"] = gamma;

     // Individual alpha
     double alpha = R::rgamma(alphaPrior[0], 1.0 / alphaPrior[1]);
     mdobj["alpha"] = alpha;

     // Draw pi_k using stick breaking
     arma::vec pi_k(num_sticks);
     arma::vec alpha_beta_params(num_sticks);
     double current_alpha = Rcpp::as<double>(mdobj["alpha"]); // Get the individual alpha

     for(int j=0; j < num_sticks; ++j) {
       alpha_beta_params[j] = current_alpha * beta_k[j]; // beta_k are global proportions
       if (alpha_beta_params[j] <= 0.0) { // Ensure gamma parameters are positive
         alpha_beta_params[j] = 1e-9; // A small positive number
       }
     }

     for(int j=0; j < num_sticks; ++j) {
       pi_k[j] = R::rgamma(alpha_beta_params[j], 1.0);
     }

     double sum_pi_k = arma::sum(pi_k);
     if (sum_pi_k == 0.0) {
       // This case should be rare if alpha_beta_params are positive.
       // If all alpha_beta_params[j] are extremely small, rgamma might return 0s.
       // Assign uniform probabilities as a fallback.
       Rcpp::warning("Sum of components for Dirichlet draw of pi_k was zero. Assigning uniform probabilities.");
       pi_k.fill(1.0 / num_sticks);
     } else {
       pi_k = pi_k / sum_pi_k; // Normalize
     }
     mdobj["pi_k"] = Rcpp::NumericVector(pi_k.begin(), pi_k.end());

     // Set class
     mdobj.attr("class") = Rcpp::CharacterVector::create("hierarchical", "mvnormal2", "nonconjugate");

     mdobj_list[i] = mdobj;
   }

   return mdobj_list;
 }

//' @title Update alpha for non-conjugate MVNormal2 DP (C++)
//' @description C++ implementation of the concentration parameter update for MVNormal2.
//' @param dpObj A list representing the Dirichlet Process object.
//' @return Updated alpha value.
//' @export
// [[Rcpp::export]]
double nonconjugate_mvnormal2_update_alpha_cpp(Rcpp::List dpObj) {
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
