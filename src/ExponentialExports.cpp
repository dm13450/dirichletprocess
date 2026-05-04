// src/ExponentialExports.cpp
#include <RcppArmadillo.h>
#include "ExponentialDistribution.h"
#include "RcppConversions.h"
#include "mcmc_runner.h"

namespace {

class ExponentialFastFitRunner : public dirichletprocess::MCMCRunner {
public:
  ExponentialFastFitRunner(const arma::mat& data,
                           const Rcpp::List& mixing_dist_params,
                           const Rcpp::List& mcmc_params)
    : MCMCRunner(data, mixing_dist_params, mcmc_params) {}

  Rcpp::List run_exponential_fit() {
    if (data.n_rows == 0 || data.n_cols == 0) {
      Rcpp::stop("Data matrix has invalid dimensions");
    }

    initialize_state();

    arma::vec alpha_chain(n_iter);
    arma::vec likelihood_chain(n_iter);
    Rcpp::IntegerMatrix labels_chain(n_iter, data.n_rows);
    Rcpp::List theta_chain(n_iter);

    for (int iter = 0; iter < n_iter; ++iter) {
      alpha_chain[iter] = state->alpha;
      likelihood_chain[iter] = compute_repaired_r_loglikelihood();

      for (arma::uword j = 0; j < data.n_rows; ++j) {
        labels_chain(iter, j) = state->cluster_labels[j] + 1;
      }

      Rcpp::List iter_params(state->cluster_params.size());
      for (size_t j = 0; j < state->cluster_params.size(); ++j) {
        iter_params[j] = state->cluster_params[j];
      }
      theta_chain[iter] = iter_params;

      single_iteration_update();
    }

    Rcpp::IntegerVector final_labels(state->cluster_labels.size());
    for (size_t i = 0; i < state->cluster_labels.size(); ++i) {
      final_labels[i] = state->cluster_labels[i] + 1;
    }

    Rcpp::List final_theta(state->cluster_params.size());
    for (size_t j = 0; j < state->cluster_params.size(); ++j) {
      final_theta[j] = state->cluster_params[j];
    }

    return Rcpp::List::create(
      Rcpp::Named("alpha_chain") = alpha_chain,
      Rcpp::Named("likelihood_chain") = likelihood_chain,
      Rcpp::Named("labels_chain") = labels_chain,
      Rcpp::Named("theta_chain") = theta_chain,
      Rcpp::Named("final_alpha") = state->alpha,
      Rcpp::Named("final_labels") = final_labels,
      Rcpp::Named("final_theta") = final_theta
    );
  }
};

} // namespace

//' @title Draw from an Exponential distribution prior (C++)
//' @description C++ implementation for drawing from the prior distribution of an
//'   Exponential/Gamma model.
//' @param priorParams A numeric vector of prior parameters (alpha0, beta0).
//' @param n The number of samples to draw.
//' @return A list containing the sampled rate parameters (lambda).
//' @export
 // [[Rcpp::export]]
 Rcpp::List exponential_prior_draw_cpp(Rcpp::NumericVector priorParams, int n = 1) {
   dp::ExponentialMixingDistribution md(priorParams);
   return md.priorDraw(n);
 }

//' @title Calculate Exponential log-likelihood (C++)
//' @description C++ implementation for calculating exponential log-likelihood.
//' @param x A numeric vector of data points.
//' @param lambda The rate parameter.
//' @return A numeric vector of log-likelihood values.
//' @export
 // [[Rcpp::export]]
 Rcpp::NumericVector exponential_log_likelihood_cpp(Rcpp::NumericVector x, double lambda) {
   if (lambda <= 0) {
     return Rcpp::NumericVector(x.size(), -std::numeric_limits<double>::infinity());
   }

   const int n = x.size();
   Rcpp::NumericVector result(n);

   for (int i = 0; i < n; ++i) {
     if (x[i] < 0) {
       result[i] = -std::numeric_limits<double>::infinity();
     } else {
       result[i] = std::log(lambda) - lambda * x[i];
     }
   }

   return result;
 }

//' @title Draw from an Exponential distribution posterior (C++)
//' @description C++ implementation for drawing from the posterior distribution of an
//'   Exponential/Gamma model.
//' @param priorParams A numeric vector of prior parameters.
//' @param x A numeric matrix of data points.
//' @param n The number of samples to draw.
//' @return A list containing the sampled rate parameters (lambda).
//' @export
 // [[Rcpp::export]]
 Rcpp::List exponential_posterior_draw_cpp(Rcpp::NumericVector priorParams,
                                           Rcpp::NumericMatrix x,
                                           int n = 1) {
   dp::ExponentialMixingDistribution md(priorParams);
   arma::mat x_arma = Rcpp::as<arma::mat>(x);
   return md.posteriorDraw(x_arma, n);
 }

//' @title Calculate Exponential posterior parameters (C++)
//' @description C++ implementation for calculating posterior parameters for an
//'   Exponential/Gamma model.
//' @param priorParams A numeric vector of prior parameters.
//' @param x A numeric matrix of data.
//' @return A list with alpha and beta posterior parameters.
//' @export
 // [[Rcpp::export]]
 Rcpp::List exponential_posterior_parameters_cpp(Rcpp::NumericVector priorParams,
                                                 Rcpp::NumericMatrix x) {
   dp::ExponentialMixingDistribution md(priorParams);
   arma::mat x_arma = Rcpp::as<arma::mat>(x);

   Rcpp::NumericMatrix post_params = md.posteriorParameters(x_arma);

   return Rcpp::List::create(
     Rcpp::Named("alpha") = post_params(0, 0),
     Rcpp::Named("beta") = post_params(0, 1)
   );
 }

//' @title Calculate Exponential likelihood (C++)
//' @description C++ implementation for calculating exponential likelihood.
//' @param x A numeric vector of data points.
//' @param lambda The rate parameter.
//' @return A numeric vector of likelihood values.
//' @export
 // [[Rcpp::export]]
 Rcpp::NumericVector exponential_likelihood_cpp(Rcpp::NumericVector x, double lambda) {
   if (lambda <= 0) {
     Rcpp::NumericVector result(x.size(), 1e-300);
     return result;
   }

   const int n = x.size();
   Rcpp::NumericVector result(n);

   // Direct pointer access - KEY OPTIMIZATION
   const double* x_ptr = &x[0];
   double* result_ptr = &result[0];

   // Vectorized computation
   for (int i = 0; i < n; ++i) {
     result_ptr[i] = (x_ptr[i] >= 0) ?
     (lambda * std::exp(-lambda * x_ptr[i])) : 0.0;
   }

   return result;
 }

//' @title Calculate Exponential predictive distribution (C++)
//' @description C++ implementation for calculating the predictive distribution.
//' @param priorParams A numeric vector of prior parameters.
//' @param x A numeric vector of data.
//' @return A numeric vector of predictive probabilities.
//' @export
 // [[Rcpp::export]]
 Rcpp::NumericVector exponential_predictive_cpp(Rcpp::NumericVector priorParams,
                                                Rcpp::NumericVector x) {
   dp::ExponentialMixingDistribution md(priorParams);
   arma::vec x_arma = Rcpp::as<arma::vec>(x);
   return md.predictive(x_arma);
 }

//' @title Update cluster components (C++ conjugate exponential)
//' @description C++ implementation of the cluster component update for conjugate models.
//' @param dpObj A list representing the Dirichlet Process object.
//' @return A list with updated cluster assignments and parameters.
//' @export
 // [[Rcpp::export]]
 Rcpp::List conjugate_exponential_cluster_component_update_cpp(Rcpp::List dpObj) {
   // Create C++ DP object using the new constructor
   dp::ConjugateExponentialDP* dp_cpp = new dp::ConjugateExponentialDP(dpObj);

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

//' @title Update alpha for conjugate exponential DP (C++)
//' @description C++ implementation of the concentration parameter update.
//' @param dpObj A list representing the Dirichlet Process object.
//' @return Updated alpha value.
//' @export
 // [[Rcpp::export]]
 double conjugate_exponential_update_alpha_cpp(Rcpp::List dpObj) {
   // Extract necessary components
   double alpha = dpObj["alpha"];
   int n = dpObj["n"];
   int numberClusters = dpObj["numberClusters"];
   Rcpp::NumericVector alphaPriorParameters = dpObj["alphaPriorParameters"];

   // Perform the update using auxiliary variable method
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

//' @title Update cluster parameters (C++ conjugate exponential)
//' @description C++ implementation of the cluster parameter update for conjugate models.
//' @param dpObj A list representing the Dirichlet Process object.
//' @return A list containing the updated cluster parameters.
//' @export
 // [[Rcpp::export]]
 Rcpp::List conjugate_exponential_cluster_parameter_update_cpp(Rcpp::List dpObj) {
   // Create C++ DP object using the new constructor
   dp::ConjugateExponentialDP* dp_cpp = new dp::ConjugateExponentialDP(dpObj);

   // Perform cluster parameter update
   dp_cpp->clusterParameterUpdate();

   // Extract results
   Rcpp::List result = dp_cpp->clusterParameters;

   // Clean up
   delete dp_cpp;

   return result;
 }

//' @title Fit Exponential DP with batch C++ live path
//' @description C++ batch implementation of the live exponential `Fit()` path
//'   with repaired-R-style pre-update chain semantics.
//' @param data Data matrix.
//' @param mixing_dist_params Mixing distribution parameters.
//' @param mcmc_params MCMC parameters.
//' @return A list containing repaired-R-style pre-update chains and final state.
//' @export
 // [[Rcpp::export]]
 Rcpp::List run_exponential_fit_cpp(arma::mat data,
                                    Rcpp::List mixing_dist_params,
                                    Rcpp::List mcmc_params) {
   try {
     if (data.n_rows == 0 || data.n_cols == 0) {
       Rcpp::stop("Data matrix cannot be empty");
     }

     if (data.has_nan()) {
       Rcpp::stop("Data contains NA values");
     }

     if (data.has_inf()) {
       Rcpp::stop("Data contains infinite values");
     }

     if (!mixing_dist_params.containsElementNamed("type") ||
         Rcpp::as<std::string>(mixing_dist_params["type"]) != "exponential") {
       Rcpp::stop("run_exponential_fit_cpp only supports exponential mixing distributions");
     }

     ExponentialFastFitRunner runner(data, mixing_dist_params, mcmc_params);
     return runner.run_exponential_fit();

   } catch (const std::exception& e) {
     Rcpp::stop("C++ Exponential Fit error: " + std::string(e.what()));
   } catch (...) {
     Rcpp::stop("Unknown error in C++ Exponential Fit");
   }
 }
