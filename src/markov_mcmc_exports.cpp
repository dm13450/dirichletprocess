// src/markov_mcmc_exports.cpp
#include <RcppArmadillo.h>
#include "markov_mcmc_runner.h"

// [[Rcpp::export]]
Rcpp::List run_markov_mcmc_cpp(arma::mat data,
                               Rcpp::List mixing_dist_params,
                               Rcpp::List mcmc_params) {
  try {
    // Input validation
    if (data.n_rows == 0 || data.n_cols == 0) {
      Rcpp::stop("Data matrix cannot be empty");
    }

    if (data.has_nan()) {
      Rcpp::stop("Data contains NA values");
    }

    // Ensure required parameters
    if (!mcmc_params.containsElementNamed("alpha")) {
      mcmc_params["alpha"] = 1.0;
    }

    if (!mcmc_params.containsElementNamed("beta")) {
      mcmc_params["beta"] = 1.0;
    }

    dirichletprocess::MarkovMCMCRunner runner(data, mixing_dist_params, mcmc_params);
    return runner.run();

  } catch (const std::exception& e) {
    Rcpp::stop("Markov MCMC error: " + std::string(e.what()));
  } catch (...) {
    Rcpp::stop("Unknown error in Markov MCMC");
  }
}
