#include "hierarchical_mcmc_runner.h"
#include <RcppArmadillo.h>

// [[Rcpp::export]]
Rcpp::List run_hierarchical_mcmc_cpp(Rcpp::List datasets,
                                     Rcpp::List mixing_dist_params,
                                     Rcpp::List mcmc_params) {
  // Convert R list of datasets to vector of arma::mat
  std::vector<arma::mat> cpp_datasets;
  for (int i = 0; i < datasets.size(); i++) {
    cpp_datasets.push_back(Rcpp::as<arma::mat>(datasets[i]));
  }

  // Create and run the hierarchical MCMC runner
  dirichletprocess::HierarchicalMCMCRunner runner(
      cpp_datasets, mixing_dist_params, mcmc_params);

  return runner.run();
}
