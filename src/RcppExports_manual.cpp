// src/RcppExports_manual.cpp
#include "mcmc_runner_manual.h"
#include <Rcpp.h>

using namespace Rcpp;
using namespace dirichletprocess;

// [[Rcpp::export]]
SEXP create_mcmc_runner_cpp(arma::mat data, List mixing_params, List mcmc_params) {
  MCMCRunnerManual* runner = new MCMCRunnerManual(data, mixing_params, mcmc_params);
  XPtr<MCMCRunnerManual> ptr(runner, true);
  return ptr;
}

// Core stepping functions
// [[Rcpp::export]]
void step_assignments_cpp(SEXP runner_ptr) {
  XPtr<MCMCRunnerManual> runner(runner_ptr);
  runner->step_cluster_assignments();
}

// [[Rcpp::export]]
void step_parameters_cpp(SEXP runner_ptr) {
  XPtr<MCMCRunnerManual> runner(runner_ptr);
  runner->step_cluster_parameters();
}

// [[Rcpp::export]]
void step_concentration_cpp(SEXP runner_ptr) {
  XPtr<MCMCRunnerManual> runner(runner_ptr);
  runner->step_concentration();
}

// [[Rcpp::export]]
void perform_iteration_cpp(SEXP runner_ptr) {
  XPtr<MCMCRunnerManual> runner(runner_ptr);
  runner->perform_iteration();
}

// State access functions
// [[Rcpp::export]]
List get_state_cpp(SEXP runner_ptr) {
  XPtr<MCMCRunnerManual> runner(runner_ptr);
  return runner->get_current_state();
}

// [[Rcpp::export]]
List get_results_cpp(SEXP runner_ptr) {
  XPtr<MCMCRunnerManual> runner(runner_ptr);
  return runner->get_results();
}

// [[Rcpp::export]]
bool is_complete_cpp(SEXP runner_ptr) {
  XPtr<MCMCRunnerManual> runner(runner_ptr);
  return runner->is_complete();
}

// State modification functions
// [[Rcpp::export]]
void set_labels_cpp(SEXP runner_ptr, std::vector<int> labels) {
  XPtr<MCMCRunnerManual> runner(runner_ptr);
  runner->set_cluster_labels(labels);
}

// [[Rcpp::export]]
void set_params_cpp(SEXP runner_ptr, List params) {
  XPtr<MCMCRunnerManual> runner(runner_ptr);
  runner->set_cluster_params(params);
}

// Additional features exports
// [[Rcpp::export]]
void set_parameter_bounds_cpp(SEXP runner_ptr, arma::vec lower, arma::vec upper) {
  XPtr<MCMCRunnerManual> runner(runner_ptr);
  runner->set_parameter_bounds(lower, upper);
}

// [[Rcpp::export]]
List get_auxiliary_params_cpp(SEXP runner_ptr) {
  XPtr<MCMCRunnerManual> runner(runner_ptr);
  return runner->get_auxiliary_params();
}

// [[Rcpp::export]]
void set_update_flags_cpp(SEXP runner_ptr, bool clusters, bool params, bool alpha) {
  XPtr<MCMCRunnerManual> runner(runner_ptr);
  runner->set_update_flags(clusters, params, alpha);
}

// [[Rcpp::export]]
arma::vec get_cluster_likelihoods_cpp(SEXP runner_ptr) {
  XPtr<MCMCRunnerManual> runner(runner_ptr);
  return runner->get_cluster_likelihoods();
}

// [[Rcpp::export]]
arma::mat get_membership_matrix_cpp(SEXP runner_ptr) {
  XPtr<MCMCRunnerManual> runner(runner_ptr);
  return runner->get_cluster_membership_matrix();
}

// [[Rcpp::export]]
List get_cluster_statistics_cpp(SEXP runner_ptr) {
  XPtr<MCMCRunnerManual> runner(runner_ptr);
  return runner->get_cluster_statistics();
}

// [[Rcpp::export]]
void merge_clusters_cpp(SEXP runner_ptr, int cluster1, int cluster2) {
  XPtr<MCMCRunnerManual> runner(runner_ptr);
  runner->merge_clusters(cluster1 - 1, cluster2 - 1);  // Convert to 0-based
}

// [[Rcpp::export]]
void split_cluster_cpp(SEXP runner_ptr, int cluster_id, double split_prob) {
  XPtr<MCMCRunnerManual> runner(runner_ptr);
  runner->split_cluster(cluster_id - 1, split_prob);  // Convert to 0-based
}

// [[Rcpp::export]]
void set_temperature_cpp(SEXP runner_ptr, double temp) {
  XPtr<MCMCRunnerManual> runner(runner_ptr);
  runner->set_temperature(temp);
}

// [[Rcpp::export]]
void set_auxiliary_count_cpp(SEXP runner_ptr, int m) {
  XPtr<MCMCRunnerManual> runner(runner_ptr);
  runner->set_auxiliary_parameter_count(m);
}

// [[Rcpp::export]]
List sample_predictive_cpp(SEXP runner_ptr, int n_samples) {
  XPtr<MCMCRunnerManual> runner(runner_ptr);
  return runner->sample_posterior_predictive(n_samples);
}

// [[Rcpp::export]]
double get_log_posterior_cpp(SEXP runner_ptr) {
  XPtr<MCMCRunnerManual> runner(runner_ptr);
  return runner->get_log_posterior();
}

// [[Rcpp::export]]
arma::vec get_cluster_entropies_cpp(SEXP runner_ptr) {
  XPtr<MCMCRunnerManual> runner(runner_ptr);
  return runner->get_cluster_entropies();
}

// [[Rcpp::export]]
double get_clustering_entropy_cpp(SEXP runner_ptr) {
  XPtr<MCMCRunnerManual> runner(runner_ptr);
  return runner->get_clustering_entropy();
}

// [[Rcpp::export]]
List get_convergence_diagnostics_cpp(SEXP runner_ptr) {
  XPtr<MCMCRunnerManual> runner(runner_ptr);
  return runner->get_convergence_diagnostics();
}
