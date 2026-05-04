# R/manual_mcmc_cpp.R

#' Manual C++ MCMC Runner Class
#'
#' @field ptr External pointer to C++ MCMCRunnerManual object
#' @field dp_obj Original Dirichlet process object
#' @field distribution_type Type of mixing distribution
#' @export
CppMCMCRunner <- setRefClass("CppMCMCRunner",
                             fields = list(
                               ptr = "externalptr",
                               dp_obj = "ANY",
                               distribution_type = "character",
                               temp_value = "numeric",
                               aux_params = "list"
                             ),

                             methods = list(
                               initialize = function(dp_object, n_iter = 1000, n_burn = 100, thin = 1) {
                                 # Initialize fields
                                 temp_value <<- 1.0
                                 aux_params <<- list()
                                 
                                 # Prepare data
                                 data <- as.matrix(dp_object$data)

                                 # Prepare parameters (works for ALL distributions)
                                 mixing_params <- prepare_mixing_dist_params(dp_object)
                                 mcmc_params <- prepare_mcmc_params(dp_object, n_iter, TRUE, n_burn, thin)

                                 # Create C++ runner
                                 ptr <<- create_mcmc_runner_cpp(data, mixing_params, mcmc_params)
                                 dp_obj <<- dp_object
                                 distribution_type <<- mixing_params$type
                               },

                               step_assignments = function() {
                                 "Update cluster assignments using Algorithm 8"
                                 step_assignments_cpp(ptr)
                                 invisible(.self)
                               },

                               step_parameters = function() {
                                 "Update cluster parameters"
                                 step_parameters_cpp(ptr)
                                 invisible(.self)
                               },

                               step_concentration = function() {
                                 "Update concentration parameter"
                                 step_concentration_cpp(ptr)
                                 invisible(.self)
                               },

                               perform_iteration = function() {
                                 "Perform a complete MCMC iteration"
                                 perform_iteration_cpp(ptr)
                                 invisible(.self)
                               },

                               get_state = function() {
                                 "Get current state of the sampler"
                                 get_state_cpp(ptr)
                               },

                               get_results = function() {
                                 "Get complete results"
                                 get_results_cpp(ptr)
                               },

                               set_labels = function(labels) {
                                 "Set cluster labels"
                                 # Convert to 0-based indexing for C++
                                 set_labels_cpp(ptr, labels - 1)
                                 invisible(.self)
                               },

                               set_params = function(params) {
                                 "Set cluster parameters"
                                 set_params_cpp(ptr, params)
                                 invisible(.self)
                               },

                               set_bounds = function(lower, upper) {
                                 "Set parameter bounds"
                                 set_parameter_bounds_cpp(ptr, lower, upper)
                                 invisible(.self)
                               },

                               set_update_flags = function(clusters = TRUE, params = TRUE, alpha = TRUE) {
                                 "Control which parameters are updated"
                                 set_update_flags_cpp(ptr, clusters, params, alpha)
                                 invisible(.self)
                               },

                               set_temperature = function(temp) {
                                 "Set temperature for annealed sampling"
                                 if (temp <= 0) {
                                   stop("Temperature must be positive")
                                 }
                                 temp_value <<- temp
                                 invisible(.self)
                               },

                               set_auxiliary_count = function(m) {
                                 "Set number of auxiliary parameters"
                                 if (m <= 0) {
                                   stop("Auxiliary count must be positive")
                                 }
                                 set_auxiliary_count_cpp(ptr, m)
                                 invisible(.self)
                               },

                               get_temperature = function() {
                                 "Get current temperature"
                                 temp_value
                               },

                               set_auxiliary_params = function(params) {
                                 "Set auxiliary parameters"
                                 aux_params <<- params
                                 invisible(.self)
                               },

                               get_n_clusters = function() {
                                 "Get number of clusters"
                                 state <- get_state()
                                 length(unique(state$labels))
                               },

                               merge_clusters = function(cluster1, cluster2) {
                                 "Merge two clusters"
                                 merge_clusters_cpp(ptr, cluster1, cluster2)
                                 invisible(.self)
                               },

                               split_cluster = function(cluster_id, split_prob = 0.5) {
                                 "Split a cluster"
                                 split_cluster_cpp(ptr, cluster_id, split_prob)
                                 invisible(.self)
                               },

                               get_auxiliary_params = function() {
                                 "Get auxiliary parameters"
                                 aux_params
                               },

                               get_cluster_likelihoods = function() {
                                 "Get cluster likelihoods"
                                 get_cluster_likelihoods_cpp(ptr)
                               },

                               get_membership_matrix = function() {
                                 "Get cluster membership matrix"
                                 get_membership_matrix_cpp(ptr)
                               },

                               get_cluster_statistics = function() {
                                 "Get cluster statistics"
                                 get_cluster_statistics_cpp(ptr)
                               },

                               sample_predictive = function(n_samples) {
                                 "Sample from posterior predictive"
                                 sample_predictive_cpp(ptr, n_samples)
                               },

                               get_log_posterior = function() {
                                 "Get log posterior"
                                 get_log_posterior_cpp(ptr)
                               },

                               get_cluster_entropies = function() {
                                 "Get cluster entropies"
                                 get_cluster_entropies_cpp(ptr)
                               },

                               get_clustering_entropy = function() {
                                 "Get clustering entropy"
                                 get_clustering_entropy_cpp(ptr)
                               },

                               get_convergence_diagnostics = function() {
                                 "Get convergence diagnostics"
                                 get_convergence_diagnostics_cpp(ptr)
                               },

                               get_iteration = function() {
                                 "Get current iteration number"
                                 state <- get_state()
                                 state$iteration
                               },

                               is_complete = function() {
                                 "Check if all iterations are complete"
                                 is_complete_cpp(ptr)
                               },

                               run = function() {
                                 "Run all iterations"
                                 while (!is_complete()) {
                                   perform_iteration()
                                 }

                                 results <- get_results()

                                 # Format results to match DP object structure
                                 results$numberClusters <- results$n_clusters
                                 results$clusterParameters <- results$cluster_params
                                 results$clusterLabels <- results$cluster_labels + 1  # R uses 1-based indexing
                                 results$alpha <- results$alpha
                                 results$data <- dp_obj$data
                                 results$mixingDistribution <- dp_obj$mixingDistribution

                                 class(results) <- c("dirichletprocess", "list")
                                 results
                               }
                             ))


#' Create Manual C++ MCMC Runner
#'
#' @param dp_obj Dirichlet process object (any distribution)
#' @param n_iter Number of iterations
#' @param n_burn Burn-in iterations
#' @param thin Thinning interval
#' @export
create_cpp_mcmc_runner <- function(dp_obj, n_iter = 1000, n_burn = 100, thin = 1) {

  # Check if C++ is available for this distribution
  if (!can_use_cpp(dp_obj)) {
    stop("C++ backend not available for distribution: ",
         class(dp_obj$mixingDistribution))
  }

  CppMCMCRunner$new(dp_obj, n_iter, n_burn, thin)
}
