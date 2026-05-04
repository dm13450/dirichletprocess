#include "hierarchical_mcmc_runner.h"

namespace dirichletprocess {

HierarchicalMCMCRunner::HierarchicalMCMCRunner(
  const std::vector<arma::mat>& datasets,
  const Rcpp::List& mixing_dist_params,
  const Rcpp::List& mcmc_params)
  : datasets(datasets) {

  // Extract MCMC parameters
  n_iter = Rcpp::as<int>(mcmc_params["n_iter"]);
  n_burn = Rcpp::as<int>(mcmc_params["n_burn"]);
  thin = Rcpp::as<int>(mcmc_params["thin"]);
  update_prior = Rcpp::as<bool>(mcmc_params["update_prior"]);

  // Create hierarchical mixing distribution
  double alpha0 = Rcpp::as<double>(mixing_dist_params["alpha0"]);
  double beta0 = Rcpp::as<double>(mixing_dist_params["beta0"]);
  double maxT = Rcpp::as<double>(mixing_dist_params["maxT"]);

  hierarchical_mixing_dist.reset(
    new HierarchicalBetaMixing(alpha0, beta0, maxT));

  // Create individual MCMC runners for each dataset
  for (const auto& data : datasets) {
    // Create parameters for individual runner
    Rcpp::List individual_params = Rcpp::clone(mixing_dist_params);
    individual_params["type"] = "beta";

    runners.emplace_back(
      new MCMCRunner(data, individual_params, mcmc_params));
  }

  // Pre-allocate storage
  gamma_samples.reserve(n_iter);
  global_param_samples.reserve(n_iter);
}

Rcpp::List HierarchicalMCMCRunner::run() {
  Rcpp::Rcout << "Starting Hierarchical Beta MCMC with "
              << datasets.size() << " datasets" << std::endl;

  // Initialize all runners (single initialization, not full run)
  for (auto& runner : runners) {
    runner->initialize_state();
  }

  // Main MCMC loop
  for (int iter = 0; iter < n_iter; iter++) {
    Rcpp::checkUserInterrupt();

    // Step 1: Update local clusters using single iteration updates
    update_local_clusters();

    // Step 2: Update global parameters
    update_global_parameters();

    // Step 3: Update gamma (concentration parameter for G0)
    update_gamma();

    // Step 4: Propagate updated G0 to local DPs
    propagate_g0_to_local();

    // Store samples after burn-in
    if (iter >= n_burn && (iter - n_burn) % thin == 0) {
      store_iteration(iter);
    }

    // Progress reporting
    if ((iter + 1) % 100 == 0) {
      Rcpp::Rcout << "Iteration " << (iter + 1) << "/" << n_iter << std::endl;
    }
  }

  // Compile results
  Rcpp::List results;

  // Individual DP results - extract final state, don't run again
  Rcpp::List individual_results;
  for (size_t i = 0; i < runners.size(); i++) {
    // Extract current state instead of running full MCMC
    Rcpp::List dp_result;
    const auto& state = runners[i]->get_state();
    // mixing_dist available if needed for future extensions
    // const auto& mixing_dist = runners[i]->get_mixing_dist();

    // Convert state to R list format (cluster labels will be added later with proper 1-indexing)
    
    // Convert cluster parameters to R Beta format (mu/nu structure)
    if (!state->cluster_params.empty()) {
      int n_clusters = state->cluster_params.size();
      
      // Create mu and nu arrays in the format expected by R
      arma::cube mu_array(1, 1, n_clusters);
      arma::cube nu_array(1, 1, n_clusters);
      
      for (int k = 0; k < n_clusters; k++) {
        if (state->cluster_params[k].n_elem >= 2) {
          mu_array(0, 0, k) = state->cluster_params[k][0];  // mu parameter
          nu_array(0, 0, k) = state->cluster_params[k][1];  // nu parameter
        }
      }
      
      Rcpp::List cluster_parameters;
      cluster_parameters["mu"] = mu_array;
      cluster_parameters["nu"] = nu_array;
      dp_result["clusterParameters"] = cluster_parameters;
    }
    
    dp_result["alpha"] = state->alpha;
    dp_result["numberClusters"] = state->n_clusters;
    
    // Add additional required fields
    dp_result["pointsPerCluster"] = Rcpp::wrap(state->cluster_sizes);
    dp_result["n"] = static_cast<int>(datasets[i].n_rows);
    
    // Add the original data matrix 
    dp_result["data"] = datasets[i];
    
    // Calculate weights
    arma::vec weights = arma::conv_to<arma::vec>::from(state->cluster_sizes) / static_cast<double>(datasets[i].n_rows);
    dp_result["weights"] = weights;
    
    // Convert cluster labels to 1-indexed for R
    std::vector<int> r_labels(state->cluster_labels.size());
    for (size_t j = 0; j < state->cluster_labels.size(); j++) {
      r_labels[j] = state->cluster_labels[j] + 1;  // Convert to 1-indexed
    }
    dp_result["clusterLabels"] = r_labels;
    
    // Set proper class attributes for Beta DP objects
    dp_result.attr("class") = Rcpp::CharacterVector::create("beta", "nonconjugate", "dirichletprocess");
    
    individual_results.push_back(dp_result);
  }
  results["indDP"] = individual_results;

  // Hierarchical parameters
  results["gamma_samples"] = gamma_samples;
  results["global_parameters"] = global_param_samples;
  results["global_weights"] = hierarchical_mixing_dist->get_global_weights();

  return results;
}

void HierarchicalMCMCRunner::update_local_clusters() {
  // Each dataset updates its cluster assignments
  // This uses Algorithm 8 from Neal (2000)

  for (auto& runner : runners) {
    // Run one iteration of local MCMC (not full run)
    runner->single_iteration_update();
  }
}

void HierarchicalMCMCRunner::update_global_parameters() {
  // Collect all cluster parameters and data from local DPs
  std::vector<arma::mat> all_cluster_data;
  std::vector<arma::vec> all_cluster_params;

  // This is simplified - in practice, we need to track which local clusters
  // are assigned to which global clusters

  hierarchical_mixing_dist->update_global_parameters(
      all_cluster_data, all_cluster_params);
}

void HierarchicalMCMCRunner::update_gamma() {
  // Count unique clusters across all datasets
  int n_unique_global = hierarchical_mixing_dist->get_global_params().size();
  int n_total_obs = 0;

  for (const auto& data : datasets) {
    n_total_obs += data.n_rows;
  }

  hierarchical_mixing_dist->update_gamma(n_unique_global, n_total_obs);
}

void HierarchicalMCMCRunner::propagate_g0_to_local() {
  // Update the base distribution G0 in each local DP
  // This ensures all local DPs share the same G0

  // In practice, this would update the mixing distribution
  // in each runner to use the current G0
}

void HierarchicalMCMCRunner::store_iteration(int iter) {
  gamma_samples.push_back(hierarchical_mixing_dist->get_gamma());
  global_param_samples.push_back(hierarchical_mixing_dist->get_global_params());
}

} // namespace dirichletprocess
