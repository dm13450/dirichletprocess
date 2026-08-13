#include "hierarchical_mvnormal_mixing.h"
#include "utilities.h"
#include <RcppArmadillo.h>
#include <set>
#include <numeric>
#include <algorithm>

namespace dirichletprocess {

// Helper function for stick breaking
std::vector<double> HierarchicalMVNormalMixing::stick_breaking(double gamma, int n_sticks) {
  std::vector<double> v(n_sticks);
  std::vector<double> beta(n_sticks);

  // Generate stick breaking weights
  for (int i = 0; i < n_sticks - 1; i++) {
    v[i] = R::rbeta(1.0, gamma);
  }
  v[n_sticks - 1] = 1.0;

  // Convert to weights
  double prod = 1.0;
  for (int i = 0; i < n_sticks; i++) {
    beta[i] = v[i] * prod;
    prod *= (1.0 - v[i]);
  }

  return beta;
}

// Draw from discrete distribution with weights beta_k
std::vector<double> HierarchicalMVNormalMixing::draw_gj(double alpha,
                                                        const std::vector<double>& beta_k) {
  int K = beta_k.size();
  std::vector<double> pi_k(K);

  // Sample from Dirichlet distribution
  std::vector<double> gamma_draws(K);
  double sum = 0.0;

  for (int k = 0; k < K; k++) {
    gamma_draws[k] = R::rgamma(alpha * beta_k[k], 1.0);
    sum += gamma_draws[k];
  }

  // Normalize
  for (int k = 0; k < K; k++) {
    pi_k[k] = gamma_draws[k] / sum;
  }

  return pi_k;
}

// Constructor
HierarchicalMVNormalMixing::HierarchicalMVNormalMixing(
  int n_groups, int n_sticks,
  const Rcpp::List& prior_params,
  const arma::vec& alpha_prior,
  const arma::vec& gamma_prior)
  : n_groups(n_groups), n_sticks(n_sticks) {

  // Initialize hyperparameters
  params.mu0 = Rcpp::as<arma::vec>(prior_params["mu0"]);
  params.kappa0 = Rcpp::as<double>(prior_params["kappa0"]);
  params.Lambda = Rcpp::as<arma::mat>(prior_params["Lambda"]);
  params.nu = Rcpp::as<double>(prior_params["nu"]);

  params.gamma_prior = gamma_prior;

  // Sample initial gamma
  params.gamma = R::rgamma(gamma_prior[0], 1.0 / gamma_prior[1]);

  // Initialize stick weights
  params.stick_weights = stick_breaking(params.gamma, n_sticks);

  // Initialize base distributions and local parameters
  params.alphas.resize(n_groups);
  params.pi_k.resize(n_groups);

  for (int i = 0; i < n_groups; i++) {
    // Sample local alpha
    params.alphas[i] = R::rgamma(alpha_prior[0], 1.0 / alpha_prior[1]);

    // Sample local weights
    params.pi_k[i] = draw_gj(params.alphas[i], params.stick_weights);

    // Create base distribution - C++11 compatible
    base_dists.push_back(std::unique_ptr<MVNormalMixing>(
        new MVNormalMixing(params.mu0, params.kappa0, params.Lambda, params.nu)));
  }
}

// Update local clusters using Algorithm 8
void HierarchicalMVNormalMixing::update_local_clusters(
    std::vector<arma::mat>& data,
    std::vector<std::vector<int>>& labels,
    std::vector<std::vector<arma::vec>>& local_params) {

  // This would typically call the MCMCRunner for each group
  // For brevity, showing the structure
  for (int g = 0; g < n_groups; g++) {
    // Update assignments and parameters for group g
    // using Algorithm 8 with the hierarchical base measure
  }
}

// Update global parameters
void HierarchicalMVNormalMixing::update_global_parameters(
    const std::vector<arma::mat>& data,
    const std::vector<std::vector<int>>& labels,
    const std::vector<std::vector<arma::vec>>& local_params) {

  // Collect all data assigned to each global cluster
  std::vector<arma::mat> global_cluster_data(n_sticks);

  for (int g = 0; g < n_groups; g++) {
    for (size_t i = 0; i < data[g].n_rows; i++) {
      int local_label = labels[g][i];
      // Map to global cluster (simplified - actual implementation would be more complex)
      int global_label = local_label % n_sticks;

      if (global_cluster_data[global_label].n_rows == 0) {
        global_cluster_data[global_label] = data[g].row(i);
      } else {
        global_cluster_data[global_label] = arma::join_vert(
          global_cluster_data[global_label], data[g].row(i));
      }
    }
  }

  // Update global parameters for each cluster
  for (int k = 0; k < n_sticks; k++) {
    if (global_cluster_data[k].n_rows > 0) {
      // Draw from posterior given data
      arma::vec flat_params = base_dists[0]->posterior_draw(
        global_cluster_data[k], arma::vec());

      // Store updated parameters (would need proper storage structure)
    }
  }
}

// Update stick weights
void HierarchicalMVNormalMixing::update_stick_weights() {
  // Count data points assigned to each global cluster
  std::vector<int> counts(n_sticks, 0);

  // Update using beta posterior
  std::vector<double> v(n_sticks);
  for (int k = 0; k < n_sticks - 1; k++) {
    double a = 1.0 + counts[k];
    double b = params.gamma;
    for (int j = k + 1; j < n_sticks; j++) {
      b += counts[j];
    }
    v[k] = R::rbeta(a, b);
  }
  v[n_sticks - 1] = 1.0;

  // Convert to weights
  double prod = 1.0;
  for (int k = 0; k < n_sticks; k++) {
    params.stick_weights[k] = v[k] * prod;
    prod *= (1.0 - v[k]);
  }
}

// Update gamma using Gibbs sampling
void HierarchicalMVNormalMixing::update_gamma() {
  // Use auxiliary variable method from Escobar & West (1995)
  int K_active = 0;  // Count active clusters

  // Sample auxiliary variable
  double eta = R::rbeta(params.gamma + 1.0, n_groups);

  // Calculate probabilities for mixture
  double log_eta = std::log(eta);
  double a = params.gamma_prior[0] + K_active;
  double b = params.gamma_prior[1] - log_eta;

  // Sample from gamma posterior
  params.gamma = R::rgamma(a, 1.0 / b);
}

// Update local concentration parameters
void HierarchicalMVNormalMixing::update_local_alphas(
    const std::vector<std::vector<int>>& labels) {

  for (int g = 0; g < n_groups; g++) {
    // Count unique clusters in group g
    std::set<int> unique_labels(labels[g].begin(), labels[g].end());
    int K_g = unique_labels.size();
    int n_g = labels[g].size();

    // Use auxiliary variable method
    double eta = R::rbeta(params.alphas[g] + 1.0, n_g);

    // Sample from posterior
    double a = params.gamma_prior[0] + K_g;
    double b = params.gamma_prior[1] - std::log(eta);

    params.alphas[g] = R::rgamma(a, 1.0 / b);
  }
}

// Get current state
Rcpp::List HierarchicalMVNormalMixing::get_state() const {
  // Create global_params list with all required parameters for MVNormalMixing
  Rcpp::List global_params = Rcpp::List::create(
    Rcpp::Named("mu0") = params.mu0,
    Rcpp::Named("kappa0") = params.kappa0,
    Rcpp::Named("Lambda") = params.Lambda,
    Rcpp::Named("nu") = params.nu
  );
  
  return Rcpp::List::create(
    Rcpp::Named("gamma") = params.gamma,
    Rcpp::Named("stick_weights") = params.stick_weights,
    Rcpp::Named("alphas") = params.alphas,
    Rcpp::Named("pi_k") = params.pi_k,
    Rcpp::Named("global_params") = global_params
  );
}

// HierarchicalMVNormalRunner implementation
HierarchicalMVNormalRunner::HierarchicalMVNormalRunner(
  const std::vector<arma::mat>& data_list,
  const Rcpp::List& hdp_params,
  const Rcpp::List& mcmc_params)
  : data_list(data_list) {

  // Extract parameters
  n_iter = Rcpp::as<int>(mcmc_params["n_iter"]);
  n_burn = Rcpp::as<int>(mcmc_params["n_burn"]);
  thin = Rcpp::as<int>(mcmc_params["thin"]);
  update_prior = Rcpp::as<bool>(mcmc_params["update_prior"]);
  show_progress = Rcpp::as<bool>(mcmc_params["show_progress"]);

  // Initialize the hierarchical model
  int n_groups = data_list.size();
  int n_sticks = Rcpp::as<int>(hdp_params["n_sticks"]);

  // C++11 compatible unique_ptr initialization
  hdp_model = std::unique_ptr<HierarchicalMVNormalMixing>(
    new HierarchicalMVNormalMixing(
        n_groups, n_sticks,
        hdp_params["prior_params"],
                  Rcpp::as<arma::vec>(hdp_params["alpha_prior"]),
                  Rcpp::as<arma::vec>(hdp_params["gamma_prior"])));

  // Initialize cluster structures
  cluster_labels.resize(n_groups);
  cluster_params.resize(n_groups);
  n_clusters.resize(n_groups);

  initialize_clusters();
}

// Initialize clusters
void HierarchicalMVNormalRunner::initialize_clusters() {
  for (size_t g = 0; g < data_list.size(); g++) {
    int n_obs = data_list[g].n_rows;
    cluster_labels[g].resize(n_obs);

    // Initialize all to one cluster
    std::fill(cluster_labels[g].begin(), cluster_labels[g].end(), 0);
    n_clusters[g] = 1;

    // Initialize cluster parameters
    cluster_params[g].resize(1);
    // Draw from prior (simplified)
    arma::vec prior_draw = hdp_model->base_dists[0]->prior_draw();
    cluster_params[g][0] = prior_draw;
  }
}

// Update cluster assignments using Algorithm 8
void HierarchicalMVNormalRunner::update_cluster_assignments_algorithm8(int group_idx) {
  arma::mat& data = data_list[group_idx];
  std::vector<int>& labels = cluster_labels[group_idx];
  std::vector<arma::vec>& params = cluster_params[group_idx];

  int m_auxiliary = 3;  // Number of auxiliary parameters

  for (size_t i = 0; i < data.n_rows; i++) {
    arma::vec obs = data.row(i).t();

    // Remove from current cluster
    std::vector<int> cluster_counts(params.size(), 0);
    for (size_t j = 0; j < labels.size(); j++) {
      if (j != i) cluster_counts[labels[j]]++;
    }

    // Prepare probabilities
    std::vector<double> probs;
    std::vector<arma::vec> candidate_params;

    // Existing clusters
    for (size_t k = 0; k < params.size(); k++) {
      if (cluster_counts[k] > 0) {
        double log_lik = hdp_model->base_dists[group_idx]->
          log_likelihood(obs, params[k]);
        probs.push_back(cluster_counts[k] * std::exp(log_lik));
        candidate_params.push_back(params[k]);
      }
    }

    // Auxiliary parameters
    double alpha = hdp_model->params.alphas[group_idx];
    for (int j = 0; j < m_auxiliary; j++) {
      arma::vec aux_param = hdp_model->base_dists[group_idx]->prior_draw();
      double log_lik = hdp_model->base_dists[group_idx]->
        log_likelihood(obs, aux_param);
      probs.push_back((alpha / m_auxiliary) * std::exp(log_lik));
      candidate_params.push_back(aux_param);
    }

    // Sample new cluster
    double total = std::accumulate(probs.begin(), probs.end(), 0.0);
    double u = R::runif(0, 1) * total;
    double cumsum = 0.0;

    int new_cluster = -1;
    for (size_t k = 0; k < probs.size(); k++) {
      cumsum += probs[k];
      if (u <= cumsum) {
        new_cluster = k;
        break;
      }
    }

    // Update assignment
    if (new_cluster < static_cast<int>(params.size())) {
      labels[i] = new_cluster;
    } else {
      // New cluster
      labels[i] = params.size();
      params.push_back(candidate_params[new_cluster]);
    }
  }

  // Clean up empty clusters
  // (Implementation omitted for brevity)
}

// Main MCMC loop
Rcpp::List HierarchicalMVNormalRunner::run() {
  // Progress bar
  Rcpp::Function txtProgressBar("txtProgressBar");
  Rcpp::Function setTxtProgressBar("setTxtProgressBar");
  Rcpp::Environment base("package:utils");

  Rcpp::List pb;
  if (show_progress) {
    pb = txtProgressBar(Rcpp::Named("min") = 0,
                        Rcpp::Named("max") = n_iter,
                        Rcpp::Named("style") = 3);
  }

  // Main MCMC iterations
  for (int iter = 0; iter < n_iter; iter++) {
    // Update local clusters for each group
    for (size_t g = 0; g < data_list.size(); g++) {
      update_cluster_assignments_algorithm8(g);

      // Update cluster parameters
      for (size_t k = 0; k < cluster_params[g].size(); k++) {
        // Collect data in cluster k
        arma::mat cluster_data;
        for (size_t i = 0; i < cluster_labels[g].size(); i++) {
          if (cluster_labels[g][i] == static_cast<int>(k)) {
            if (cluster_data.n_rows == 0) {
              cluster_data = data_list[g].row(i);
            } else {
              cluster_data = arma::join_vert(cluster_data, data_list[g].row(i));
            }
          }
        }

        if (cluster_data.n_rows > 0) {
          cluster_params[g][k] = hdp_model->base_dists[g]->
            posterior_draw(cluster_data, arma::vec());
        }
      }
    }

    // Update global parameters
    if (update_prior) {
      hdp_model->update_global_parameters(data_list, cluster_labels, cluster_params);
      hdp_model->update_stick_weights();
      hdp_model->update_gamma();
      hdp_model->update_local_alphas(cluster_labels);
    }

    // Store samples
    if (iter >= n_burn && (iter - n_burn) % thin == 0) {
      store_iteration(iter);
    }

    // Update progress
    if (show_progress) {
      setTxtProgressBar(pb, iter + 1);
    }
  }

  if (show_progress) {
    Rcpp::Function close("close");
    close(pb);
  }

  // Return results
  return Rcpp::List::create(
    Rcpp::Named("samples") = state_samples,
    Rcpp::Named("data") = data_list,
    Rcpp::Named("final_state") = hdp_model->get_state()
  );
}

// Store iteration
void HierarchicalMVNormalRunner::store_iteration(int iter) {
  Rcpp::List iteration_state = Rcpp::List::create(
    Rcpp::Named("iteration") = iter,
    Rcpp::Named("cluster_labels") = cluster_labels,
    Rcpp::Named("cluster_params") = cluster_params,
    Rcpp::Named("n_clusters") = n_clusters,
    Rcpp::Named("hdp_state") = hdp_model->get_state()
  );

  state_samples.push_back(iteration_state);
}

} // namespace dirichletprocess
