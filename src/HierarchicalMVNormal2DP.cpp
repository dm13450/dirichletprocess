// src/HierarchicalMVNormal2DP.cpp
#include "HierarchicalDP.h"
#include "MVNormal2Distribution.h"
#include "RcppConversions.h"
#include <RcppArmadillo.h>

namespace dp {

// HierarchicalMVNormal2DP implementation
HierarchicalMVNormal2DP::HierarchicalMVNormal2DP() {
  // Constructor
}

HierarchicalMVNormal2DP::~HierarchicalMVNormal2DP() {
  // Clean up individual DPs
  for (auto& dp : indDP) {
    if (dp) {
      delete dp;
      dp = nullptr;
    }
  }
}

HierarchicalMVNormal2DP* HierarchicalMVNormal2DP::fromR(const Rcpp::List& rObj) {
  HierarchicalMVNormal2DP* hdp = new HierarchicalMVNormal2DP();

  if (rObj.containsElementNamed("indDP")) {
    Rcpp::List indDP_list = rObj["indDP"];

    for (int i = 0; i < indDP_list.size(); i++) {
      Rcpp::List dp_obj = indDP_list[i];

      // Create NonConjugateMVNormal2DP from R object
      NonConjugateMVNormal2DP* mvn2DP = new NonConjugateMVNormal2DP();

      // Set common DP properties
      mvn2DP->data = Rcpp::as<arma::mat>(dp_obj["data"]);
      mvn2DP->n = mvn2DP->data.n_rows;
      mvn2DP->alpha = Rcpp::as<double>(dp_obj["alpha"]);
      mvn2DP->alphaPriorParameters = dp_obj["alphaPriorParameters"];
      mvn2DP->mhDraws = dp_obj.containsElementNamed("mhDraws") ?
      Rcpp::as<int>(dp_obj["mhDraws"]) : 100;

      // Set cluster information
      mvn2DP->clusterLabels = Rcpp::as<arma::uvec>(dp_obj["clusterLabels"]) - 1; // Convert to 0-indexed
      mvn2DP->pointsPerCluster = Rcpp::as<arma::uvec>(dp_obj["pointsPerCluster"]);
      mvn2DP->numberClusters = Rcpp::as<int>(dp_obj["numberClusters"]);
      mvn2DP->clusterParameters = dp_obj["clusterParameters"];
      mvn2DP->m = dp_obj.containsElementNamed("m") ? Rcpp::as<int>(dp_obj["m"]) : 3;

      // Create mixing distribution
      Rcpp::List mixDist = dp_obj["mixingDistribution"];
      mvn2DP->mixingDistribution = new MVNormal2MixingDistribution(
        Rcpp::as<Rcpp::List>(mixDist["priorParameters"]));

      hdp->indDP.push_back(mvn2DP);
    }
  }

  if (rObj.containsElementNamed("globalParameters")) {
    hdp->globalParameters = rObj["globalParameters"];
  }

  if (rObj.containsElementNamed("globalStick")) {
    hdp->globalStick = Rcpp::as<arma::vec>(rObj["globalStick"]);
  }

  if (rObj.containsElementNamed("gamma")) {
    hdp->gamma = Rcpp::as<double>(rObj["gamma"]);
  }

  if (rObj.containsElementNamed("gammaPriors")) {
    hdp->gammaPriors = Rcpp::as<Rcpp::NumericVector>(rObj["gammaPriors"]);
  }

  return hdp;
}

void HierarchicalMVNormal2DP::fit(int iterations, bool updatePrior, bool progressBar) {
  if (progressBar) {
    Rcpp::Rcout << "Starting Hierarchical MVNormal2 DP fitting..." << std::endl;
  }

  // Store chain values
  Rcpp::NumericVector gammaValues(iterations);

  for (int iter = 0; iter < iterations; iter++) {
    // Update components
    clusterComponentUpdate();
    updateAlpha();
    globalParameterUpdate();
    updateG0();
    updateGamma();

    // Store gamma value
    gammaValues[iter] = gamma;

    // Update prior if requested
    if (updatePrior && indDP.size() > 0) {
      // For MVNormal2, the prior update would be more complex
      // For now, we'll skip this as it's not typically done for MVNormal2
    }

    if (progressBar && ((iter + 1) % (iterations / 10) == 0 || iter == iterations - 1)) {
      Rcpp::Rcout << "Iteration " << iter + 1 << "/" << iterations << std::endl;
    }
  }

  if (progressBar) {
    Rcpp::Rcout << "Hierarchical MVNormal2 DP fitting complete." << std::endl;
  }
}

void HierarchicalMVNormal2DP::clusterComponentUpdate() {
  // For hierarchical DP, we need to update each individual DP
  for (size_t i = 0; i < indDP.size(); i++) {
    if (indDP[i]) {
      indDP[i]->clusterComponentUpdate();

      // Note: The R version also calls DuplicateClusterRemove here
      // We might need to implement that as well if needed
    }
  }
}

void HierarchicalMVNormal2DP::globalParameterUpdate() {
  // Get unique global labels across all DPs
  std::vector<int> all_global_labels;

  for (size_t i = 0; i < indDP.size(); i++) {
    NonConjugateMVNormal2DP* mvn2DP = dynamic_cast<NonConjugateMVNormal2DP*>(indDP[i]);
    if (!mvn2DP) continue;

    // Match cluster parameters to global parameters
    Rcpp::NumericVector mu_params = mvn2DP->clusterParameters[0];
    Rcpp::NumericVector mu_global = globalParameters[0];

    // Get dimensions
    Rcpp::IntegerVector mu_dim = mu_params.attr("dim");
    int d = mu_dim[1];

    for (int j = 0; j < mvn2DP->numberClusters; j++) {
      // Find which global parameter this cluster corresponds to
      for (int k = 0; k < mu_global.size() / d; k++) {
        bool match = true;
        for (int dim = 0; dim < d; dim++) {
          if (std::abs(mu_params[dim + j * d] - mu_global[dim + k * d]) > 1e-10) {
            match = false;
            break;
          }
        }
        if (match) {
          all_global_labels.push_back(k);
          break;
        }
      }
    }
  }

  // Get unique labels
  std::sort(all_global_labels.begin(), all_global_labels.end());
  all_global_labels.erase(std::unique(all_global_labels.begin(), all_global_labels.end()),
                          all_global_labels.end());

  // Update each global parameter
  for (int global_idx : all_global_labels) {
    // Collect all data points assigned to this global parameter
    arma::mat combined_data;
    int total_points = 0;

    for (size_t dp_idx = 0; dp_idx < indDP.size(); dp_idx++) {
      NonConjugateMVNormal2DP* mvn2DP = dynamic_cast<NonConjugateMVNormal2DP*>(indDP[dp_idx]);
      if (!mvn2DP) continue;

      Rcpp::NumericVector mu_params = mvn2DP->clusterParameters[0];
      Rcpp::NumericVector mu_global = globalParameters[0];

      // Get dimensions
      Rcpp::IntegerVector mu_dim = mu_params.attr("dim");
      int d = mu_dim[1];

      // Find clusters in this DP that use this global parameter
      for (int j = 0; j < mvn2DP->numberClusters; j++) {
        bool match = true;
        for (int dim = 0; dim < d; dim++) {
          if (std::abs(mu_params[dim + j * d] - mu_global[dim + global_idx * d]) > 1e-10) {
            match = false;
            break;
          }
        }

        if (match) {
          // Get data points for this cluster
          arma::uvec cluster_indices = arma::find(mvn2DP->clusterLabels == j);
          if (cluster_indices.n_elem > 0) {
            if (total_points == 0) {
              combined_data = mvn2DP->data.rows(cluster_indices);
            } else {
              combined_data = arma::join_vert(combined_data, mvn2DP->data.rows(cluster_indices));
            }
            total_points += cluster_indices.n_elem;
          }
        }
      }
    }

    if (total_points > 0) {
      // Draw new parameters from posterior using combined data
      MVNormal2MixingDistribution* mixDist = dynamic_cast<MVNormal2MixingDistribution*>(
        indDP[0]->getMixingDistribution());

      if (mixDist) {
        // Use single sample for efficiency in hierarchical MCMC
        Rcpp::List new_params = mixDist->posteriorDraw(combined_data, 1);
        Rcpp::NumericVector new_mu = new_params[0];
        Rcpp::NumericVector new_sig = new_params[1];

        // Get dimensions
        Rcpp::IntegerVector mu_dim = new_mu.attr("dim");
        int d = mu_dim[1];

        // Update global parameters with last sample
        Rcpp::NumericVector mu_global = globalParameters[0];
        Rcpp::NumericVector sig_global = globalParameters[1];

        int sample_idx = 0; // Single sample
        for (int dim = 0; dim < d; dim++) {
          mu_global[dim + global_idx * d] = new_mu[dim + sample_idx * d];
        }
        for (int i = 0; i < d; i++) {
          for (int j = 0; j < d; j++) {
            sig_global[i + j * d + global_idx * d * d] =
              new_sig[i + j * d + sample_idx * d * d];
          }
        }

        globalParameters[0] = mu_global;
        globalParameters[1] = sig_global;

        // Update individual DP parameters
        for (size_t dp_idx = 0; dp_idx < indDP.size(); dp_idx++) {
          NonConjugateMVNormal2DP* mvn2DP = dynamic_cast<NonConjugateMVNormal2DP*>(indDP[dp_idx]);
          if (!mvn2DP) continue;

          Rcpp::NumericVector mu_params = mvn2DP->clusterParameters[0];
          Rcpp::NumericVector sig_params = mvn2DP->clusterParameters[1];

          for (int j = 0; j < mvn2DP->numberClusters; j++) {
            bool match = true;
            for (int dim = 0; dim < d; dim++) {
              if (std::abs(mu_params[dim + j * d] - mu_global[dim + global_idx * d]) > 1e-10) {
                match = false;
                break;
              }
            }

            if (match) {
              for (int dim = 0; dim < d; dim++) {
                mu_params[dim + j * d] = new_mu[dim + sample_idx * d];
              }
              for (int i = 0; i < d; i++) {
                for (int jj = 0; jj < d; jj++) {
                  sig_params[i + jj * d + j * d * d] =
                    new_sig[i + jj * d + sample_idx * d * d];
                }
              }
            }
          }

          mvn2DP->clusterParameters[0] = mu_params;
          mvn2DP->clusterParameters[1] = sig_params;
        }
      }
    }
  }
}


void HierarchicalMVNormal2DP::updateGamma() {
  // Get the number of unique global parameters
  std::set<int> unique_global_labels;

  for (size_t i = 0; i < indDP.size(); i++) {
    NonConjugateMVNormal2DP* mvn2DP = dynamic_cast<NonConjugateMVNormal2DP*>(indDP[i]);
    if (!mvn2DP) continue;

    Rcpp::NumericVector mu_params = mvn2DP->clusterParameters[0];
    Rcpp::NumericVector mu_global = globalParameters[0];

    // Get dimensions
    Rcpp::IntegerVector mu_dim = mu_params.attr("dim");
    int d = mu_dim[1];

    for (int j = 0; j < mvn2DP->numberClusters; j++) {
      for (int k = 0; k < mu_global.size() / d; k++) {
        bool match = true;
        for (int dim = 0; dim < d; dim++) {
          if (std::abs(mu_params[dim + j * d] - mu_global[dim + k * d]) > 1e-10) {
            match = false;
            break;
          }
        }
        if (match) {
          unique_global_labels.insert(k);
          break;
        }
      }
    }
  }

  int numParams = unique_global_labels.size();
  int numTables = 0;

  // Count total number of tables (clusters across all DPs)
  for (auto& dp : indDP) {
    NonConjugateMVNormal2DP* mvn2DP = dynamic_cast<NonConjugateMVNormal2DP*>(dp);
    if (mvn2DP) {
      numTables += mvn2DP->numberClusters;
    }
  }

  // Update gamma using the same logic as in R
  double x = R::rbeta(gamma + 1.0, numTables);
  double log_x = std::log(x);

  double pi1 = gammaPriors[0] + numParams - 1.0;
  double pi2 = numTables * (gammaPriors[1] - log_x);

  double pi_val = pi1 / (pi1 + pi2);
  if (!std::isfinite(pi_val)) {
    pi_val = 0.5;
  }

  double postShape;
  if (R::runif(0, 1) < pi_val) {
    postShape = gammaPriors[0] + numParams;
  } else {
    postShape = gammaPriors[0] + numParams - 1.0;
  }

  double postRate = gammaPriors[1] - log_x;
  if (postRate <= 0) postRate = 1e-6;

  gamma = R::rgamma(postShape, 1.0 / postRate);
  if (gamma <= 0) gamma = 1e-6;
}

void HierarchicalMVNormal2DP::updateG0() {
  // Get global parameters and their frequencies
  std::map<int, int> global_param_counts;

  for (size_t i = 0; i < indDP.size(); i++) {
    NonConjugateMVNormal2DP* mvn2DP = dynamic_cast<NonConjugateMVNormal2DP*>(indDP[i]);
    if (!mvn2DP) continue;

    Rcpp::NumericVector mu_params = mvn2DP->clusterParameters[0];
    Rcpp::NumericVector mu_global = globalParameters[0];

    // Get dimensions
    Rcpp::IntegerVector mu_dim = mu_params.attr("dim");
    int d = mu_dim[1];

    for (int j = 0; j < mvn2DP->numberClusters; j++) {
      for (int k = 0; k < mu_global.size() / d; k++) {
        bool match = true;
        for (int dim = 0; dim < d; dim++) {
          if (std::abs(mu_params[dim + j * d] - mu_global[dim + k * d]) > 1e-10) {
            match = false;
            break;
          }
        }
        if (match) {
          global_param_counts[k]++;
          break;
        }
      }
    }
  }

  int num_tables = global_param_counts.size();
  if (num_tables == 0) return;

  // Get frequencies
  Rcpp::NumericVector frequencies(num_tables);
  int idx = 0;
  for (auto& pair : global_param_counts) {
    frequencies[idx++] = pair.second;
  }

  // Draw from Dirichlet distribution
  Rcpp::NumericVector dirichlet_params = Rcpp::NumericVector::create();
  for (int i = 0; i < num_tables; i++) {
    dirichlet_params.push_back(frequencies[i]);
  }
  dirichlet_params.push_back(gamma);

  // Use R's rdirichlet through Rcpp
  Rcpp::Environment gtools("package:gtools");
  Rcpp::Function rdirichlet = gtools["rdirichlet"];
  Rcpp::NumericMatrix dirichlet_draw = rdirichlet(1, dirichlet_params);
  Rcpp::NumericVector weights = dirichlet_draw(0, Rcpp::_);

  // Update stick breaking weights
  int num_breaks = std::ceil(gamma + num_tables) * 20 + 5;
  globalStick.set_size(num_tables + num_breaks);

  // Existing table weights
  for (int i = 0; i < num_tables; i++) {
    globalStick[i] = weights[i];
  }

  // New table weights from stick breaking
  double remaining_weight = weights[num_tables];
  for (int i = 0; i < num_breaks; i++) {
    double beta = R::rbeta(1.0, gamma + num_tables);
    globalStick[num_tables + i] = beta * remaining_weight;
    remaining_weight *= (1.0 - beta);
  }

  // Draw new parameters for the additional breaks
  MVNormal2MixingDistribution* mixDist = dynamic_cast<MVNormal2MixingDistribution*>(
    indDP[0]->getMixingDistribution());

  if (mixDist) {
    Rcpp::List new_params = mixDist->priorDraw(num_breaks);

    // Expand global parameters
    Rcpp::NumericVector mu_global = globalParameters[0];
    Rcpp::NumericVector sig_global = globalParameters[1];
    Rcpp::NumericVector new_mu = new_params[0];
    Rcpp::NumericVector new_sig = new_params[1];

    // Get dimensions
    Rcpp::IntegerVector mu_dim = new_mu.attr("dim");
    int d = mu_dim[1];

    // Create expanded arrays
    int current_size = mu_global.size() / d;
    Rcpp::NumericVector expanded_mu = Rcpp::NumericVector(Rcpp::Dimension(1, d, current_size + num_breaks));
    Rcpp::NumericVector expanded_sig = Rcpp::NumericVector(Rcpp::Dimension(d, d, current_size + num_breaks));

    // Copy existing parameters
    for (int k = 0; k < current_size; k++) {
      for (int j = 0; j < d; j++) {
        expanded_mu[j + k * d] = mu_global[j + k * d];
      }
      for (int i = 0; i < d; i++) {
        for (int j = 0; j < d; j++) {
          expanded_sig[i + j * d + k * d * d] = sig_global[i + j * d + k * d * d];
        }
      }
    }

    // Add new parameters
    for (int k = 0; k < num_breaks; k++) {
      for (int j = 0; j < d; j++) {
        expanded_mu[j + (current_size + k) * d] = new_mu[j + k * d];
      }
      for (int i = 0; i < d; i++) {
        for (int j = 0; j < d; j++) {
          expanded_sig[i + j * d + (current_size + k) * d * d] = new_sig[i + j * d + k * d * d];
        }
      }
    }

    globalParameters[0] = expanded_mu;
    globalParameters[1] = expanded_sig;
  }
}

} // namespace dp
