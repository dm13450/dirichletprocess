// src/HierarchicalBetaDP.cpp
#include "HierarchicalDP.h"
#include "BetaDistribution.h"
#include "RcppConversions.h"
#include <RcppArmadillo.h>
#include <memory>

namespace dp {

// HierarchicalBetaDP implementation
HierarchicalBetaDP::HierarchicalBetaDP() {
  // Constructor
}

HierarchicalBetaDP::~HierarchicalBetaDP() {
  // Clean up individual DPs
  for (auto& dp : indDP) {
    if (dp) {
      delete dp;
      dp = nullptr;
    }
  }
}

HierarchicalBetaDP* HierarchicalBetaDP::fromR(const Rcpp::List& rObj) {
  HierarchicalBetaDP* hdp = new HierarchicalBetaDP();

  try {
    if (rObj.containsElementNamed("indDP")) {
      Rcpp::List indDP_list = rObj["indDP"];

      for (int i = 0; i < indDP_list.size(); i++) {
        Rcpp::List dp_obj = indDP_list[i];

        // Create NonConjugateBetaDP from R object using smart pointer
        std::unique_ptr<NonConjugateBetaDP> betaDP(new NonConjugateBetaDP());

        // Set common DP properties with validation
        if (dp_obj.containsElementNamed("data")) {
          betaDP->data = Rcpp::as<arma::mat>(dp_obj["data"]);
          betaDP->n = betaDP->data.n_rows;
        } else {
          throw Rcpp::exception("Missing 'data' in DP object");
        }

        betaDP->alpha = dp_obj.containsElementNamed("alpha") ?
        Rcpp::as<double>(dp_obj["alpha"]) : 1.0;

        betaDP->alphaPriorParameters = dp_obj.containsElementNamed("alphaPriorParameters") ?
        Rcpp::as<Rcpp::NumericVector>(dp_obj["alphaPriorParameters"]) : Rcpp::NumericVector::create(1.0, 1.0);

        betaDP->mhDraws = dp_obj.containsElementNamed("mhDraws") ?
        Rcpp::as<int>(dp_obj["mhDraws"]) : 250;

        // Set cluster information with bounds checking
        if (dp_obj.containsElementNamed("clusterLabels")) {
          SEXP labels_sexp = dp_obj["clusterLabels"];
          if (!Rf_isNull(labels_sexp)) {
            arma::uvec labels = Rcpp::as<arma::uvec>(labels_sexp);
            if (labels.size() > 0) {
              // Labels should already be 0-indexed from R wrapper
              // Just check they're not negative
              if (labels.min() < 0) {
                throw Rcpp::exception("Invalid cluster labels (must be >= 0 after conversion)");
              }
              betaDP->clusterLabels = labels;  // Already 0-indexed
            } else {
              // Empty labels vector
              betaDP->clusterLabels = arma::uvec();
            }
          } else {
            // NULL labels - initialize as empty
            betaDP->clusterLabels = arma::uvec();
          }
        } else {
          // No cluster labels - initialize as empty
          betaDP->clusterLabels = arma::uvec();
        }

        if (dp_obj.containsElementNamed("pointsPerCluster")) {
          betaDP->pointsPerCluster = Rcpp::as<arma::uvec>(dp_obj["pointsPerCluster"]);
        }

        betaDP->numberClusters = dp_obj.containsElementNamed("numberClusters") ?
        Rcpp::as<int>(dp_obj["numberClusters"]) : 1;

        if (dp_obj.containsElementNamed("clusterParameters")) {
          betaDP->clusterParameters = Rcpp::as<Rcpp::List>(dp_obj["clusterParameters"]);
        }

        betaDP->m = dp_obj.containsElementNamed("m") ?
        Rcpp::as<int>(dp_obj["m"]) : 3;

        // Create mixing distribution with validation
        if (dp_obj.containsElementNamed("mixingDistribution")) {
          Rcpp::List mixDist_obj = Rcpp::as<Rcpp::List>(dp_obj["mixingDistribution"]);

          if (mixDist_obj.containsElementNamed("priorParameters")) {
            betaDP->mixingDistribution = std::unique_ptr<BetaMixingDistribution>(
              new BetaMixingDistribution(Rcpp::as<Rcpp::NumericVector>(mixDist_obj["priorParameters"])));

            betaDP->mixingDistribution->maxT = mixDist_obj.containsElementNamed("maxT") ?
            Rcpp::as<double>(mixDist_obj["maxT"]) : 1.0;

            if (mixDist_obj.containsElementNamed("mhStepSize")) {
              betaDP->mixingDistribution->mhStepSize = Rcpp::as<Rcpp::NumericVector>(mixDist_obj["mhStepSize"]);
            }
          } else {
            throw Rcpp::exception("Missing prior parameters in mixing distribution");
          }
        } else {
          throw Rcpp::exception("Missing mixing distribution");
        }

        hdp->indDP.push_back(betaDP.release());
      }
    }

    // Copy global parameters with validation
    if (rObj.containsElementNamed("globalParameters")) {
      hdp->globalParameters = Rcpp::as<Rcpp::List>(rObj["globalParameters"]);
    }

    if (rObj.containsElementNamed("globalStick")) {
      hdp->globalStick = Rcpp::as<arma::vec>(rObj["globalStick"]);
    }

    if (rObj.containsElementNamed("gamma")) {
      hdp->gamma = Rcpp::as<double>(rObj["gamma"]);
      if (hdp->gamma <= 0) {
        throw Rcpp::exception("gamma must be positive");
      }
    }

    if (rObj.containsElementNamed("gammaPriors")) {
      hdp->gammaPriors = Rcpp::as<Rcpp::NumericVector>(rObj["gammaPriors"]);
    }

    return hdp;

  } catch (std::exception& e) {
    // Clean up on error
    delete hdp;
    Rcpp::Rcerr << "Exception in HierarchicalBetaDP::fromR: " << e.what() << std::endl;
    throw;
  }
}

void HierarchicalBetaDP::globalParameterUpdate() {
  // Get unique global labels across all DPs
  std::vector<int> all_global_labels;

  // Use a more reasonable tolerance for parameter matching
  const double PARAM_TOLERANCE = 1e-6;  // Changed from 1e-10

  for (size_t i = 0; i < indDP.size(); i++) {
    NonConjugateBetaDP* betaDP = dynamic_cast<NonConjugateBetaDP*>(indDP[i]);
    if (!betaDP) continue;

    // Match cluster parameters to global parameters
    Rcpp::NumericVector mu_params = betaDP->clusterParameters[0];
    Rcpp::NumericVector mu_global = globalParameters[0];

    for (int j = 0; j < betaDP->numberClusters; j++) {
      // Find which global parameter this cluster corresponds to
      bool found = false;
      for (int k = 0; k < mu_global.size(); k++) {
        if (std::abs(mu_params[j] - mu_global[k]) < PARAM_TOLERANCE) {
          all_global_labels.push_back(k);
          found = true;
          break;
        }
      }

      // If no match found, this might be a new cluster that hasn't been assigned yet
      if (!found) {
        // Find the closest global parameter
        double min_dist = std::numeric_limits<double>::infinity();
        int closest_idx = 0;
        for (int k = 0; k < mu_global.size(); k++) {
          double dist = std::abs(mu_params[j] - mu_global[k]);
          if (dist < min_dist) {
            min_dist = dist;
            closest_idx = k;
          }
        }
        // If the closest is still reasonably close, use it
        if (min_dist < 0.1) {  // More lenient threshold for assignment
          all_global_labels.push_back(closest_idx);
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
    std::vector<double> combined_data;

    for (size_t dp_idx = 0; dp_idx < indDP.size(); dp_idx++) {
      NonConjugateBetaDP* betaDP = dynamic_cast<NonConjugateBetaDP*>(indDP[dp_idx]);
      if (!betaDP) continue;

      Rcpp::NumericVector mu_params = betaDP->clusterParameters[0];
      Rcpp::NumericVector mu_global = globalParameters[0];

      // Find clusters in this DP that use this global parameter
      for (int j = 0; j < betaDP->numberClusters; j++) {
        if (std::abs(mu_params[j] - mu_global[global_idx]) < PARAM_TOLERANCE) {
          // Get data points for this cluster
          for (arma::uword i = 0; i < static_cast<arma::uword>(betaDP->n); i++) {
            if (betaDP->clusterLabels[i] == static_cast<arma::uword>(j)) {
              combined_data.push_back(betaDP->data(i, 0));
            }
          }
        }
      }
    }

    if (combined_data.size() > 0) {
      // Draw new parameters from posterior
      arma::mat data_mat(combined_data.size(), 1);
      for (size_t i = 0; i < combined_data.size(); i++) {
        data_mat(i, 0) = combined_data[i];
      }

      // Use single sample for efficiency in hierarchical MCMC
      Rcpp::List new_params = indDP[0]->getMixingDistribution()->posteriorDraw(data_mat, 1);
      Rcpp::NumericVector new_mu = new_params[0];
      Rcpp::NumericVector new_nu = new_params[1];

      // Update global parameters
      Rcpp::NumericVector mu_global = globalParameters[0];
      Rcpp::NumericVector nu_global = globalParameters[1];
      mu_global[global_idx] = new_mu[0]; // Single sample
      nu_global[global_idx] = new_nu[0];
      globalParameters[0] = mu_global;
      globalParameters[1] = nu_global;

      // Update individual DP parameters
      for (size_t dp_idx = 0; dp_idx < indDP.size(); dp_idx++) {
        NonConjugateBetaDP* betaDP = dynamic_cast<NonConjugateBetaDP*>(indDP[dp_idx]);
        if (!betaDP) continue;

        Rcpp::NumericVector mu_params = betaDP->clusterParameters[0];
        Rcpp::NumericVector nu_params = betaDP->clusterParameters[1];

        for (int j = 0; j < betaDP->numberClusters; j++) {
          if (std::abs(mu_params[j] - new_mu[0]) < PARAM_TOLERANCE ||
              std::abs(mu_params[j] - mu_global[global_idx]) < PARAM_TOLERANCE) {
            mu_params[j] = new_mu[0];
            nu_params[j] = new_nu[0];
          }
        }

        betaDP->clusterParameters[0] = mu_params;
        betaDP->clusterParameters[1] = nu_params;
      }
    }
  }
}

void HierarchicalBetaDP::updateGamma() {
  // Get the number of unique global parameters
  std::set<int> unique_global_labels;
  const double PARAM_TOLERANCE = 1e-6;  // Match the tolerance used in globalParameterUpdate

  for (size_t i = 0; i < indDP.size(); i++) {
    NonConjugateBetaDP* betaDP = dynamic_cast<NonConjugateBetaDP*>(indDP[i]);
    if (!betaDP) continue;

    Rcpp::NumericVector mu_params = betaDP->clusterParameters[0];
    Rcpp::NumericVector mu_global = globalParameters[0];

    for (int j = 0; j < betaDP->numberClusters; j++) {
      for (int k = 0; k < mu_global.size(); k++) {
        if (std::abs(mu_params[j] - mu_global[k]) < PARAM_TOLERANCE) {
          unique_global_labels.insert(k);
          break;
        }
      }
    }
  }

  int numParams = unique_global_labels.size();
  int numTables = 0;

  // Count total number of tables
  for (auto& dp : indDP) {
    NonConjugateBetaDP* betaDP = dynamic_cast<NonConjugateBetaDP*>(dp);
    if (betaDP) {
      numTables += betaDP->numberClusters;
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

void HierarchicalBetaDP::updateG0() {
  // Get global parameters and their frequencies
  std::map<int, int> global_param_counts;
  const double PARAM_TOLERANCE = 1e-6;  // Match the tolerance used in globalParameterUpdate

  for (size_t i = 0; i < indDP.size(); i++) {
    NonConjugateBetaDP* betaDP = dynamic_cast<NonConjugateBetaDP*>(indDP[i]);
    if (!betaDP) continue;

    Rcpp::NumericVector mu_params = betaDP->clusterParameters[0];
    Rcpp::NumericVector mu_global = globalParameters[0];

    for (int j = 0; j < betaDP->numberClusters; j++) {
      for (int k = 0; k < mu_global.size(); k++) {
        if (std::abs(mu_params[j] - mu_global[k]) < PARAM_TOLERANCE) {
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
  BetaMixingDistribution* betaMD = dynamic_cast<BetaMixingDistribution*>(
    dynamic_cast<NonConjugateBetaDP*>(indDP[0])->mixingDistribution.get());

  if (betaMD) {
    Rcpp::List new_params = betaMD->priorDraw(num_breaks);

    // Expand global parameters
    Rcpp::NumericVector mu_global = globalParameters[0];
    Rcpp::NumericVector nu_global = globalParameters[1];
    Rcpp::NumericVector new_mu = new_params[0];
    Rcpp::NumericVector new_nu = new_params[1];

    // Combine existing and new parameters
    Rcpp::NumericVector expanded_mu(mu_global.size() + num_breaks);
    Rcpp::NumericVector expanded_nu(nu_global.size() + num_breaks);

    for (int i = 0; i < mu_global.size(); i++) {
      expanded_mu[i] = mu_global[i];
      expanded_nu[i] = nu_global[i];
    }

    for (int i = 0; i < num_breaks; i++) {
      expanded_mu[mu_global.size() + i] = new_mu[i];
      expanded_nu[nu_global.size() + i] = new_nu[i];
    }

    globalParameters[0] = expanded_mu;
    globalParameters[1] = expanded_nu;
  }
}

void HierarchicalBetaDP::clusterComponentUpdate() {
  // Update cluster components for each individual DP
  for (auto& dp : indDP) {
    if (dp) {
      dp->clusterComponentUpdate();
    }
  }
}

void HierarchicalBetaDP::clusterParameterUpdate() {
  // Update cluster parameters for each individual DP
  for (auto& dp : indDP) {
    if (dp) {
      dp->clusterParameterUpdate();
    }
  }
}

void HierarchicalBetaDP::updateAlpha() {
  // Update alpha for each individual DP
  for (auto& dp : indDP) {
    if (dp) {
      dp->updateAlpha();
    }
  }
}

void HierarchicalBetaDP::fit(int iterations, bool updatePrior, bool progressBar) {
  if (progressBar) {
    Rcpp::Rcout << "Starting Hierarchical Beta DP fitting..." << std::endl;
  }

  // Store chain values by resizing the gammaChain member
  this->gammaChain = Rcpp::NumericVector(iterations);

  for (int iter = 0; iter < iterations; iter++) {
    // Update components
    clusterComponentUpdate();
    updateAlpha();
    globalParameterUpdate();
    updateG0();
    updateGamma();

    // Store gamma value
    this->gammaChain[iter] = gamma;

    // Update prior if requested
    if (updatePrior && indDP.size() > 0) {
      // Get all cluster parameters
      int total_clusters = 0;
      for (auto& dp : indDP) {
        NonConjugateBetaDP* betaDP = dynamic_cast<NonConjugateBetaDP*>(dp);
        if (betaDP) {
          total_clusters += betaDP->numberClusters;
        }
      }

      if (total_clusters > 0) {
        // Collect all nu parameters
        Rcpp::NumericVector all_nu;
        for (auto& dp : indDP) {
          NonConjugateBetaDP* betaDP = dynamic_cast<NonConjugateBetaDP*>(dp);
          if (betaDP) {
            Rcpp::List cluster_params_loop = betaDP->clusterParameters;
            if (cluster_params_loop.size() > 1) {
              Rcpp::NumericVector nu_params = Rcpp::as<Rcpp::NumericVector>(cluster_params_loop[1]);
              for (int i = 0; i < nu_params.size(); i++) {
                all_nu.push_back(nu_params[i]);
              }
            }
          }
        }

        // Update prior parameters using the first DP's mixing distribution
        if (indDP.empty() || !indDP[0]) {
          Rcpp::Rcerr << "indDP is empty or first element is null in fit()." << std::endl;
        } else {
          NonConjugateBetaDP* firstDP = dynamic_cast<NonConjugateBetaDP*>(indDP[0]);
          if (firstDP && firstDP->mixingDistribution) {
            Rcpp::List priorUpdateParams = Rcpp::List::create(
              Rcpp::Named("mu") = Rcpp::NumericVector(),
              Rcpp::Named("nu") = all_nu
            );
            firstDP->mixingDistribution->updatePriorParameters(priorUpdateParams, total_clusters);

            // Propagate updated prior to all DPs
            Rcpp::NumericVector newPrior = Rcpp::as<Rcpp::NumericVector>(
              firstDP->mixingDistribution->priorParameters);

            for (auto& dp_loop : indDP) {
              NonConjugateBetaDP* betaDP_loop = dynamic_cast<NonConjugateBetaDP*>(dp_loop);
              if (betaDP_loop && betaDP_loop->mixingDistribution) {
                betaDP_loop->mixingDistribution->priorParameters = newPrior;
              }
            }
          }
        }
      }
    }

    if (progressBar && (iterations == 0 || (iter + 1) % (iterations / 10) == 0 || iter == iterations - 1)) {
      Rcpp::Rcout << "Iteration " << iter + 1 << "/" << iterations << std::endl;
    }
  }

  if (progressBar) {
    Rcpp::Rcout << "Hierarchical Beta DP fitting complete." << std::endl;
  }
}

Rcpp::List HierarchicalBetaDP::toR() const {
  Rcpp::List result;

  // Convert individual DPs
  Rcpp::List indDP_list;
  for (size_t i = 0; i < indDP.size(); i++) {
    NonConjugateBetaDP* betaDP = dynamic_cast<NonConjugateBetaDP*>(const_cast<DirichletProcess*>(indDP[i]));
    if (betaDP) {
      // Convert cluster labels back to 1-indexed for R (already handled in wrapper)
      indDP_list.push_back(betaDP->toR());
    }
  }
  result["indDP"] = indDP_list;

  // Copy global parameters
  result["globalParameters"] = Rcpp::clone(globalParameters);
  result["globalStick"] = Rcpp::wrap(globalStick);
  result["gamma"] = gamma;
  result["gammaPriors"] = Rcpp::clone(gammaPriors);

  // IMPORTANT: Include the gamma chain values
  if (gammaChain.size() > 0) {
    result["gammaValues"] = Rcpp::clone(gammaChain);
  }

  // Set the class attribute
  result.attr("class") = Rcpp::CharacterVector::create("list", "dirichletprocess", "hierarchical");

  return result;
}

} // namespace dp
