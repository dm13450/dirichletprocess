// src/MarkovDP.cpp - Fixed compilation errors
#include "MarkovDP.h"
#include "RcppConversions.h"
#include <RcppArmadillo.h>
#include <algorithm>
#include <numeric>

namespace dp {

// Constructor
MarkovDP::MarkovDP() : beta(1.0), mixingDistribution(nullptr) {
  // Initialize with default values
}

// Constructor from R object with proper validation
MarkovDP::MarkovDP(const Rcpp::List& rObj) : DirichletProcess(rObj), beta(1.0), mixingDistribution(nullptr) {
  // Initialize base class is done by DirichletProcess(rObj)

  // Initialize Markov-specific members with validation
  if (rObj.containsElementNamed("states")) {
    Rcpp::IntegerVector r_states = Rcpp::as<Rcpp::IntegerVector>(rObj["states"]);

    // Validate states
    if (r_states.size() == 0) {
      Rcpp::stop("States vector cannot be empty");
    }

    // Check for NA values
    for (int i = 0; i < r_states.size(); i++) {
      if (Rcpp::IntegerVector::is_na(r_states[i])) {
        Rcpp::stop("States vector contains NA values");
      }
    }

    // Convert to arma::uvec with proper subtraction
    // First convert to std::vector, then subtract 1, then convert to arma::uvec
    std::vector<unsigned int> states_vec(r_states.size());
    for (int i = 0; i < r_states.size(); i++) {
      states_vec[i] = static_cast<unsigned int>(r_states[i] - 1);
    }
    states = arma::uvec(states_vec);
  }

  if (rObj.containsElementNamed("beta")) {
    beta = Rcpp::as<double>(rObj["beta"]);
    if (beta <= 0) {
      Rcpp::warning("Beta parameter should be positive, setting to 1.0");
      beta = 1.0;
    }
  }

  if (rObj.containsElementNamed("uniqueParams")) {
    uniqueParams = rObj["uniqueParams"];
  }

  // Initialize params with proper validation
  if (rObj.containsElementNamed("params")) {
    Rcpp::List rParams = rObj["params"];
    params.clear();
    params.reserve(rParams.size());

    for (int i = 0; i < rParams.size(); i++) {
      // Validate each parameter before adding
      if (Rcpp::is<Rcpp::List>(rParams[i])) {
        params.push_back(Rcpp::as<Rcpp::List>(rParams[i]));
      } else {
        // If not a list, create an empty list
        params.push_back(Rcpp::List());
      }
    }

    // Ensure params size matches data size
    if (params.size() != static_cast<size_t>(n)) {
      Rcpp::warning("Params size does not match data size, resizing...");
      params.resize(n);
    }
  } else {
    // Initialize empty params vector of correct size
    params.resize(n);
  }

  // Create mixing distribution with error handling
  if (rObj.containsElementNamed("mixingDistribution")) {
    Rcpp::List mdObj = rObj["mixingDistribution"];

    try {
      mixingDistribution = dp::createMDFromR(mdObj);

      if (!mixingDistribution) {
        Rcpp::stop("Failed to create mixing distribution");
      }
    } catch (const std::exception& e) {
      Rcpp::stop("Error creating mixing distribution: " + std::string(e.what()));
    }
  } else {
    Rcpp::stop("No mixing distribution specified in Markov DP object");
  }

  // Initialize params if they're empty
  for (size_t i = 0; i < params.size(); i++) {
    if (params[i].size() == 0 && mixingDistribution) {
      // Draw from prior for uninitialized params
      params[i] = mixingDistribution->priorDraw(1);
    }
  }
}

// Destructor
MarkovDP::~MarkovDP() {
  if (mixingDistribution) {
    delete mixingDistribution;
    mixingDistribution = nullptr;
  }
}

// Update states using Gibbs sampling
void MarkovDP::updateStates() {
  int n = data.n_rows;

  // Validate params vector size
  if (params.size() != static_cast<size_t>(n)) {
    // Resize params if needed
    params.resize(n);

    // Initialize empty params with proper structure
    for (size_t i = 0; i < params.size(); i++) {
      if (params[i].size() == 0) {
        // Draw from prior for uninitialized params
        Rcpp::List priorDraw = mixingDistribution->priorDraw(1);
        params[i] = priorDraw;
      }
    }
  }

  for (int i = 0; i < n; i++) {
    if (i == 0) {
      // First state can only transition from itself or state 2
      if (n > 1 && states[0] != states[1]) {
        // Count transitions for state 2
        int n_s2 = 0;
        for (int j = 1; j < n; j++) {
          if (states[j] == states[1]) n_s2++;
        }
        n_s2--; // Don't count the current state

        // Calculate weights
        double w1 = alpha / (beta + alpha);
        double w2 = (n_s2 + alpha) / (n_s2 + beta + alpha);

        // Calculate likelihoods
        Rcpp::NumericVector likelihoodValues(2);
        // Removed unused variable candidate_states

        for (int k = 0; k < 2; k++) {
          int state_idx = (k == 0) ? 0 : 1;  // Index into params array

          // Validate index
          if (state_idx >= static_cast<int>(params.size())) {
            Rcpp::stop("Invalid state index in updateStates");
          }

          // Get parameters for this candidate with proper validation
          Rcpp::List state_params = getValidatedParams(state_idx);

          arma::vec data_point = data.row(i).t();
          Rcpp::NumericVector likelihood_result = mixingDistribution->likelihood(data_point, state_params);

          if (likelihood_result.size() > 0) {
            likelihoodValues[k] = likelihood_result[0];
          } else {
            likelihoodValues[k] = 0.0;
          }
        }

        // Sample new state
        double wts[2] = {w1, w2};
        double probs[2];
        probs[0] = wts[0] * likelihoodValues[0];
        probs[1] = wts[1] * likelihoodValues[1];

        // Normalize
        double sum_probs = probs[0] + probs[1];
        if (sum_probs > 0) {
          probs[0] /= sum_probs;
          probs[1] /= sum_probs;
        } else {
          probs[0] = 0.5;
          probs[1] = 0.5;
        }

        // Sample
        double u = R::runif(0, 1);
        int newStateIdx = (u < probs[0]) ? 0 : 1;

        states[i] = states[newStateIdx];
        if (newStateIdx < static_cast<int>(params.size())) {
          params[i] = params[newStateIdx];
        }
      }
    }
    else if (i == n - 1) {
      // Last state
      if (i > 0 && states[i] != states[i-1]) {
        // Count transitions for previous state
        int n_sn1 = 0;
        for (int j = 0; j < n-1; j++) {
          if (states[j] == states[i-1]) n_sn1++;
        }
        n_sn1--; // Don't count the transition to current state

        // Calculate weights
        double w1 = n_sn1 + alpha;
        double w2 = beta;

        // Calculate likelihoods
        Rcpp::NumericVector likelihoodValues(2);
        int candidate_indices[2] = {i-1, i};

        for (int k = 0; k < 2; k++) {
          int idx = candidate_indices[k];

          // Get parameters for this candidate
          Rcpp::List state_params = getValidatedParams(idx);

          arma::vec data_point = data.row(i).t();
          Rcpp::NumericVector likelihood_result = mixingDistribution->likelihood(data_point, state_params);

          if (likelihood_result.size() > 0) {
            likelihoodValues[k] = likelihood_result[0];
          } else {
            likelihoodValues[k] = 0.0;
          }
        }

        // Sample new state
        double probs[2];
        probs[0] = w1 * likelihoodValues[0];
        probs[1] = w2 * likelihoodValues[1];

        // Normalize
        double sum_probs = probs[0] + probs[1];
        if (sum_probs > 0) {
          probs[0] /= sum_probs;
          probs[1] /= sum_probs;
        } else {
          probs[0] = 0.5;
          probs[1] = 0.5;
        }

        // Sample
        double u = R::runif(0, 1);
        int newStateIdx = (u < probs[0]) ? 0 : 1;

        states[i] = states[candidate_indices[newStateIdx]];
        params[i] = params[candidate_indices[newStateIdx]];
      }
    }
    else {
      // Middle states
      if (i > 0 && i < n - 1 && states[i-1] != states[i+1]) {
        // Count transitions
        int nii = 0;
        int nipip = 0;

        for (int j = 0; j < i; j++) {
          if (states[j] == states[i-1]) nii++;
        }
        nii--; // Don't count the transition to current state

        for (int j = i+1; j < n; j++) {
          if (states[j] == states[i+1]) nipip++;
        }
        nipip--; // Don't count the current state

        // Calculate weights
        double w1 = (nii + alpha) / (nii + 1 + beta + alpha);
        double w2 = (nipip + alpha) / (nipip + beta + alpha);

        // Calculate likelihoods
        Rcpp::NumericVector likelihoodValues(2);
        int candidate_indices[2] = {i-1, i+1};

        for (int k = 0; k < 2; k++) {
          int idx = candidate_indices[k];

          // Get parameters for this candidate
          Rcpp::List state_params = getValidatedParams(idx);

          arma::vec data_point = data.row(i).t();
          Rcpp::NumericVector likelihood_result = mixingDistribution->likelihood(data_point, state_params);

          if (likelihood_result.size() > 0) {
            likelihoodValues[k] = likelihood_result[0];
          } else {
            likelihoodValues[k] = 0.0;
          }
        }

        // Sample new state
        double probs[2];
        probs[0] = w1 * likelihoodValues[0];
        probs[1] = w2 * likelihoodValues[1];

        // Normalize
        double sum_probs = probs[0] + probs[1];
        if (sum_probs > 0) {
          probs[0] /= sum_probs;
          probs[1] /= sum_probs;
        } else {
          probs[0] = 0.5;
          probs[1] = 0.5;
        }

        // Sample
        double u = R::runif(0, 1);
        int newStateIdx = (u < probs[0]) ? 0 : 1;

        states[i] = states[candidate_indices[newStateIdx]];
        params[i] = params[candidate_indices[newStateIdx]];
      }
    }
  }

  // Relabel states to be contiguous and update params accordingly
  states = relabelStates(states);
}

// Helper method to get validated parameters
Rcpp::List MarkovDP::getValidatedParams(int idx) {
  Rcpp::List state_params;

  if (idx < 0 || idx >= static_cast<int>(params.size())) {
    // Return default parameters from prior
    return mixingDistribution->priorDraw(1);
  }

  state_params = params[idx];

  // Validate and format parameters based on distribution type
  if (mixingDistribution->distribution == "normal") {
    // Ensure proper structure for normal distribution
    if (!state_params.containsElementNamed("mu") || !state_params.containsElementNamed("sigma")) {
      Rcpp::List formatted_params;

      if (state_params.size() >= 2) {
        // Try to extract from array format
        try {
          Rcpp::NumericVector mu_vec = state_params[0];
          Rcpp::NumericVector sigma_vec = state_params[1];

          if (mu_vec.size() > 0 && sigma_vec.size() > 0) {
            // Extract the first value from each
            double mu_val = mu_vec[0];
            double sigma_val = sigma_vec[0];

            // Create properly formatted parameters
            Rcpp::NumericVector mu_param = Rcpp::NumericVector::create(mu_val);
            Rcpp::NumericVector sigma_param = Rcpp::NumericVector::create(sigma_val);

            // Add dimension attributes if needed
            mu_param.attr("dim") = Rcpp::IntegerVector::create(1, 1, 1);
            sigma_param.attr("dim") = Rcpp::IntegerVector::create(1, 1, 1);

            formatted_params["mu"] = mu_param;
            formatted_params["sigma"] = sigma_param;
          } else {
            // Use defaults
            formatted_params = getDefaultNormalParams();
          }
        } catch (...) {
          // If extraction fails, use defaults
          formatted_params = getDefaultNormalParams();
        }
      } else {
        // Use defaults
        formatted_params = getDefaultNormalParams();
      }

      state_params = formatted_params;
    }
  }

  return state_params;
}

// Helper to get default normal parameters
Rcpp::List MarkovDP::getDefaultNormalParams() {
  Rcpp::List default_params;

  Rcpp::NumericVector mu = Rcpp::NumericVector::create(0.0);
  Rcpp::NumericVector sigma = Rcpp::NumericVector::create(1.0);

  mu.attr("dim") = Rcpp::IntegerVector::create(1, 1, 1);
  sigma.attr("dim") = Rcpp::IntegerVector::create(1, 1, 1);

  default_params["mu"] = mu;
  default_params["sigma"] = sigma;

  return default_params;
}

// Relabel states to be contiguous (0, 1, 2, ...)
arma::uvec MarkovDP::relabelStates(const arma::uvec& dpStates) {
  arma::uvec uniqueStates = arma::unique(dpStates);
  int newUniqueStates = uniqueStates.n_elem;

  arma::uvec newStates(dpStates.n_elem);

  // Create mapping from old to new labels
  std::map<int, int> labelMap;
  for (int i = 0; i < newUniqueStates; i++) {
    labelMap[uniqueStates[i]] = i;
  }

  // Apply mapping
  for (size_t i = 0; i < dpStates.n_elem; i++) {
    newStates[i] = labelMap[dpStates[i]];
  }

  return newStates;
}

// Log posterior for alpha and beta
double MarkovDP::alphabetaLogPosterior(double alpha, double beta, const arma::vec& nii) {
  if (alpha <= 0 || beta <= 0) {
    return -std::numeric_limits<double>::infinity();
  }

  double logTerm1 = std::log(beta) + std::lgamma(alpha + beta) - std::lgamma(alpha);

  double logTerm2 = 0.0;
  for (size_t i = 0; i < nii.n_elem; i++) {
    logTerm2 += std::lgamma(nii[i] + alpha) - std::lgamma(nii[i] + 1 + alpha + beta);
  }

  // Prior: Gamma(1, 1) for both alpha and beta
  double logPrior = -alpha - beta;

  return logPrior + logTerm1 * nii.n_elem + logTerm2;
}

// Update alpha and beta parameters
void MarkovDP::updateAlphaBeta() {
  // Get unique states and count transitions
  arma::uvec uniqueStates = arma::unique(states);
  arma::vec nii(uniqueStates.n_elem);

  for (size_t i = 0; i < uniqueStates.n_elem; i++) {
    int count = 0;
    for (size_t j = 0; j < states.n_elem; j++) {
      if (states[j] == uniqueStates[i]) count++;
    }
    nii[i] = count - 1; // Don't count the first occurrence
  }

  // Simple grid search for starting values
  double bestAlpha = 1.0, bestBeta = 1.0;
  double bestLogPost = alphabetaLogPosterior(1.0, 1.0, nii);

  for (double a = 0.1; a <= 5.0; a += 0.5) {
    for (double b = 0.1; b <= 5.0; b += 0.5) {
      double logPost = alphabetaLogPosterior(a, b, nii);
      if (logPost > bestLogPost) {
        bestLogPost = logPost;
        bestAlpha = a;
        bestBeta = b;
      }
    }
  }

  // Metropolis-Hastings sampling
  double currentAlpha = bestAlpha;
  double currentBeta = bestBeta;
  double currentLogPost = bestLogPost;

  for (int i = 0; i < 100; i++) {
    // Propose new values
    double newAlpha = std::abs(currentAlpha + 0.1 * R::rnorm(0, 1));
    double newBeta = std::abs(currentBeta + 0.1 * R::rnorm(0, 1));

    double newLogPost = alphabetaLogPosterior(newAlpha, newBeta, nii);

    double acceptProb = std::min(1.0, std::exp(newLogPost - currentLogPost));

    if (R::runif(0, 1) < acceptProb) {
      currentAlpha = newAlpha;
      currentBeta = newBeta;
      currentLogPost = newLogPost;
    }
  }

  alpha = currentAlpha;
  beta = currentBeta;
}

// Update parameters for each unique state
void MarkovDP::paramUpdate() {
  // Get unique states
  arma::uvec uniqueStates = arma::unique(states);
  int numUniqueStates = uniqueStates.n_elem;

  // Initialize new unique parameters
  Rcpp::List newUniqueParams;

  // First, determine the structure from existing uniqueParams or create new
  if (uniqueParams.size() > 0) {
    // Initialize with same structure
    for (int j = 0; j < uniqueParams.size(); j++) {
      Rcpp::NumericVector paramArray = uniqueParams[j];
      if (paramArray.hasAttribute("dim")) {
        Rcpp::IntegerVector dims = paramArray.attr("dim");
        if (dims.size() == 3) {
          // Create new array with correct size for unique states
          Rcpp::NumericVector newArray(dims[0] * dims[1] * numUniqueStates);
          newArray.attr("dim") = Rcpp::IntegerVector::create(dims[0], dims[1], numUniqueStates);
          newUniqueParams.push_back(newArray);
        } else {
          newUniqueParams.push_back(Rcpp::NumericVector(numUniqueStates));
        }
      } else {
        newUniqueParams.push_back(Rcpp::NumericVector(numUniqueStates));
      }
    }
  } else {
    // Create from prior draw
    Rcpp::List priorSample = mixingDistribution->priorDraw(numUniqueStates);
    newUniqueParams = priorSample;
  }

  // Update parameters for each unique state
  for (int i = 0; i < numUniqueStates; i++) {
    // Find all data points for this state
    arma::uvec stateIndices = arma::find(states == uniqueStates[i]);

    if (stateIndices.n_elem > 0) {
      arma::mat stateData = data.rows(stateIndices);

      // Draw from posterior
      Rcpp::List postDraw = mixingDistribution->posteriorDraw(stateData, 1);

      // Update unique parameters
      for (int j = 0; j < postDraw.size(); j++) {
        Rcpp::NumericVector paramArray = newUniqueParams[j];
        Rcpp::NumericVector newParam = postDraw[j];

        if (paramArray.hasAttribute("dim") && newParam.hasAttribute("dim")) {
          Rcpp::IntegerVector dims = paramArray.attr("dim");
          if (dims.size() == 3) {
            // Copy the parameter values
            for (int k = 0; k < dims[0] * dims[1]; k++) {
              paramArray[k + i * dims[0] * dims[1]] = newParam[k];
            }
          } else {
            paramArray[i] = newParam[0];
          }
        } else {
          if (newParam.size() > 0) {
            paramArray[i] = newParam[0];
          }
        }
      }
    }
  }

  uniqueParams = newUniqueParams;

  // Update params to point to the correct unique parameters
  std::vector<Rcpp::List> newParams;
  newParams.reserve(states.n_elem);

  for (size_t i = 0; i < states.n_elem; i++) {
    Rcpp::List stateParams;

    // Find which unique state this corresponds to
    int uniqueIdx = 0;
    for (int j = 0; j < numUniqueStates; j++) {
      if (uniqueStates[j] == states[i]) {
        uniqueIdx = j;
        break;
      }
    }

    // Extract parameters for this unique state
    for (int j = 0; j < uniqueParams.size(); j++) {
      Rcpp::NumericVector paramArray = uniqueParams[j];

      if (paramArray.hasAttribute("dim")) {
        Rcpp::IntegerVector dims = paramArray.attr("dim");
        if (dims.size() == 3 && dims[2] > uniqueIdx) {
          // Extract slice for this state
          Rcpp::NumericVector stateParam(dims[0] * dims[1]);
          stateParam.attr("dim") = Rcpp::IntegerVector::create(dims[0], dims[1], 1);

          for (int k = 0; k < dims[0] * dims[1]; k++) {
            stateParam[k] = paramArray[k + uniqueIdx * dims[0] * dims[1]];
          }
          stateParams.push_back(stateParam);
        } else if (paramArray.size() > uniqueIdx) {
          stateParams.push_back(Rcpp::NumericVector::create(paramArray[uniqueIdx]));
        } else {
          // Default value if index out of bounds
          stateParams.push_back(Rcpp::NumericVector::create(0.0));
        }
      } else {
        if (paramArray.size() > uniqueIdx) {
          stateParams.push_back(Rcpp::NumericVector::create(paramArray[uniqueIdx]));
        } else {
          stateParams.push_back(Rcpp::NumericVector::create(0.0));
        }
      }
    }

    newParams.push_back(stateParams);
  }

  params = newParams;
}

// Fit method
void MarkovDP::fit(int iterations, bool updatePrior, bool progressBar) {
  if (progressBar) {
    Rcpp::Rcout << "Starting Markov DP (HMM) fitting..." << std::endl;
  }

  // Initialize chains
  alphaChain = Rcpp::NumericVector(iterations);
  betaChain = Rcpp::NumericVector(iterations);
  statesChain = Rcpp::List(iterations);
  paramChain = Rcpp::List(iterations);

  for (int i = 0; i < iterations; i++) {
    // Store current values
    alphaChain[i] = alpha;
    betaChain[i] = beta;

    // Convert states to 1-indexed for R
    Rcpp::IntegerVector r_states(states.n_elem);
    for (size_t j = 0; j < states.n_elem; j++) {
      r_states[j] = states[j] + 1;
    }
    statesChain[i] = r_states;
    paramChain[i] = uniqueParams;

    // Update components
    updateStates();
    updateAlphaBeta();
    paramUpdate();

    if (progressBar && ((i + 1) % (iterations / 10) == 0 || i == iterations - 1)) {
      Rcpp::Rcout << "Iteration " << i + 1 << "/" << iterations << std::endl;
    }
  }

  if (progressBar) {
    Rcpp::Rcout << "Markov DP fitting complete." << std::endl;
  }
}

// Convert to R
Rcpp::List MarkovDP::toR() const {
  Rcpp::List result = DirichletProcess::toR(); // Get base class data

  // Convert states to 1-indexed for R
  Rcpp::IntegerVector r_states(states.n_elem);
  for (size_t i = 0; i < states.n_elem; i++) {
    r_states[i] = states[i] + 1;
  }
  result["states"] = r_states;

  result["beta"] = beta;
  result["uniqueParams"] = uniqueParams;

  // Convert params vector to R list
  Rcpp::List rParams;
  for (const auto& p : params) {
    rParams.push_back(p);
  }
  result["params"] = rParams;

  // Add chains if they exist
  if (alphaChain.size() > 0) {
    result["alphaChain"] = alphaChain;
    result["betaChain"] = betaChain;
    result["statesChain"] = statesChain;
    result["paramChain"] = paramChain;
  }

  // Add mixing distribution
  if (mixingDistribution) {
    result["mixingDistribution"] = mixingDistribution->toR();
  }

  // Set class
  result.attr("class") = Rcpp::CharacterVector::create("list", "markov", "dirichletprocess",
              mixingDistribution->distribution,
              mixingDistribution->conjugate ? "conjugate" : "nonconjugate");

  return result;
}

// Create from R
MarkovDP* MarkovDP::fromR(const Rcpp::List& rObj) {
  return new MarkovDP(rObj);
}

} // namespace dp
