// src/markov_mcmc_runner.cpp
#include "markov_mcmc_runner.h"
#include "mixing_distribution_base.h"
#include <algorithm>
#include <numeric>

namespace dirichletprocess {

MarkovMCMCRunner::MarkovMCMCRunner(const arma::mat& data,
                                   const Rcpp::List& mixing_dist_params,
                                   const Rcpp::List& mcmc_params)
  : data(data) {

  // Extract MCMC parameters
  n_iter = Rcpp::as<int>(mcmc_params["n_iter"]);
  n_burn = Rcpp::as<int>(mcmc_params["n_burn"]);
  thin = Rcpp::as<int>(mcmc_params["thin"]);
  update_prior = Rcpp::as<bool>(mcmc_params["update_prior"]);

  // Extract hyperparameters
  alpha = Rcpp::as<double>(mcmc_params["alpha"]);
  beta = Rcpp::as<double>(mcmc_params["beta"]);

  // Set m_auxiliary (default to 3 for Algorithm 8)
  if (mcmc_params.containsElementNamed("m_auxiliary")) {
    m_auxiliary = Rcpp::as<int>(mcmc_params["m_auxiliary"]);
  } else {
    m_auxiliary = 3;
  }

  // Extract prior parameters
  if (mcmc_params.containsElementNamed("alpha_prior_shape")) {
    alpha_prior_shape = Rcpp::as<double>(mcmc_params["alpha_prior_shape"]);
  } else {
    alpha_prior_shape = 1.0;
  }

  if (mcmc_params.containsElementNamed("alpha_prior_rate")) {
    alpha_prior_rate = Rcpp::as<double>(mcmc_params["alpha_prior_rate"]);
  } else {
    alpha_prior_rate = 1.0;
  }

  if (mcmc_params.containsElementNamed("beta_prior_shape")) {
    beta_prior_shape = Rcpp::as<double>(mcmc_params["beta_prior_shape"]);
  } else {
    beta_prior_shape = 1.0;
  }

  if (mcmc_params.containsElementNamed("beta_prior_rate")) {
    beta_prior_rate = Rcpp::as<double>(mcmc_params["beta_prior_rate"]);
  } else {
    beta_prior_rate = 1.0;
  }

  // Create mixing distribution
  std::string dist_type = Rcpp::as<std::string>(mixing_dist_params["type"]);
  mixing_dist = MixingDistribution::create(dist_type, mixing_dist_params);

  // Initialize state
  state.reset(new MarkovDPState(data.n_rows, alpha, beta));

  // Initialize with prior draw
  arma::vec initial_params = mixing_dist->prior_draw();
  state->unique_params.push_back(initial_params);
  state->state_params.resize(data.n_rows);
  for (arma::uword i = 0; i < data.n_rows; i++) {  // Changed to arma::uword
    state->state_params[i] = initial_params;
  }

  // Pre-allocate storage
  alpha_samples.reserve(n_iter);
  beta_samples.reserve(n_iter);
  states_samples.reserve(n_iter);
  params_samples.reserve(n_iter);
  unique_params_samples.reserve(n_iter);
}

Rcpp::List MarkovMCMCRunner::run() {
  Rcpp::Rcout << "Starting Markov MCMC with Algorithm 8" << std::endl;

  // Main MCMC loop
  for (int iter = 0; iter < n_iter; iter++) {
    Rcpp::checkUserInterrupt();

    // Step 1: Update states using Algorithm 8 with Markov dynamics
    update_states_algorithm8();

    // Step 2: Update state parameters
    update_state_parameters();

    // Step 3: Update concentration parameters
    if (update_prior) {
      update_alpha_beta();
    }

    // Store samples after burn-in
    if (iter >= n_burn && (iter - n_burn) % thin == 0) {
      store_iteration(iter);
    }

    // Progress reporting
    if ((iter + 1) % 100 == 0) {
      Rcpp::Rcout << "Iteration " << (iter + 1) << "/" << n_iter
                  << " (unique states: " << state->n_states << ")" << std::endl;
    }
  }

  // Compile results
  Rcpp::List results;
  results["alpha_chain"] = Rcpp::wrap(alpha_samples);
  results["beta_chain"] = Rcpp::wrap(beta_samples);
  results["states_chain"] = states_samples;
  results["params_chain"] = params_samples;
  results["unique_params_chain"] = unique_params_samples;
  Rcpp::IntegerVector final_states_vec(state->states.n_elem);
  for (size_t i = 0; i < state->states.n_elem; i++) {
    final_states_vec[i] = state->states[i] + 1;  // Convert to 1-indexed
  }
  results["final_states"] = final_states_vec; // Convert to 1-indexed
  results["final_params"] = state->state_params;
  results["final_unique_params"] = state->unique_params;
  results["n_states"] = state->n_states;

  return results;
}

void MarkovMCMCRunner::update_states_algorithm8() {
  int n = data.n_rows;

  // Update each state sequentially
  for (int i = 0; i < n; i++) {
    arma::vec obs = data.row(i).t();

    // Get current unique states and their parameters
    state->update_unique_states();

    // Compute weights for existing states based on Markov dynamics
    std::vector<double> weights;
    std::vector<arma::vec> candidate_params;
    std::vector<unsigned int> candidate_states;  // Changed to unsigned int

    // Add weights for existing states
    for (int s = 0; s < state->n_states; s++) {
      double weight = 1.0;

      // Compute transition probabilities
      if (i == 0) {
        // First observation, use stationary distribution
        if (static_cast<unsigned int>(s) == state->states[0]) {  // Cast to unsigned int
          weight = state->alpha / (state->beta + state->alpha);
        } else {
          weight = state->beta / (state->n_states * (state->beta + state->alpha));
        }
      } else if (i == n - 1) {
        // Last observation, simpler calculation
        if (static_cast<unsigned int>(s) == state->states[n-2]) {  // Cast to unsigned int
          weight = state->alpha / (state->beta + state->alpha);
        } else {
          weight = state->beta / (state->n_states * (state->beta + state->alpha));
        }
      } else {
        // Middle observations
        weight = compute_transition_probability(state->states[i-1], s, state->states, i);
      }

      weights.push_back(weight);
      candidate_params.push_back(state->unique_params[s]);
      candidate_states.push_back(state->unique_states[s]);
    }

    // Algorithm 8: Add auxiliary parameters
    std::vector<arma::vec> aux_params = draw_auxiliary_parameters(m_auxiliary);
    for (int m = 0; m < m_auxiliary; m++) {
      arma::vec aux_param = aux_params[m];
      double weight = 0.0;

      if (i == 0 || i == n - 1) {
        // Edge cases
        weight = state->beta / (m_auxiliary * (state->beta + state->alpha));
      } else {
        // Middle states
        weight = state->beta / (m_auxiliary * (state->beta + state->alpha));
      }

      weights.push_back(weight);
      candidate_params.push_back(aux_param);
      candidate_states.push_back(state->n_states + m); // New state labels
    }

    // Compute likelihoods
    std::vector<double> probs(weights.size());
    for (size_t k = 0; k < weights.size(); k++) {
      double lik = mixing_dist->log_likelihood(obs, candidate_params[k]);
      probs[k] = weights[k] * std::exp(lik);
    }

    // Sample new state
    int chosen = sample_categorical(probs);
    state->states[i] = candidate_states[chosen];
    state->state_params[i] = candidate_params[chosen];

    // If new state was created, add to unique parameters
    if (candidate_states[chosen] >= static_cast<unsigned int>(state->n_states)) {  // Cast
      state->unique_params.push_back(candidate_params[chosen]);
    }
  }

  // Relabel states to be contiguous
  state->states = relabel_states(state->states);
  state->update_unique_states();
}

void MarkovMCMCRunner::update_state_parameters() {
  state->update_unique_states();

  // Clear and resize unique_params
  state->unique_params.clear();
  state->unique_params.resize(state->n_states);

  // Update parameters for each unique state
  for (int s = 0; s < state->n_states; s++) {
    // Find all observations in this state
    arma::uvec state_indices = arma::find(state->states == state->unique_states[s]);

    if (state_indices.n_elem > 0) {
      arma::mat state_data = data.rows(state_indices);

      // Draw from posterior
      arma::vec prior_params = mixing_dist->prior_draw();
      state->unique_params[s] = mixing_dist->posterior_draw(state_data, prior_params);

      // Update state_params for all observations in this state
      for (size_t i = 0; i < state_indices.n_elem; i++) {
        state->state_params[state_indices[i]] = state->unique_params[s];
      }
    } else {
      // No data for this state, draw from prior
      state->unique_params[s] = mixing_dist->prior_draw();
    }
  }
}

void MarkovMCMCRunner::update_alpha_beta() {
  // Compute transition counts
  arma::vec transition_counts = compute_transition_counts(state->states);

  // Use Metropolis-Hastings for alpha and beta
  double current_log_post = log_posterior_alpha_beta(state->alpha, state->beta);

  // Propose new alpha
  double prop_alpha = state->alpha * std::exp(R::rnorm(0, 0.2));
  double new_log_post = log_posterior_alpha_beta(prop_alpha, state->beta);

  double log_ratio = new_log_post - current_log_post +
    std::log(prop_alpha / state->alpha);

  if (std::log(R::runif(0, 1)) < log_ratio) {
    state->alpha = prop_alpha;
    current_log_post = new_log_post;
  }

  // Propose new beta
  double prop_beta = state->beta * std::exp(R::rnorm(0, 0.2));
  new_log_post = log_posterior_alpha_beta(state->alpha, prop_beta);

  log_ratio = new_log_post - current_log_post +
    std::log(prop_beta / state->beta);

  if (std::log(R::runif(0, 1)) < log_ratio) {
    state->beta = prop_beta;
  }
}

double MarkovMCMCRunner::compute_transition_probability(int from_state, int to_state,
                                                        const arma::uvec& states, int pos) {
  // Exclude position pos from counts
  int n_from = 0;
  int n_from_to = 0;

  for (arma::uword i = 0; i < states.n_elem - 1; i++) {  // Changed to arma::uword
    if (static_cast<int>(i) != pos && static_cast<int>(states[i]) == from_state) {  // Cast both
      n_from++;
      if (static_cast<int>(i + 1) != pos && static_cast<int>(states[i + 1]) == to_state) {  // Cast both
        n_from_to++;
      }
    }
  }

  if (from_state == to_state) {
    return (n_from_to + state->alpha) / (n_from + state->alpha + state->beta);
  } else {
    return (n_from_to + state->beta / state->n_states) /
      (n_from + state->alpha + state->beta);
  }
}

arma::vec MarkovMCMCRunner::compute_transition_counts(const arma::uvec& states) {
  // Count self-transitions and different transitions
  int n_same = 0;
  int n_diff = 0;

  for (size_t i = 0; i < states.n_elem - 1; i++) {
    if (states[i] == states[i + 1]) {
      n_same++;
    } else {
      n_diff++;
    }
  }

  return arma::vec({static_cast<double>(n_same), static_cast<double>(n_diff)});
}

double MarkovMCMCRunner::log_posterior_alpha_beta(double alpha, double beta) {
  arma::vec counts = compute_transition_counts(state->states);
  double n_same = counts[0];
  double n_diff = counts[1];

  // Log posterior (up to proportionality constant)
  double log_post = 0.0;

  // Prior contributions
  log_post += (alpha_prior_shape - 1) * std::log(alpha) - alpha_prior_rate * alpha;
  log_post += (beta_prior_shape - 1) * std::log(beta) - beta_prior_rate * beta;

  // Likelihood contributions (simplified)
  log_post += n_same * std::log(alpha / (alpha + beta));
  log_post += n_diff * std::log(beta / (alpha + beta));

  return log_post;
}

arma::uvec MarkovMCMCRunner::relabel_states(const arma::uvec& states) {
  arma::uvec new_states = states;
  std::map<int, int> label_map;
  int next_label = 0;

  for (size_t i = 0; i < states.n_elem; i++) {
    int current = states[i];
    if (label_map.find(current) == label_map.end()) {
      label_map[current] = next_label++;
    }
    new_states[i] = label_map[current];
  }

  return new_states;
}

int MarkovMCMCRunner::sample_categorical(const std::vector<double>& probs) {
  double sum = std::accumulate(probs.begin(), probs.end(), 0.0);

  if (sum <= 0) {
    // If all probabilities are zero, sample uniformly
    // Fixed: Use R::runif instead of R::sample
    return static_cast<int>(R::runif(0, probs.size()));
  }

  double u = R::runif(0, sum);
  double cumsum = 0.0;

  for (size_t i = 0; i < probs.size(); i++) {
    cumsum += probs[i];
    if (u <= cumsum) {
      return i;
    }
  }

  return probs.size() - 1;
}

void MarkovMCMCRunner::store_iteration(int iter) {
  alpha_samples.push_back(state->alpha);
  beta_samples.push_back(state->beta);

  // Store states (convert to 1-indexed for R)
  std::vector<int> states_copy(state->states.n_elem);
  for (size_t i = 0; i < state->states.n_elem; i++) {
    states_copy[i] = state->states[i] + 1;
  }
  states_samples.push_back(states_copy);

  // Store parameters
  params_samples.push_back(state->state_params);
  unique_params_samples.push_back(state->unique_params);
}

std::vector<arma::vec> MarkovMCMCRunner::draw_auxiliary_parameters(int m) {
  std::vector<arma::vec> params;
  for (int i = 0; i < m; i++) {
    params.push_back(mixing_dist->prior_draw());
  }
  return params;
}

} // namespace dirichletprocess
