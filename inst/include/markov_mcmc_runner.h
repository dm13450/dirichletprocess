// inst/include/markov_mcmc_runner.h
#ifndef MARKOV_MCMC_RUNNER_H
#define MARKOV_MCMC_RUNNER_H

#include <RcppArmadillo.h>
#include <memory>
#include <vector>
#include <map>
#include "mixing_distribution_base.h"

namespace dirichletprocess {

// Forward declarations
class MarkovDPState;

// Markov MCMC Runner class implementing Algorithm 8 with Markov dynamics
class MarkovMCMCRunner {
private:
  // Data
  arma::mat data;

  // Model components
  std::unique_ptr<MixingDistribution> mixing_dist;

  // MCMC state
  std::unique_ptr<MarkovDPState> state;

  // Parameters
  int n_iter;
  int n_burn;
  int thin;
  bool update_prior;
  int m_auxiliary; // Number of auxiliary parameters for Algorithm 8

  // Hyperparameters
  double alpha;  // DP concentration parameter
  double beta;   // Markov transition concentration parameter

  // Alpha and beta prior parameters
  double alpha_prior_shape;
  double alpha_prior_rate;
  double beta_prior_shape;
  double beta_prior_rate;

  // Storage for results
  std::vector<double> alpha_samples;
  std::vector<double> beta_samples;
  std::vector<std::vector<int>> states_samples;
  std::vector<std::vector<arma::vec>> params_samples;
  std::vector<std::vector<arma::vec>> unique_params_samples;

public:
  MarkovMCMCRunner(const arma::mat& data,
                   const Rcpp::List& mixing_dist_params,
                   const Rcpp::List& mcmc_params);

  // Main MCMC loop
  Rcpp::List run();

private:
  // MCMC steps
  void update_states_algorithm8();
  void update_state_parameters();
  void update_alpha_beta();
  void store_iteration(int iter);

  // Helper functions
  arma::uvec relabel_states(const arma::uvec& states);
  double compute_transition_probability(int from_state, int to_state,
                                        const arma::uvec& states, int pos);
  double log_posterior_alpha_beta(double alpha, double beta);
  arma::vec compute_transition_counts(const arma::uvec& states);

  // Algorithm 8 auxiliary parameter helpers
  std::vector<arma::vec> draw_auxiliary_parameters(int m);
  int sample_categorical(const std::vector<double>& probs);
};

// State container for Markov DP
class MarkovDPState {
public:
  arma::uvec states;                      // State sequence
  std::vector<arma::vec> state_params;    // Parameters for each state
  std::vector<arma::vec> unique_params;   // Unique state parameters
  arma::uvec unique_states;              // Unique state labels
  double alpha;                          // DP concentration
  double beta;                           // Transition concentration
  int n_states;                          // Number of unique states

  MarkovDPState(int n_obs, double initial_alpha, double initial_beta)
    : states(n_obs), alpha(initial_alpha), beta(initial_beta), n_states(0) {
    // Initialize with single state
    states.fill(0);
    unique_states = arma::uvec({0});
    n_states = 1;
  }

  void update_unique_states() {
    unique_states = arma::unique(states);
    n_states = unique_states.n_elem;
  }

  // Get state index in unique_states
  int get_unique_index(int state) const {
    for (size_t i = 0; i < unique_states.n_elem; i++) {
      if (unique_states[i] == static_cast<unsigned int>(state)) {
        return i;
      }
    }
    return -1;
  }
};

} // namespace dirichletprocess

#endif // MARKOV_MCMC_RUNNER_H
