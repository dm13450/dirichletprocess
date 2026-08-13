// src/GaussianExports.cpp
#include <RcppArmadillo.h>
#include "mcmc_runner.h"
#include "mixing_distribution_base.h"

namespace {

class GaussianFastFitRunner : public dirichletprocess::MCMCRunner {
public:
  GaussianFastFitRunner(const arma::mat& data,
                        const Rcpp::List& mixing_dist_params,
                        const Rcpp::List& mcmc_params)
    : MCMCRunner(data, mixing_dist_params, mcmc_params) {}

  double compute_gaussian_repaired_r_loglikelihood() const {
    const double neg_inf = -std::numeric_limits<double>::infinity();

    if (!state || data.n_rows == 0 || state->n_clusters <= 0) {
      return neg_inf;
    }

    const arma::uword n_obs = data.n_rows;
    const int n_clusters = state->n_clusters;
    std::vector<double> flat_likelihoods(static_cast<size_t>(n_obs) *
                                         static_cast<size_t>(n_clusters));
    size_t idx = 0;

    for (arma::uword i = 0; i < n_obs; ++i) {
      arma::vec obs = data.row(i).t();
      for (int k = 0; k < n_clusters; ++k) {
        double log_lik = mixing_dist->log_likelihood(obs, state->cluster_params[k]);
        if (std::isnan(log_lik)) {
          return NA_REAL;
        }
        if (std::isinf(log_lik) && log_lik > 0) {
          flat_likelihoods[idx++] = R_PosInf;
        } else if (!std::isfinite(log_lik)) {
          flat_likelihoods[idx++] = 0.0;
        } else {
          flat_likelihoods[idx++] = std::exp(log_lik);
        }
      }
    }

    std::vector<double> weights(n_clusters, 0.0);
    for (int k = 0; k < n_clusters; ++k) {
      weights[k] = static_cast<double>(state->cluster_sizes[k]) /
        static_cast<double>(n_obs);
    }

    double total_loglik = 0.0;
    for (arma::uword row = 0; row < n_obs; ++row) {
      double weighted_sum = 0.0;
      bool any_pos_inf = false;

      for (int col = 0; col < n_clusters; ++col) {
        double density = flat_likelihoods[static_cast<size_t>(row) +
                                          static_cast<size_t>(col) * n_obs];
        double contribution = density * weights[col];
        if (std::isnan(contribution)) {
          return NA_REAL;
        }
        if (std::isinf(contribution) && contribution > 0) {
          any_pos_inf = true;
          continue;
        }
        weighted_sum += contribution;
      }

      if (any_pos_inf) {
        return R_PosInf;
      }
      if (weighted_sum <= 0.0) {
        return neg_inf;
      }
      total_loglik += std::log(weighted_sum);
    }

    return total_loglik;
  }

  Rcpp::List run_gaussian_fit() {
    if (data.n_rows == 0 || data.n_cols == 0) {
      Rcpp::stop("Data matrix has invalid dimensions");
    }

    initialize_state();

    arma::vec alpha_chain(n_iter);
    arma::vec likelihood_chain(n_iter);
    Rcpp::IntegerMatrix labels_chain(n_iter, data.n_rows);
    Rcpp::List theta_chain(n_iter);

    for (int iter = 0; iter < n_iter; ++iter) {
      alpha_chain[iter] = state->alpha;
      likelihood_chain[iter] = compute_gaussian_repaired_r_loglikelihood();

      for (arma::uword j = 0; j < data.n_rows; ++j) {
        labels_chain(iter, j) = state->cluster_labels[j] + 1;
      }

      Rcpp::List iter_params(state->cluster_params.size());
      for (size_t j = 0; j < state->cluster_params.size(); ++j) {
        iter_params[j] = state->cluster_params[j];
      }
      theta_chain[iter] = iter_params;

      single_iteration_update();
    }

    Rcpp::IntegerVector final_labels(state->cluster_labels.size());
    for (size_t i = 0; i < state->cluster_labels.size(); ++i) {
      final_labels[i] = state->cluster_labels[i] + 1;
    }

    Rcpp::List final_theta(state->cluster_params.size());
    for (size_t j = 0; j < state->cluster_params.size(); ++j) {
      final_theta[j] = state->cluster_params[j];
    }

    return Rcpp::List::create(
      Rcpp::Named("alpha_chain") = alpha_chain,
      Rcpp::Named("likelihood_chain") = likelihood_chain,
      Rcpp::Named("labels_chain") = labels_chain,
      Rcpp::Named("theta_chain") = theta_chain,
      Rcpp::Named("final_alpha") = state->alpha,
      Rcpp::Named("final_labels") = final_labels,
      Rcpp::Named("final_theta") = final_theta
    );
  }
};

} // namespace

// [[Rcpp::export]]
Rcpp::List run_mcmc_cpp(arma::mat data,
                        Rcpp::List mixing_dist_params,
                        Rcpp::List mcmc_params) {
  try {
    // Input validation
    if (data.n_rows == 0 || data.n_cols == 0) {
      Rcpp::stop("Data matrix cannot be empty");
    }

    if (data.has_nan()) {
      Rcpp::stop("Data contains NA values");
    }

    if (data.has_inf()) {
      Rcpp::stop("Data contains infinite values");
    }

    dirichletprocess::MCMCRunner runner(data, mixing_dist_params, mcmc_params);
    return runner.run();

  } catch (const std::exception& e) {
    Rcpp::stop("C++ MCMC error: " + std::string(e.what()));
  } catch (...) {
    Rcpp::stop("Unknown error in C++ MCMC");
  }
}

// [[Rcpp::export]]
Rcpp::List run_gaussian_fit_cpp(arma::mat data,
                                Rcpp::List mixing_dist_params,
                                Rcpp::List mcmc_params) {
  try {
    if (data.n_rows == 0 || data.n_cols == 0) {
      Rcpp::stop("Data matrix cannot be empty");
    }

    if (data.has_nan()) {
      Rcpp::stop("Data contains NA values");
    }

    if (data.has_inf()) {
      Rcpp::stop("Data contains infinite values");
    }

    if (!mixing_dist_params.containsElementNamed("type") ||
        Rcpp::as<std::string>(mixing_dist_params["type"]) != "gaussian") {
      Rcpp::stop("run_gaussian_fit_cpp only supports Gaussian mixing distributions");
    }

    GaussianFastFitRunner runner(data, mixing_dist_params, mcmc_params);
    return runner.run_gaussian_fit();

  } catch (const std::exception& e) {
    Rcpp::stop("C++ Gaussian Fit error: " + std::string(e.what()));
  } catch (...) {
    Rcpp::stop("Unknown error in C++ Gaussian Fit");
  }
}
