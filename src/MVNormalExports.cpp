#include <RcppArmadillo.h>
#include "mcmc_runner.h"

namespace {

class MVNormalFastFitRunner : public dirichletprocess::MCMCRunner {
public:
  MVNormalFastFitRunner(const arma::mat& data,
                        const Rcpp::List& mixing_dist_params,
                        const Rcpp::List& mcmc_params)
    : MCMCRunner(data, mixing_dist_params, mcmc_params) {}

  Rcpp::List run_mvnormal_fit() {
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
      likelihood_chain[iter] = compute_repaired_r_loglikelihood();

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

// Additional export for testing MVNormal likelihood
// [[Rcpp::export]]
arma::vec mvnormal_log_likelihood_cpp(arma::mat x, arma::vec mu, arma::mat Sigma) {
  int n = x.n_rows;
  int d = x.n_cols;
  arma::vec log_lik(n);

  // Ensure Sigma is symmetric
  Sigma = 0.5 * (Sigma + Sigma.t());

  double log_det_val;
  double sign;
  arma::log_det(log_det_val, sign, Sigma);

  if (sign <= 0) {
    log_lik.fill(-std::numeric_limits<double>::infinity());
    return log_lik;
  }

  arma::mat Sigma_inv;
  try {
    Sigma_inv = arma::inv_sympd(Sigma);
  } catch (...) {
    log_lik.fill(-std::numeric_limits<double>::infinity());
    return log_lik;
  }

  double log_const = -0.5 * d * std::log(2.0 * M_PI) - 0.5 * log_det_val;

  for (int i = 0; i < n; ++i) {
    arma::vec x_centered = x.row(i).t() - mu;
    double quad_form = arma::as_scalar(x_centered.t() * Sigma_inv * x_centered);
    log_lik(i) = log_const - 0.5 * quad_form;
  }

  return log_lik;
}

// [[Rcpp::export]]
Rcpp::List run_mvnormal_fit_cpp(arma::mat data,
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
        Rcpp::as<std::string>(mixing_dist_params["type"]) != "mvnormal") {
      Rcpp::stop("run_mvnormal_fit_cpp only supports mvnormal mixing distributions");
    }

    if (mixing_dist_params.containsElementNamed("covModel")) {
      std::string cov_model = Rcpp::as<std::string>(mixing_dist_params["covModel"]);
      if (cov_model != "FULL") {
        Rcpp::stop("run_mvnormal_fit_cpp only supports mvnormal covModel = 'FULL'");
      }
    }

    MVNormalFastFitRunner runner(data, mixing_dist_params, mcmc_params);
    return runner.run_mvnormal_fit();

  } catch (const std::exception& e) {
    Rcpp::stop("C++ mvnormal Fit error: " + std::string(e.what()));
  } catch (...) {
    Rcpp::stop("Unknown error in C++ mvnormal Fit");
  }
}
