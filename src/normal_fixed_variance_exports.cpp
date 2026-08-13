#include <RcppArmadillo.h>
#include "mcmc_runner.h"
#include "normal_fixed_variance_mixing.h"

using namespace dirichletprocess;

namespace {

class NormalFixedVarianceFastFitRunner : public dirichletprocess::MCMCRunner {
public:
  NormalFixedVarianceFastFitRunner(const arma::mat& data,
                                   const Rcpp::List& mixing_dist_params,
                                   const Rcpp::List& mcmc_params)
    : MCMCRunner(data, mixing_dist_params, mcmc_params) {}

  double compute_normal_fixed_variance_repaired_r_loglikelihood() const {
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

  Rcpp::List run_normal_fixed_variance_fit() {
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
      likelihood_chain[iter] = compute_normal_fixed_variance_repaired_r_loglikelihood();

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
Rcpp::NumericVector cpp_normal_fixed_variance_prior_draw(double mu0, double sigma0,
                                                         double sigma, int n) {
  NormalFixedVarianceMixing nfv(mu0, sigma0, sigma);
  Rcpp::NumericVector result(n);

  for (int i = 0; i < n; ++i) {
    arma::vec params = nfv.prior_draw();
    result[i] = params[0];
  }

  return result;
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_normal_fixed_variance_posterior_draw(arma::mat data, double mu0,
                                                             double sigma0, double sigma, int n) {
  NormalFixedVarianceMixing nfv(mu0, sigma0, sigma);
  Rcpp::NumericVector result(n);

  for (int i = 0; i < n; ++i) {
    arma::vec params = nfv.posterior_draw(data, arma::vec());
    result[i] = params[0];
  }

  return result;
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_normal_fixed_variance_likelihood(arma::vec x, double mu, double sigma) {
  NormalFixedVarianceMixing nfv(0.0, 1.0, sigma);
  arma::vec params(1);
  params[0] = mu;

  Rcpp::NumericVector result(x.n_elem);
  for (arma::uword i = 0; i < x.n_elem; ++i) {
    result[i] = std::exp(nfv.log_likelihood(x.row(i).t(), params));
  }

  return result;
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_normal_fixed_variance_posterior_parameters(arma::mat data,
                                                                   double mu0, double sigma0,
                                                                   double sigma) {
  NormalFixedVarianceMixing nfv(mu0, sigma0, sigma);
  arma::vec params = nfv.posterior_parameters(data);

  return Rcpp::NumericVector::create(params[0], params[1]);
}

// [[Rcpp::export]]
Rcpp::List run_normal_fixed_variance_fit_cpp(arma::mat data,
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
        Rcpp::as<std::string>(mixing_dist_params["type"]) != "normalFixedVariance") {
      Rcpp::stop("run_normal_fixed_variance_fit_cpp only supports normalFixedVariance mixing distributions");
    }

    NormalFixedVarianceFastFitRunner runner(data, mixing_dist_params, mcmc_params);
    return runner.run_normal_fixed_variance_fit();

  } catch (const std::exception& e) {
    Rcpp::stop("C++ normalFixedVariance Fit error: " + std::string(e.what()));
  } catch (...) {
    Rcpp::stop("Unknown error in C++ normalFixedVariance Fit");
  }
}
