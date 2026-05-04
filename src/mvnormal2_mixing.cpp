#include "mvnormal2_mixing.h"
#include <RcppArmadillo.h>
#include <algorithm>
#include <cmath>
#include <limits>

namespace dirichletprocess {

namespace {

arma::mat safe_sympd_inverse(const arma::mat& matrix, double jitter = 1e-8) {
  arma::mat sym = 0.5 * (matrix + matrix.t());
  arma::mat inv;
  if (arma::inv_sympd(inv, sym)) {
    return inv;
  }

  arma::mat regularized = sym + arma::eye(sym.n_rows, sym.n_cols) * jitter;
  if (arma::inv_sympd(inv, regularized)) {
    return inv;
  }

  return arma::pinv(regularized);
}

arma::mat safe_iwishrnd(const arma::mat& scale, double df) {
  arma::mat sym = 0.5 * (scale + scale.t());
  try {
    arma::mat draw = arma::iwishrnd(sym, df);
    return 0.5 * (draw + draw.t());
  } catch (...) {
    arma::mat regularized = sym + arma::eye(sym.n_rows, sym.n_cols) * 1e-8;
    arma::mat draw = arma::iwishrnd(regularized, df);
    return 0.5 * (draw + draw.t());
  }
}

} // namespace

MVNormal2Mixing::MVNormal2Mixing(const arma::mat& mu0, const arma::mat& sigma0,
                                 const arma::mat& phi0, double nu0, int mh_draws)
  : mu0(mu0), sigma0(sigma0), phi0(phi0), nu0(nu0), mh_draws(mh_draws) {
  
  // Extract dimension from mu0
  if (mu0.n_rows == 1) {
    d = mu0.n_cols;
  } else if (mu0.n_cols == 1) {
    d = mu0.n_rows;
  } else {
    Rcpp::stop("mu0 must be a row or column vector");
  }

  // Validate inputs
  if (nu0 <= d - 1) {
    Rcpp::stop("nu0 must be greater than dimension - 1");
  }
  if (phi0.n_rows != static_cast<arma::uword>(d) || phi0.n_cols != static_cast<arma::uword>(d)) {
    Rcpp::stop("phi0 must be a d x d matrix");
  }
  if (sigma0.n_rows != static_cast<arma::uword>(d) || sigma0.n_cols != static_cast<arma::uword>(d)) {
    Rcpp::stop("sigma0 must be a d x d matrix");
  }

  // Ensure phi0 and sigma0 are symmetric
  this->phi0 = ensureSymmetric(phi0);
  this->sigma0 = ensureSymmetric(sigma0);
}

double MVNormal2Mixing::log_likelihood(const arma::vec& data_point,
                                       const arma::vec& params) const {
  // Extract mean and covariance matrix from flattened params
  arma::vec mu(d);
  arma::mat Sigma(d, d);
  unflatten_params(params, mu, Sigma);
  Sigma = ensureSymmetric(Sigma);

  // Compute log-likelihood for multivariate normal
  arma::vec x_centered = data_point - mu;

  double log_det_val;
  double sign;
  arma::log_det(log_det_val, sign, Sigma);

  if (sign <= 0) {
    return -std::numeric_limits<double>::infinity();
  }

  // Compute quadratic form
  arma::mat Sigma_inv;
  try {
    Sigma_inv = arma::inv_sympd(Sigma);
  } catch (...) {
    return -std::numeric_limits<double>::infinity();
  }

  double quad_form = arma::as_scalar(x_centered.t() * Sigma_inv * x_centered);
  double log_lik = -0.5 * d * std::log(2.0 * M_PI) - 0.5 * log_det_val - 0.5 * quad_form;

  return log_lik;
}

arma::vec MVNormal2Mixing::posterior_draw(const arma::mat& cluster_data,
                                          const arma::vec& prior_params) const {
  int n = cluster_data.n_rows;
  if (n == 0) {
    return prior_draw();
  }

  arma::vec mu0_vec;
  if (mu0.n_rows == 1) {
    mu0_vec = mu0.row(0).t();
  } else {
    mu0_vec = mu0.col(0);
  }
  arma::vec mu_samp(d, arma::fill::zeros);

  if (prior_params.n_elem == static_cast<arma::uword>(param_dim())) {
    arma::vec warm_mu;
    arma::mat warm_sigma;
    unflatten_params(prior_params, warm_mu, warm_sigma);
    if (warm_mu.is_finite()) {
      mu_samp = warm_mu;
    }
  }

  arma::mat sigma_draw = ensureSymmetric(phi0);
  arma::mat sigma0_inv = safe_sympd_inverse(sigma0);
  arma::vec x_bar = arma::mean(cluster_data, 0).t();
  int draws = std::max(1, mh_draws);

  for (int iter = 0; iter < draws; ++iter) {
    double nu_n = n + nu0;
    arma::mat phi_n = phi0;
    for (int j = 0; j < n; ++j) {
      arma::vec diff = cluster_data.row(j).t() - mu_samp;
      phi_n += diff * diff.t();
    }
    phi_n = ensureSymmetric(phi_n);

    sigma_draw = safe_iwishrnd(phi_n, nu_n);
    arma::mat sigma_draw_inv = safe_sympd_inverse(sigma_draw);
    arma::mat sigma_n = safe_sympd_inverse(sigma0_inv + n * sigma_draw_inv);
    arma::vec mu_n = sigma_n * (n * sigma_draw_inv * x_bar + sigma0_inv * mu0_vec);
    mu_samp = arma::mvnrnd(mu_n, ensureSymmetric(sigma_n));
  }

  return flatten_params(mu_samp, sigma_draw);
}

arma::vec MVNormal2Mixing::prior_draw() const {
  arma::vec mu0_vec;
  if (mu0.n_rows == 1) {
    mu0_vec = mu0.row(0).t();
  } else {
    mu0_vec = mu0.col(0);
  }

  arma::mat Sigma_draw = safe_iwishrnd(phi0, nu0);
  arma::vec mu_draw = arma::mvnrnd(mu0_vec, ensureSymmetric(sigma0));

  return flatten_params(mu_draw, Sigma_draw);
}

int MVNormal2Mixing::param_dim() const {
  // d parameters for mu + d*(d+1)/2 parameters for symmetric Sigma
  return d + d * (d + 1) / 2;
}

arma::vec MVNormal2Mixing::flatten_params(const arma::vec& mu, const arma::mat& Sigma) const {
  arma::vec params(param_dim());
  
  // First d elements are mu
  params.subvec(0, d - 1) = mu;
  
  // Remaining elements are upper triangle of Sigma (including diagonal)
  int idx = d;
  for (int i = 0; i < d; ++i) {
    for (int j = i; j < d; ++j) {
      params(idx++) = Sigma(i, j);
    }
  }
  
  return params;
}

void MVNormal2Mixing::unflatten_params(const arma::vec& params, arma::vec& mu, arma::mat& Sigma) const {
  // Extract mu
  mu = params.subvec(0, d - 1);
  
  // Extract Sigma from upper triangle
  Sigma = arma::zeros(d, d);
  int idx = d;
  for (int i = 0; i < d; ++i) {
    for (int j = i; j < d; ++j) {
      Sigma(i, j) = params(idx);
      if (i != j) {
        Sigma(j, i) = params(idx);  // Make symmetric
      }
      idx++;
    }
  }
}

} // namespace dirichletprocess
