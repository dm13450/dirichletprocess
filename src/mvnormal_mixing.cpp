#include "mvnormal_mixing.h"
#include <RcppArmadillo.h>
#include <cmath>

namespace dirichletprocess {

MVNormalMixing::MVNormalMixing(const arma::vec& mu0, double kappa0,
                               const arma::mat& Lambda, double nu)
  : mu0(mu0), kappa0(kappa0), Lambda(Lambda), nu(nu), d(mu0.n_elem) {

  // Validate inputs
  if (kappa0 <= 0) {
    Rcpp::stop("kappa0 must be positive");
  }
  if (nu <= d - 1) {
    Rcpp::stop("nu must be greater than dimension - 1");
  }
  if (Lambda.n_rows != static_cast<arma::uword>(d) || Lambda.n_cols != static_cast<arma::uword>(d)) {
    Rcpp::stop("Lambda must be a d x d matrix");
  }

  // Ensure Lambda is symmetric
  this->Lambda = ensureSymmetric(Lambda);
}

double MVNormalMixing::log_likelihood(const arma::vec& data_point,
                                      const arma::vec& params) const {
  // Validate input dimensions
  if (data_point.n_elem != static_cast<arma::uword>(d) || d == 0) {
    return -std::numeric_limits<double>::infinity();
  }

  // Extract mean and precision matrix from flattened params
  arma::vec mu(d);
  arma::mat Sigma(d, d);
  unflatten_params(params, mu, Sigma);

  // Validate that unflatten_params worked correctly
  if (mu.n_elem != static_cast<arma::uword>(d) || Sigma.n_rows != static_cast<arma::uword>(d) || Sigma.n_cols != static_cast<arma::uword>(d)) {
    return -std::numeric_limits<double>::infinity();
  }

  // Compute log-likelihood for multivariate normal
  arma::vec x_centered = data_point - mu;

  double log_det_val;
  double sign;
  arma::log_det(log_det_val, sign, Sigma);

  if (sign <= 0 || !std::isfinite(log_det_val)) {
    return -std::numeric_limits<double>::infinity();
  }

  // Using precision parameterization with bounds checking
  if (x_centered.n_elem != static_cast<arma::uword>(d)) {
    return -std::numeric_limits<double>::infinity();
  }
  
  arma::mat quad_result = x_centered.t() * Sigma * x_centered;
  if (quad_result.n_elem != 1) {
    return -std::numeric_limits<double>::infinity();
  }
  
  double quad_form = quad_result(0, 0);
  double log_lik = -0.5 * d * std::log(2.0 * M_PI) + 0.5 * log_det_val - 0.5 * quad_form;

  return log_lik;
}

arma::vec MVNormalMixing::posterior_draw(const arma::mat& cluster_data,
                                         const arma::vec& prior_params) const {
  int n = cluster_data.n_rows;

  // Handle empty cluster case or invalid dimensions
  if (n == 0 || cluster_data.n_cols != static_cast<arma::uword>(d) || d == 0) {
    return prior_draw();
  }

  // Compute sample statistics with bounds checking
  arma::vec x_bar;
  try {
    arma::rowvec x_bar_row = arma::mean(cluster_data, 0);
    x_bar = x_bar_row.t();
    if (x_bar.n_elem != static_cast<arma::uword>(d)) {
      return prior_draw();
    }
  } catch (...) {
    return prior_draw();
  }

  // Update posterior parameters (Normal-Wishart conjugate update)
  double kappa_n = kappa0 + n;
  arma::vec mu_n = (kappa0 * mu0 + n * x_bar) / kappa_n;
  double nu_n = nu + n;

  // Compute scatter matrix with bounds checking
  arma::mat S = arma::zeros(d, d);
  for (int i = 0; i < n; ++i) {
    if (i >= 0 && i < static_cast<int>(cluster_data.n_rows)) {
      try {
        arma::vec xi = cluster_data.row(i).t();
        if (xi.n_elem == static_cast<arma::uword>(d) && x_bar.n_elem == static_cast<arma::uword>(d)) {
          arma::vec diff = xi - x_bar;
          S += diff * diff.t();
        }
      } catch (...) {
        // Skip this observation if there's an error
        continue;
      }
    }
  }

  // Update Lambda_n
  arma::mat Lambda_n = Lambda + S +
    (kappa0 * n / kappa_n) * (x_bar - mu0) * (x_bar - mu0).t();
  Lambda_n = ensureSymmetric(Lambda_n);

  // Draw from posterior Wishart(nu_n, Lambda_n^{-1})
  arma::mat Lambda_n_inv;
  try {
    // Ensure Lambda_n is properly symmetric before inversion
    Lambda_n = ensureSymmetric(Lambda_n);
    Lambda_n_inv = arma::inv_sympd(Lambda_n);
    if (!Lambda_n_inv.is_finite()) {
      throw std::runtime_error("Non-finite inverse matrix");
    }
  } catch (...) {
    // Try regular inversion first, then pseudoinverse as last resort
    try {
      Lambda_n_inv = arma::inv(Lambda_n);
      if (!Lambda_n_inv.is_finite()) {
        throw std::runtime_error("Non-finite inverse matrix");
      }
    } catch (...) {
      Lambda_n_inv = arma::pinv(Lambda_n);
    }
  }

  // Sample precision matrix from Wishart distribution
  // Additional validation before Wishart sampling to prevent segfaults
  if (!Lambda_n_inv.is_finite() || Lambda_n_inv.n_rows != static_cast<arma::uword>(d) || Lambda_n_inv.n_cols != static_cast<arma::uword>(d)) {
    return prior_draw(); // Fall back to prior if matrix is invalid
  }
  
  // Check condition number to avoid numerical issues
  double rcond = arma::rcond(Lambda_n_inv);
  if (rcond < 1e-12) {
    // Matrix is too ill-conditioned, use regularized version
    Lambda_n_inv += arma::eye<arma::mat>(d, d) * 1e-6;
  }
  
  arma::mat prec_draw = arma::wishrnd(Lambda_n_inv, nu_n);
  prec_draw = ensureSymmetric(prec_draw);

  // Sample mean from multivariate normal
  arma::mat prec_mu = kappa_n * prec_draw;
  prec_mu = ensureSymmetric(prec_mu);
  arma::mat cov_mu;
  try {
    cov_mu = arma::inv_sympd(prec_mu);
    if (!cov_mu.is_finite()) {
      throw std::runtime_error("Non-finite covariance matrix");
    }
  } catch (...) {
    try {
      cov_mu = arma::inv(prec_mu);
      if (!cov_mu.is_finite()) {
        throw std::runtime_error("Non-finite covariance matrix");
      }
    } catch (...) {
      cov_mu = arma::pinv(prec_mu);
    }
  }

  arma::vec mu_draw = arma::mvnrnd(mu_n, cov_mu);

  // Return flattened parameters
  return flatten_params(mu_draw, prec_draw);
}

arma::vec MVNormalMixing::prior_draw() const {
  // Validate dimensions
  if (d <= 0 || mu0.n_elem != static_cast<arma::uword>(d) || Lambda.n_rows != static_cast<arma::uword>(d) || Lambda.n_cols != static_cast<arma::uword>(d)) {
    Rcpp::stop("Invalid dimensions in prior_draw: d=" + std::to_string(d) +
               ", mu0.size=" + std::to_string(mu0.n_elem) +
               ", Lambda.size=" + std::to_string(Lambda.n_rows) + "x" + std::to_string(Lambda.n_cols));
  }

  // Draw precision matrix from Wishart(nu, Lambda^{-1})
  arma::mat Lambda_inv;
  try {
    arma::mat Lambda_sym = ensureSymmetric(Lambda);
    Lambda_inv = arma::inv_sympd(Lambda_sym);
    if (!Lambda_inv.is_finite()) {
      throw std::runtime_error("Non-finite inverse matrix");
    }
  } catch (...) {
    try {
      Lambda_inv = arma::inv(Lambda);
      if (!Lambda_inv.is_finite()) {
        throw std::runtime_error("Non-finite inverse matrix");
      }
    } catch (...) {
      Lambda_inv = arma::pinv(Lambda);
    }
  }

  // Validate Lambda_inv dimensions
  if (Lambda_inv.n_rows != static_cast<arma::uword>(d) || Lambda_inv.n_cols != static_cast<arma::uword>(d)) {
    Rcpp::stop("Lambda_inv has wrong dimensions: " +
               std::to_string(Lambda_inv.n_rows) + "x" + std::to_string(Lambda_inv.n_cols));
  }

  arma::mat prec_draw;
  try {
    // Additional validation before Wishart sampling to prevent segfaults
    if (!Lambda_inv.is_finite() || Lambda_inv.n_rows != static_cast<arma::uword>(d) || Lambda_inv.n_cols != static_cast<arma::uword>(d)) {
      Rcpp::stop("Invalid Lambda_inv matrix for Wishart sampling");
    }
    
    // Check condition number to avoid numerical issues
    double rcond = arma::rcond(Lambda_inv);
    if (rcond < 1e-12) {
      // Matrix is too ill-conditioned, use regularized version
      Lambda_inv += arma::eye<arma::mat>(d, d) * 1e-6;
    }

    prec_draw = arma::wishrnd(Lambda_inv, nu);
    if (prec_draw.n_rows != static_cast<arma::uword>(d) || prec_draw.n_cols != static_cast<arma::uword>(d)) {
      Rcpp::stop("prec_draw has wrong dimensions after Wishart draw");
    }
    prec_draw = ensureSymmetric(prec_draw);
  } catch (const std::exception& e) {
    Rcpp::stop("Error in Wishart draw: " + std::string(e.what()));
  }

  // Draw mean from multivariate normal
  arma::mat prec_mu = kappa0 * prec_draw;
  prec_mu = ensureSymmetric(prec_mu);
  arma::mat cov_mu;
  try {
    cov_mu = arma::inv_sympd(prec_mu);
    if (!cov_mu.is_finite()) {
      throw std::runtime_error("Non-finite covariance matrix");
    }
  } catch (...) {
    try {
      cov_mu = arma::inv(prec_mu);
      if (!cov_mu.is_finite()) {
        throw std::runtime_error("Non-finite covariance matrix");
      }
    } catch (...) {
      cov_mu = arma::pinv(prec_mu);
    }
  }

  arma::vec mu_draw;
  try {
    mu_draw = arma::mvnrnd(mu0, cov_mu);
    if (mu_draw.n_elem != static_cast<arma::uword>(d)) {
      Rcpp::stop("mu_draw has wrong size: " + std::to_string(mu_draw.n_elem));
    }
  } catch (const std::exception& e) {
    Rcpp::stop("Error in multivariate normal draw: " + std::string(e.what()));
  }

  return flatten_params(mu_draw, prec_draw);
}

int MVNormalMixing::param_dim() const {
  // Mean vector (d) + precision matrix (d*d)
  return d + d * d;
}

arma::vec MVNormalMixing::flatten_params(const arma::vec& mu,
                                         const arma::mat& Sigma) const {
  arma::vec params(param_dim());

  // First d elements are the mean with bounds checking
  if (d > 0 && mu.n_elem >= static_cast<arma::uword>(d)) {
    params.subvec(0, d-1) = mu;
  }

  // Remaining elements are the precision matrix (column-major order) with bounds checking
  if (d > 0 && Sigma.n_rows == static_cast<arma::uword>(d) && Sigma.n_cols == static_cast<arma::uword>(d)) {
    arma::vec sigma_vec = arma::vectorise(Sigma);
    int sigma_start = d;
    int sigma_end = param_dim() - 1;
    if (sigma_end >= sigma_start && sigma_vec.n_elem == static_cast<arma::uword>(sigma_end - sigma_start + 1)) {
      params.subvec(sigma_start, sigma_end) = sigma_vec;
    }
  }

  return params;
}

void MVNormalMixing::unflatten_params(const arma::vec& params,
                                      arma::vec& mu, arma::mat& Sigma) const {
  // Extract mean with bounds checking
  if (d > 0 && params.n_elem >= static_cast<arma::uword>(d)) {
    mu = params.subvec(0, d-1);
  } else {
    mu.set_size(d);
    mu.zeros();
  }

  // Extract precision matrix with bounds checking
  int expected_size = param_dim();
  if (d > 0 && params.n_elem >= static_cast<arma::uword>(expected_size)) {
    arma::vec sigma_vec = params.subvec(d, expected_size-1);
    if (sigma_vec.n_elem == static_cast<arma::uword>(d * d)) {
      Sigma = arma::reshape(sigma_vec, d, d);
      Sigma = ensureSymmetric(Sigma);
    } else {
      Sigma = arma::eye<arma::mat>(d, d);
    }
  } else {
    Sigma = arma::eye<arma::mat>(d, d);
  }
}

double MVNormalMixing::predictive_probability(const arma::vec& data_point) const {
  // Normal-Wishart predictive distribution
  // This is the multivariate Student's t-distribution
  
  arma::vec x = data_point;
  
  // Predictive parameters
  arma::vec mu_pred = mu0;
  double nu_pred = nu - d + 1;
  
  // Scale matrix for predictive distribution
  arma::mat Lambda_inv;
  try {
    Lambda_inv = arma::inv_sympd(Lambda);
  } catch (...) {
    Lambda_inv = arma::pinv(Lambda);
  }
  
  arma::mat Scale = Lambda_inv * (kappa0 + 1) / (kappa0 * nu_pred);
  
  // Compute multivariate t log-density
  arma::vec x_centered = x - mu_pred;
  
  double log_det_val;
  double sign;
  arma::log_det(log_det_val, sign, Scale);
  
  if (sign <= 0) {
    return 0.0;  // Invalid covariance
  }
  
  arma::mat Scale_inv;
  try {
    Scale_inv = arma::inv_sympd(Scale);
  } catch (...) {
    return 0.0;
  }
  
  double quad_form = arma::as_scalar(x_centered.t() * Scale_inv * x_centered);
  
  // Log probability of multivariate t-distribution
  double log_prob = std::lgamma((nu_pred + d) / 2.0) - std::lgamma(nu_pred / 2.0) - 
                   (d / 2.0) * std::log(nu_pred * M_PI) - 0.5 * log_det_val - 
                   ((nu_pred + d) / 2.0) * std::log(1 + quad_form / nu_pred);
  
  return std::exp(log_prob);
}

} // namespace dirichletprocess
