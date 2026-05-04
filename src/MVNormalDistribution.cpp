// src/MVNormalDistribution.cpp
#include "MVNormalDistribution.h"
#include "RcppConversions.h"
#include <RcppArmadillo.h>

namespace dp {

// Helper function to parse covariance model from string
CovarianceModel parseCovarianceModel(const std::string& model) {
  if (model == "E") return CovarianceModel::E;
  else if (model == "V") return CovarianceModel::V;
  else if (model == "EII") return CovarianceModel::EII;
  else if (model == "VII") return CovarianceModel::VII;
  else if (model == "EEI") return CovarianceModel::EEI;
  else if (model == "VEI") return CovarianceModel::VEI;
  else if (model == "EVI") return CovarianceModel::EVI;
  else if (model == "VVI") return CovarianceModel::VVI;
  else return CovarianceModel::FULL;
}

// MVNormalMixingDistribution implementation
MVNormalMixingDistribution::MVNormalMixingDistribution(const Rcpp::List& priorParams) {
  distribution = "mvnormal";
  conjugate = true;
  priorParameters = priorParams;

  // Extract prior parameters
  if (priorParams.containsElementNamed("mu0")) {
    Rcpp::NumericVector mu0_vec = Rcpp::as<Rcpp::NumericVector>(priorParams["mu0"]);
    mu0 = arma::vec(mu0_vec.begin(), mu0_vec.size());
  }

  if (priorParams.containsElementNamed("kappa0")) {
    kappa0 = Rcpp::as<double>(priorParams["kappa0"]);
  }

  if (priorParams.containsElementNamed("Lambda")) {
    Lambda = Rcpp::as<arma::mat>(priorParams["Lambda"]);
  }

  if (priorParams.containsElementNamed("nu")) {
    nu = Rcpp::as<double>(priorParams["nu"]);
  }

  // Extract covariance model
  if (priorParams.containsElementNamed("covModel")) {
    std::string modelStr = Rcpp::as<std::string>(priorParams["covModel"]);
    covModel = parseCovarianceModel(modelStr);
  } else {
    covModel = CovarianceModel::FULL;
  }
}

MVNormalMixingDistribution::~MVNormalMixingDistribution() {
  // Destructor
}

// Get number of covariance parameters for the model
int MVNormalMixingDistribution::getNumCovParams(int d) const {
  switch (covModel) {
  case CovarianceModel::E:
    return 1;  // One variance parameter
  case CovarianceModel::V:
    return 1;  // One variance parameter per observation
  case CovarianceModel::EII:
    return 1;  // One volume parameter
  case CovarianceModel::VII:
    return 1;  // One volume parameter per cluster
  case CovarianceModel::EEI:
    return d;  // Diagonal elements (same across clusters)
  case CovarianceModel::VEI:
    return d + 1;  // Volume + diagonal shape
  case CovarianceModel::EVI:
    return d;  // Diagonal elements (varying across clusters)
  case CovarianceModel::VVI:
    return d;  // Full diagonal per cluster
  case CovarianceModel::FULL:
  default:
    return d * (d + 1) / 2;  // Full covariance matrix
  }
}

// Construct covariance matrix from parameters based on model
arma::mat MVNormalMixingDistribution::constructCovarianceMatrix(
    const arma::vec& params, int d) const {

  arma::mat sigma(d, d, arma::fill::zeros);

  switch (covModel) {
  case CovarianceModel::E:
  case CovarianceModel::V:
    // For univariate case, return scalar as 1x1 matrix
    sigma(0, 0) = params(0);
    break;

  case CovarianceModel::EII:
  case CovarianceModel::VII:
    // Spherical: sigma = lambda * I
    sigma = params(0) * arma::eye(d, d);
    break;

  case CovarianceModel::EEI:
    // Diagonal, equal volume and shape
    for (int i = 0; i < d; i++) {
      sigma(i, i) = params(i);
    }
    break;

  case CovarianceModel::VEI:
    // Diagonal, varying volume, equal shape
    // params[0] = volume, params[1:d] = shape
  {
    double volume = params(0);
    arma::vec shape = params.subvec(1, d);  // This should be params.subvec(1, d) for indices 1 to d
    shape = shape / arma::prod(shape);  // Normalize shape
    for (int i = 0; i < d; i++) {
      sigma(i, i) = volume * shape(i);
    }
  }
    break;

  case CovarianceModel::EVI:
  case CovarianceModel::VVI:
    // Diagonal matrices
    for (int i = 0; i < d; i++) {
      sigma(i, i) = params(i);
    }
    break;

  case CovarianceModel::FULL:
  default:
    // Full covariance matrix (lower triangular parameterization)
  {
    int idx = 0;
    for (int i = 0; i < d; i++) {
      for (int j = 0; j <= i; j++) {
        sigma(i, j) = params(idx);
        if (i != j) sigma(j, i) = params(idx);
        idx++;
      }
    }
  }
    break;
  }

  return sigma;
}

// Extract covariance parameters from matrix based on model
arma::vec MVNormalMixingDistribution::extractCovarianceParams(
    const arma::mat& sigma) const {

  int d = sigma.n_rows;
  int nParams = getNumCovParams(d);
  arma::vec params(nParams);

  switch (covModel) {
  case CovarianceModel::E:
  case CovarianceModel::V:
    params(0) = sigma(0, 0);
    break;

  case CovarianceModel::EII:
  case CovarianceModel::VII:
    // Extract volume (average of diagonal elements)
    params(0) = arma::trace(sigma) / d;
    break;

  case CovarianceModel::EEI:
  case CovarianceModel::EVI:
  case CovarianceModel::VVI:
    // Extract diagonal elements
    for (int i = 0; i < d; i++) {
      params(i) = sigma(i, i);
    }
    break;

  case CovarianceModel::VEI:
    // Extract volume and shape
  {
    arma::vec diag = sigma.diag();
    params(0) = arma::prod(diag);  // Volume
    arma::vec shape = diag / std::pow(params(0), 1.0/d);
    // Fix: subvec(1, d) goes out of bounds when trying to assign d elements to indices 1-d
    // We need to assign d elements starting at index 1, so use subvec(1, d)
    if (d > 0) {
      params.subvec(1, d) = shape;
    }
  }
    break;

  case CovarianceModel::FULL:
  default:
    // Extract lower triangular elements
  {
    int idx = 0;
    for (int i = 0; i < d; i++) {
      for (int j = 0; j <= i; j++) {
        params(idx) = sigma(i, j);
        idx++;
      }
    }
  }
    break;
  }

  return params;
}

arma::vec MVNormalMixingDistribution::mvnLikelihood(const arma::mat& x,
                                                    const arma::vec& mu,
                                                    const arma::mat& sigma) const {
  int n = x.n_rows;
  int d = x.n_cols;
  arma::vec result(n);

  // Ensure sigma is symmetric before inversion
  arma::mat sigma_sym = ensureSymmetric(sigma);

  // sigma here is actually a precision matrix (inverse covariance)
  // We need to convert it to covariance for likelihood calculation
  arma::mat covariance;
  try {
    covariance = arma::inv_sympd(sigma_sym);
  } catch(...) {
    result.fill(1e-300);
    return result;
  }

  // Calculate log-determinant and inverse of covariance
  double log_det_val;
  double sign;
  arma::log_det(log_det_val, sign, covariance);

  if (sign <= 0) {
    // Covariance is not positive definite
    result.fill(1e-300);
    return result;
  }

  // Use the precision matrix (sigma_sym) directly for the quadratic form
  double log_const = -0.5 * d * std::log(2.0 * M_PI) - 0.5 * log_det_val;

  for (int i = 0; i < n; i++) {
    arma::vec x_centered = x.row(i).t() - mu;
    double quad_form = arma::as_scalar(x_centered.t() * sigma_sym * x_centered);
    result(i) = std::exp(log_const - 0.5 * quad_form);
  }

  return result;
}

Rcpp::NumericVector MVNormalMixingDistribution::likelihood(const arma::vec& x,
                                                           const Rcpp::List& theta) const {
  // Extract parameters - handle the array structure
  Rcpp::NumericVector mu_array = theta["mu"];
  Rcpp::NumericVector sig_array = theta["sig"];

  // Get dimensions
  Rcpp::IntegerVector mu_dim = mu_array.attr("dim");
  Rcpp::IntegerVector sig_dim = sig_array.attr("dim");

  int d = mu_dim[1]; // Number of dimensions
  int n_clusters = mu_dim[2]; // Number of clusters

  // Convert x to matrix (single row)
  arma::mat x_mat(1, x.n_elem);
  x_mat.row(0) = x.t();

  Rcpp::NumericVector result(n_clusters);

  for (int k = 0; k < n_clusters; k++) {
    // Extract mu for cluster k
    arma::vec mu_k(d);
    for (int j = 0; j < d; j++) {
      mu_k(j) = mu_array[j + k * d];
    }

    // Extract sigma for cluster k based on covariance model
    arma::mat sig_k(d, d);

    if (covModel == CovarianceModel::FULL) {
      // Full precision matrix
      for (int i = 0; i < d; i++) {
        for (int j = 0; j < d; j++) {
          sig_k(i, j) = sig_array[i + j * d + k * d * d];
        }
      }
    } else {
      // Reconstruct from parameters
      int nParams = getNumCovParams(d);
      arma::vec params(nParams);
      for (int i = 0; i < nParams; i++) {
        params(i) = sig_array[i + k * nParams];
      }
      arma::mat cov = constructCovarianceMatrix(params, d);
      sig_k = arma::inv_sympd(cov); // Convert to precision
    }

    arma::vec lik = mvnLikelihood(x_mat, mu_k, sig_k);
    result[k] = lik(0);
  }

  return result;
}

Rcpp::List MVNormalMixingDistribution::posteriorParameters(const arma::mat& x) const {
  int n = x.n_rows;
  int d = x.n_cols;

  // Special case: no data
  if (n == 0) {
    Rcpp::NumericVector mu0_vec = Rcpp::wrap(mu0);
    mu0_vec.attr("dim") = R_NilValue;

    return Rcpp::List::create(
      Rcpp::Named("mu_n") = mu0_vec,
      Rcpp::Named("t_n") = ensureSymmetric(Lambda),
      Rcpp::Named("Lambda_n") = ensureSymmetric(Lambda),
      Rcpp::Named("kappa_n") = kappa0,
      Rcpp::Named("nu_n") = nu
    );
  }

  // Compute sample statistics
  arma::vec x_bar = arma::mean(x, 0).t();

  // Posterior parameters for mean (same for all models)
  double kappa_n = kappa0 + n;
  arma::vec mu_n_arma = (kappa0 * mu0 + n * x_bar) / kappa_n;
  double nu_n = nu + n;

  // Compute scatter matrix based on covariance model
  arma::mat S(d, d, arma::fill::zeros);

  switch (covModel) {
  case CovarianceModel::E:
  case CovarianceModel::V:
    // Univariate case
    if (n > 1) {
      double var = arma::as_scalar(arma::var(x));
      S(0, 0) = (n - 1) * var;
    }
    break;

  case CovarianceModel::EII:
  case CovarianceModel::VII:
    // Spherical covariance
    if (n > 1) {
      arma::mat centered = x.each_row() - x_bar.t();
      double trace_S = arma::accu(centered % centered) / (n - 1);
      S = (trace_S / d) * arma::eye(d, d);
    }
    break;

  case CovarianceModel::EEI:
  case CovarianceModel::VEI:
  case CovarianceModel::EVI:
  case CovarianceModel::VVI:
    // Diagonal covariance
    if (n > 1) {
      arma::vec diag_var = arma::var(x, 0, 0).t();
      S = arma::diagmat(diag_var) * (n - 1);
    }
    break;

  case CovarianceModel::FULL:
  default:
    // Full covariance
    if (n > 1) {
      S = (n - 1) * arma::cov(x);
      S = ensureSymmetric(S);
    }
    break;
  }

  // Update Lambda (called t_n in R code)
  arma::vec diff = x_bar - mu0;
  arma::mat t_n = Lambda + S + (kappa0 * n / kappa_n) * (diff * diff.t());
  t_n = ensureSymmetric(t_n);

  // Convert arma::vec to plain Rcpp::NumericVector
  Rcpp::NumericVector mu_n_vec = Rcpp::wrap(mu_n_arma);
  mu_n_vec.attr("dim") = R_NilValue;

  return Rcpp::List::create(
    Rcpp::Named("mu_n") = mu_n_vec,
    Rcpp::Named("t_n") = t_n,
    Rcpp::Named("Lambda_n") = t_n,
    Rcpp::Named("kappa_n") = kappa_n,
    Rcpp::Named("nu_n") = nu_n
  );
}

Rcpp::List MVNormalMixingDistribution::priorDraw(int n) const {
  int d = mu0.n_elem;

  // Validate input parameters
  if (n <= 0) {
    Rcpp::stop("Number of draws must be positive");
  }
  if (d <= 0) {
    Rcpp::stop("Dimension must be positive");
  }

  // Arrays to store results
  Rcpp::NumericVector mu_arr = Rcpp::NumericVector(Rcpp::Dimension(1, d, n));
  Rcpp::NumericVector sig_arr;

  // Determine storage size for covariance parameters
  if (covModel == CovarianceModel::FULL) {
    sig_arr = Rcpp::NumericVector(Rcpp::Dimension(d, d, n));
  } else {
    int nCovParams = getNumCovParams(d);
    if (nCovParams <= 0) {
      Rcpp::stop("Invalid number of covariance parameters");
    }
    sig_arr = Rcpp::NumericVector(Rcpp::Dimension(nCovParams, n));
  }

  // Ensure Lambda is symmetric
  arma::mat Lambda_sym = ensureSymmetric(Lambda);

  for (int i = 0; i < n; i++) {
    // Draw precision from Wishart with additional validation
    // Check condition number to avoid numerical issues
    double rcond = arma::rcond(Lambda_sym);
    arma::mat Lambda_safe = Lambda_sym;
    if (rcond < 1e-12) {
      // Matrix is too ill-conditioned, use regularized version
      Lambda_safe += arma::eye<arma::mat>(d, d) * 1e-6;
    }
    
    arma::mat prec_draw = arma::wishrnd(Lambda_safe, nu);

    // Ensure the drawn precision matrix is symmetric
    prec_draw = ensureSymmetric(prec_draw);

    // Draw mu from Multivariate Normal given precision
    arma::mat cov_mu = arma::inv_sympd(prec_draw / kappa0);
    arma::vec mu_draw = arma::mvnrnd(mu0, cov_mu);

    // Store mu (array has dimensions 1 x d x n)
    for (int j = 0; j < d; j++) {
      mu_arr[0 + j * 1 + i * 1 * d] = mu_draw(j);
    }

    // Store covariance parameters based on model
    if (covModel == CovarianceModel::FULL) {
      // Store full precision matrix
      for (int j = 0; j < d; j++) {
        for (int k = 0; k < d; k++) {
          sig_arr[j + k * d + i * d * d] = prec_draw(j, k);
        }
      }
    } else {
      // Convert to covariance and extract model-specific parameters
      arma::mat cov_draw = arma::inv_sympd(prec_draw);
      arma::vec params = extractCovarianceParams(cov_draw);
      for (arma::uword j = 0; j < params.n_elem; j++) {
        sig_arr[j + i * params.n_elem] = params(j);
      }
    }
  }

  return Rcpp::List::create(
    Rcpp::Named("mu") = mu_arr,
    Rcpp::Named("sig") = sig_arr
  );
}

Rcpp::List MVNormalMixingDistribution::posteriorDraw(const arma::mat& x, int n) const {
  // Validate input parameters
  if (n <= 0) {
    Rcpp::stop("Number of draws must be positive");
  }
  if (x.n_rows == 0 || x.n_cols == 0) {
    Rcpp::stop("Data matrix cannot be empty");
  }

  // Get posterior parameters
  Rcpp::List post_params = posteriorParameters(x);

  // Extract with error checking
  if (!post_params.containsElementNamed("mu_n") || !post_params.containsElementNamed("t_n") ||
      !post_params.containsElementNamed("kappa_n") || !post_params.containsElementNamed("nu_n")) {
    Rcpp::stop("posteriorParameters missing required elements");
  }

  SEXP kappa_n_sexp = post_params["kappa_n"];
  SEXP nu_n_sexp = post_params["nu_n"];
  
  if (TYPEOF(kappa_n_sexp) != REALSXP && TYPEOF(kappa_n_sexp) != INTSXP) {
    Rcpp::stop("kappa_n is not numeric, got type %d", TYPEOF(kappa_n_sexp));
  }
  
  if (TYPEOF(nu_n_sexp) != REALSXP && TYPEOF(nu_n_sexp) != INTSXP) {
    Rcpp::stop("nu_n is not numeric, got type %d", TYPEOF(nu_n_sexp));
  }

  arma::vec mu_n = Rcpp::as<arma::vec>(post_params["mu_n"]);
  arma::mat t_n = Rcpp::as<arma::mat>(post_params["t_n"]);
  double kappa_n = Rcpp::as<double>(kappa_n_sexp);
  double nu_n = Rcpp::as<double>(nu_n_sexp);

  int d = mu_n.n_elem;
  
  if (d <= 0) {
    Rcpp::stop("Dimension must be positive");
  }

  // Arrays to store results
  Rcpp::NumericVector mu_arr = Rcpp::NumericVector(Rcpp::Dimension(1, d, n));
  Rcpp::NumericVector sig_arr;

  // Determine storage size for covariance parameters
  if (covModel == CovarianceModel::FULL) {
    sig_arr = Rcpp::NumericVector(Rcpp::Dimension(d, d, n));
  } else {
    int nCovParams = getNumCovParams(d);
    if (nCovParams <= 0) {
      Rcpp::stop("Invalid number of covariance parameters");
    }
    sig_arr = Rcpp::NumericVector(Rcpp::Dimension(nCovParams, n));
  }

  // Ensure t_n is symmetric
  arma::mat t_n_sym = ensureSymmetric(t_n);

  for (int i = 0; i < n; i++) {
    // Draw precision from Wishart with additional validation
    // Check condition number to avoid numerical issues
    double rcond = arma::rcond(t_n_sym);
    arma::mat t_n_safe = t_n_sym;
    if (rcond < 1e-12) {
      // Matrix is too ill-conditioned, use regularized version
      t_n_safe += arma::eye<arma::mat>(d, d) * 1e-6;
    }
    
    arma::mat prec_draw = arma::wishrnd(t_n_safe, nu_n);

    // Ensure the drawn precision matrix is symmetric
    prec_draw = ensureSymmetric(prec_draw);

    // Draw mu from Multivariate Normal given precision
    arma::mat cov_mu = arma::inv_sympd(ensureSymmetric(prec_draw / kappa_n));
    arma::vec mu_draw = arma::mvnrnd(mu_n, cov_mu);

    // Store mu (array has dimensions 1 x d x n)
    for (int j = 0; j < d; j++) {
      mu_arr[0 + j * 1 + i * 1 * d] = mu_draw(j);
    }

    // Store covariance parameters based on model
    if (covModel == CovarianceModel::FULL) {
      // Store full precision matrix
      for (int j = 0; j < d; j++) {
        for (int k = 0; k < d; k++) {
          sig_arr[j + k * d + i * d * d] = prec_draw(j, k);
        }
      }
    } else {
      // Convert to covariance and extract model-specific parameters
      arma::mat cov_draw = arma::inv_sympd(prec_draw);
      arma::vec params = extractCovarianceParams(cov_draw);
      for (arma::uword j = 0; j < params.n_elem; j++) {
        sig_arr[j + i * params.n_elem] = params(j);
      }
    }
  }

  return Rcpp::List::create(
    Rcpp::Named("mu") = mu_arr,
    Rcpp::Named("sig") = sig_arr
  );
}

Rcpp::NumericVector MVNormalMixingDistribution::predictive(const arma::mat& x) const {
  int n = x.n_rows;
  int d = x.n_cols;
  Rcpp::NumericVector result(n);

  double pi_const = std::pow(M_PI, -0.5 * d);

  for (int i = 0; i < n; i++) {
    arma::mat x_i = x.row(i);
    Rcpp::List post_params = posteriorParameters(x_i);

    arma::vec mu_n = Rcpp::as<arma::vec>(post_params["mu_n"]);
    arma::mat t_n = Rcpp::as<arma::mat>(post_params["t_n"]);
    double kappa_n = Rcpp::as<double>(post_params["kappa_n"]);
    double nu_n = Rcpp::as<double>(post_params["nu_n"]);

    // Calculate determinants
    double log_det_Lambda, log_det_t_n;
    double sign_Lambda, sign_t_n;
    arma::log_det(log_det_Lambda, sign_Lambda, Lambda);
    arma::log_det(log_det_t_n, sign_t_n, t_n);

    // Handle potential numerical issues
    if (sign_Lambda <= 0 || sign_t_n <= 0) {
      result[i] = 1e-300;
      continue;
    }

    double ratio_det = std::exp((nu / 2.0) * (log_det_Lambda - log_det_t_n));
    double ratio_kappa = std::pow(kappa0 / kappa_n, d / 2.0);

    // Compute multivariate gamma ratio
    double log_gamma_ratio = 0.0;
    for (int j = 1; j <= d; j++) {
      log_gamma_ratio += lgamma((nu_n + 1.0 - j) / 2.0) - lgamma((nu + 1.0 - j) / 2.0);
    }
    double gamma_ratio = std::exp(log_gamma_ratio);

    result[i] = pi_const * ratio_kappa * ratio_det * gamma_ratio;
  }

  return result;
}

// Static methods
Rcpp::List MVNormalMixingDistribution::priorDrawStatic(const Rcpp::List& priorParams, int n) {
  MVNormalMixingDistribution md(priorParams);
  return md.priorDraw(n);
}

Rcpp::List MVNormalMixingDistribution::posteriorDrawStatic(const Rcpp::List& priorParams,
                                                           const arma::mat& x, int n) {
  MVNormalMixingDistribution md(priorParams);
  return md.posteriorDraw(x, n);
}

// ConjugateMVNormalDP implementation
ConjugateMVNormalDP::ConjugateMVNormalDP() : mixingDistribution(nullptr), numberClusters(0) {
  // Constructor
}

ConjugateMVNormalDP::~ConjugateMVNormalDP() {
  // No manual delete needed - using smart pointer
}

void ConjugateMVNormalDP::initialize(const Rcpp::List& dpObj) {
  // Extract data
  data = Rcpp::as<arma::mat>(dpObj["data"]);

  // Extract cluster labels (already 0-indexed from R wrapper)
  Rcpp::IntegerVector labels = dpObj["clusterLabels"];
  clusterLabels = Rcpp::as<arma::uvec>(labels);

  // Initialize mixing distribution
  Rcpp::List mdObj = dpObj["mixingDistribution"];
  Rcpp::List priorParams = mdObj["priorParameters"];
  try {
    mixingDistribution = std::unique_ptr<MVNormalMixingDistribution>(new MVNormalMixingDistribution(priorParams));
  } catch (const std::exception& e) {
    Rcpp::stop("Failed to initialize mixing distribution: %s", e.what());
  }

  // Extract cluster parameters if they exist
  if (dpObj.containsElementNamed("clusterParameters")) {
    clusterParameters = dpObj["clusterParameters"];
  }

  // Extract other parameters with error checking
  if (dpObj.containsElementNamed("alpha")) {
    SEXP alpha_sexp = dpObj["alpha"];
    if (TYPEOF(alpha_sexp) != REALSXP && TYPEOF(alpha_sexp) != INTSXP) {
      Rcpp::stop("alpha is not numeric, got type %d", TYPEOF(alpha_sexp));
    }
    alpha = Rcpp::as<double>(alpha_sexp);
  } else {
    alpha = 1.0; // Default
  }

  if (dpObj.containsElementNamed("alphaPriorParameters")) {
    alphaPriorParameters = dpObj["alphaPriorParameters"];
  }

  // Get dimensions
  n = data.n_rows;

  // Extract predictive array if it exists
  if (dpObj.containsElementNamed("predictiveArray")) {
    predictiveArray = Rcpp::as<Rcpp::NumericVector>(dpObj["predictiveArray"]);
  } else {
    predictiveArray = Rcpp::NumericVector(n);
  }

  // Extract points per cluster
  if (dpObj.containsElementNamed("pointsPerCluster")) {
    Rcpp::IntegerVector ppc = dpObj["pointsPerCluster"];
    pointsPerCluster = Rcpp::as<arma::uvec>(ppc);
  }

  // Count clusters
  numberClusters = arma::max(clusterLabels) + 1;
}

void ConjugateMVNormalDP::initialisePredictive() {
  // Calculate predictive probabilities for all data points
  predictiveArray = mixingDistribution->predictive(data);
}

void ConjugateMVNormalDP::updateAlpha() {
  // Same implementation as Normal case - follows Escobar & West (1995)
  double x = R::rbeta(alpha + 1.0, n);

  Rcpp::NumericVector alphaPriors = Rcpp::as<Rcpp::NumericVector>(alphaPriorParameters);

  double pi1 = alphaPriors[0] + numberClusters - 1.0;
  double pi2 = n * (alphaPriors[1] - log(x));
  double pi_ratio = pi1 / (pi1 + pi2);

  double postShape, postRate;
  if (R::runif(0, 1) < pi_ratio) {
    postShape = alphaPriors[0] + numberClusters;
  } else {
    postShape = alphaPriors[0] + numberClusters - 1.0;
  }
  postRate = alphaPriors[1] - log(x);

  alpha = R::rgamma(postShape, 1.0/postRate);
}

Rcpp::List ConjugateMVNormalDP::clusterLabelChange(int i, int newLabel, int currentLabel) {
  if (newLabel == currentLabel) {
    return Rcpp::List::create(
      Rcpp::Named("clusterLabels") = clusterLabels,
      Rcpp::Named("pointsPerCluster") = pointsPerCluster,
      Rcpp::Named("clusterParameters") = clusterParameters,
      Rcpp::Named("numberClusters") = numberClusters
    );
  }

  arma::mat x_i = data.row(i);

  // Extract current parameters
  Rcpp::NumericVector mu_array = Rcpp::clone(Rcpp::as<Rcpp::NumericVector>(clusterParameters["mu"]));
  Rcpp::NumericVector sig_array = Rcpp::clone(Rcpp::as<Rcpp::NumericVector>(clusterParameters["sig"]));

  // Get dimensions
  Rcpp::IntegerVector mu_dim = mu_array.attr("dim");
  int d = mu_dim[1];
  int current_max_clusters = mu_dim[2];

  // 1. Remove point from old cluster
  pointsPerCluster[currentLabel]--;

  // 2. Assign point to new cluster
  clusterLabels[i] = newLabel;

  if (newLabel == numberClusters) {
    // New cluster case
    numberClusters++;
    pointsPerCluster.resize(numberClusters);
    pointsPerCluster[newLabel] = 1;

    // Check if we need to expand the parameter arrays
    if (numberClusters > current_max_clusters) {
      // Need to expand arrays - double the size or add at least 10 more slots
      int new_max_clusters = std::max(current_max_clusters * 2, numberClusters + 10);

      // Create new arrays with expanded size based on covariance model
      Rcpp::NumericVector new_mu_array = Rcpp::NumericVector(Rcpp::Dimension(1, d, new_max_clusters));
      Rcpp::NumericVector new_sig_array;

      if (mixingDistribution->getCovarianceModel() == CovarianceModel::FULL) {
        new_sig_array = Rcpp::NumericVector(Rcpp::Dimension(d, d, new_max_clusters));
      } else {
        int nCovParams = mixingDistribution->getNumCovParams(d);
        new_sig_array = Rcpp::NumericVector(Rcpp::Dimension(nCovParams, new_max_clusters));
      }

      // Initialize new arrays with NA
      new_mu_array.fill(NA_REAL);
      new_sig_array.fill(NA_REAL);

      // Copy existing parameters
      for (int k = 0; k < current_max_clusters; k++) {
        for (int j = 0; j < d; j++) {
          new_mu_array[j + k * d] = mu_array[j + k * d];
        }

        if (mixingDistribution->getCovarianceModel() == CovarianceModel::FULL) {
          for (int r_idx = 0; r_idx < d; r_idx++) {
            for (int c_idx = 0; c_idx < d; c_idx++) {
              new_sig_array[r_idx + c_idx * d + k * d * d] =
                sig_array[r_idx + c_idx * d + k * d * d];
            }
          }
        } else {
          int nCovParams = mixingDistribution->getNumCovParams(d);
          for (int j = 0; j < nCovParams; j++) {
            new_sig_array[j + k * nCovParams] = sig_array[j + k * nCovParams];
          }
        }
      }

      mu_array = new_mu_array;
      sig_array = new_sig_array;
      current_max_clusters = new_max_clusters;
    }

    // Draw parameters for new cluster
    Rcpp::List postDraw = mixingDistribution->posteriorDraw(x_i, 1);
    Rcpp::NumericVector new_mu = postDraw["mu"];
    Rcpp::NumericVector new_sig = postDraw["sig"];

    // Add new cluster parameters at position newLabel
    for (int j = 0; j < d; j++) {
      mu_array[j + newLabel * d] = new_mu[j];
    }

    if (mixingDistribution->getCovarianceModel() == CovarianceModel::FULL) {
      for (int r_idx = 0; r_idx < d; r_idx++) {
        for (int c_idx = 0; c_idx < d; c_idx++) {
          sig_array[r_idx + c_idx * d + newLabel * d * d] = new_sig[r_idx + c_idx * d];
        }
      }
    } else {
      int nCovParams = mixingDistribution->getNumCovParams(d);
      for (int j = 0; j < nCovParams; j++) {
        sig_array[j + newLabel * nCovParams] = new_sig[j];
      }
    }

    clusterParameters["mu"] = mu_array;
    clusterParameters["sig"] = sig_array;

  } else {
    // Existing cluster
    pointsPerCluster[newLabel]++;
  }

  // 3. If old cluster is empty, remove it
  if (pointsPerCluster[currentLabel] == 0) {
    pointsPerCluster.shed_row(currentLabel);
    numberClusters--;

    // Shift labels
    for (arma::uword j = 0; j < clusterLabels.n_elem; j++) {
      if (clusterLabels[j] > (unsigned int)currentLabel) {
        clusterLabels[j]--;
      }
    }

    // Compact the parameter arrays by shifting left
    for (int k = currentLabel; k < numberClusters; k++) {
      // Copy from k+1 to k
      for (int j = 0; j < d; j++) {
        mu_array[j + k * d] = mu_array[j + (k+1) * d];
      }

      if (mixingDistribution->getCovarianceModel() == CovarianceModel::FULL) {
        for (int r_idx = 0; r_idx < d; r_idx++) {
          for (int c_idx = 0; c_idx < d; c_idx++) {
            sig_array[r_idx + c_idx * d + k * d * d] =
              sig_array[r_idx + c_idx * d + (k+1) * d * d];
          }
        }
      } else {
        int nCovParams = mixingDistribution->getNumCovParams(d);
        for (int j = 0; j < nCovParams; j++) {
          sig_array[j + k * nCovParams] = sig_array[j + (k+1) * nCovParams];
        }
      }
    }

    // Clear the last slot (now unused)
    for (int j = 0; j < d; j++) {
      mu_array[j + numberClusters * d] = NA_REAL;
    }

    if (mixingDistribution->getCovarianceModel() == CovarianceModel::FULL) {
      for (int r_idx = 0; r_idx < d; r_idx++) {
        for (int c_idx = 0; c_idx < d; c_idx++) {
          sig_array[r_idx + c_idx * d + numberClusters * d * d] = NA_REAL;
        }
      }
    } else {
      int nCovParams = mixingDistribution->getNumCovParams(d);
      for (int j = 0; j < nCovParams; j++) {
        sig_array[j + numberClusters * nCovParams] = NA_REAL;
      }
    }

    clusterParameters["mu"] = mu_array;
    clusterParameters["sig"] = sig_array;
  }

  return Rcpp::List::create(
    Rcpp::Named("clusterLabels") = clusterLabels,
    Rcpp::Named("pointsPerCluster") = pointsPerCluster,
    Rcpp::Named("clusterParameters") = clusterParameters,
    Rcpp::Named("numberClusters") = numberClusters
  );
}

void ConjugateMVNormalDP::clusterComponentUpdate() {
  // This method is kept from the original but updated to handle different covariance models
  for (int i = 0; i < n; i++) {
    int currentLabel = clusterLabels[i];

    // Remove point from current cluster
    pointsPerCluster[currentLabel]--;

    // Calculate probabilities for existing clusters
    Rcpp::NumericVector probs(numberClusters + 1);

    // Get parameters from clusterParameters list
    Rcpp::NumericVector mu_array = clusterParameters["mu"];
    Rcpp::NumericVector sig_array = clusterParameters["sig"];

    // Get dimensions
    Rcpp::IntegerVector mu_dim = mu_array.attr("dim");
    int d = mu_dim[1];
    int max_clusters = mu_dim[2];

    // Probability for existing clusters with bounds checking
    for (int j = 0; j < numberClusters; j++) {
      if (j < 0 || j >= max_clusters) {
        Rcpp::stop("Cluster index %d exceeds parameter array size %d", j, max_clusters);
      }

      if (pointsPerCluster[j] > 0) {
        // Extract parameters for cluster j based on covariance model
        Rcpp::NumericVector mu_j = Rcpp::NumericVector(Rcpp::Dimension(1, d, 1));
        Rcpp::NumericVector sig_j;

        if (mixingDistribution->getCovarianceModel() == CovarianceModel::FULL) {
          sig_j = Rcpp::NumericVector(Rcpp::Dimension(d, d, 1));

          // Copy parameters for cluster j
          for (int k = 0; k < d; k++) {
            mu_j[k] = mu_array[k + j * d];
          }

          for (int k1 = 0; k1 < d; k1++) {
            for (int k2 = 0; k2 < d; k2++) {
              sig_j[k1 + k2 * d] = sig_array[k1 + k2 * d + j * d * d];
            }
          }
        } else {
          int nCovParams = mixingDistribution->getNumCovParams(d);
          sig_j = Rcpp::NumericVector(Rcpp::Dimension(nCovParams, 1));

          for (int k = 0; k < d; k++) {
            mu_j[k] = mu_array[k + j * d];
          }

          for (int k = 0; k < nCovParams; k++) {
            sig_j[k] = sig_array[k + j * nCovParams];
          }
        }

        // Create parameter list for cluster j
        Rcpp::List clusterParam = Rcpp::List::create(
          Rcpp::Named("mu") = mu_j,
          Rcpp::Named("sig") = sig_j
        );

        Rcpp::NumericVector lik = mixingDistribution->likelihood(data.row(i).t(), clusterParam);
        probs[j] = pointsPerCluster[j] * lik[0];
      } else {
        probs[j] = 0.0;
      }
    }

    // Probability for new cluster
    probs[numberClusters] = alpha * predictiveArray[i];

    // Handle edge cases
    for (int j = 0; j < probs.size(); j++) {
      if (!std::isfinite(probs[j])) probs[j] = 0.0;
    }

    if (Rcpp::is_true(Rcpp::all(probs == 0))) {
      probs.fill(1.0 / probs.size());
    }

    // Normalize
    double probSum = Rcpp::sum(probs);
    if (probSum <= 0) probSum = 1.0;
    probs = probs / probSum;

    // Sample new label
    int newLabel = 0;
    double u = R::runif(0, 1);
    double cumProb = 0.0;
    for (int j = 0; j < probs.size(); j++) {
      cumProb += probs[j];
      if (u <= cumProb) {
        newLabel = j;
        break;
      }
    }

    // Restore point count before calling clusterLabelChange
    pointsPerCluster[currentLabel]++;

    // Update cluster assignment
    Rcpp::List updateResult = clusterLabelChange(i, newLabel, currentLabel);

    // Update state from result
    clusterLabels = Rcpp::as<arma::uvec>(updateResult["clusterLabels"]);
    pointsPerCluster = Rcpp::as<arma::uvec>(updateResult["pointsPerCluster"]);
    clusterParameters = updateResult["clusterParameters"];
    numberClusters = updateResult["numberClusters"];
  }
}

void ConjugateMVNormalDP::clusterParameterUpdate() {
  // Update parameters for each cluster
  for (int k = 0; k < numberClusters; k++) {
    // Get data points assigned to this cluster
    arma::uvec clusterIndices = arma::find(clusterLabels == k);

    if (clusterIndices.n_elem > 0) {
      arma::mat clusterData = data.rows(clusterIndices);

      // Draw from posterior with error handling
      Rcpp::List postDraw;
      try {
        postDraw = mixingDistribution->posteriorDraw(clusterData, 1);
      } catch (const std::exception& e) {
        Rcpp::stop("Error in posteriorDraw for cluster %d: %s", k, e.what());
      }

      // Update cluster parameters with error checking
      if (!clusterParameters.containsElementNamed("mu") || !clusterParameters.containsElementNamed("sig")) {
        Rcpp::stop("clusterParameters missing mu or sig in clusterParameterUpdate");
      }
      
      Rcpp::NumericVector mu_array = clusterParameters["mu"];
      Rcpp::NumericVector sig_array = clusterParameters["sig"];

      if (!postDraw.containsElementNamed("mu") || !postDraw.containsElementNamed("sig")) {
        Rcpp::stop("postDraw missing mu or sig in clusterParameterUpdate");
      }
      
      // Extract with type checking
      SEXP mu_sexp = postDraw["mu"];
      SEXP sig_sexp = postDraw["sig"];
      
      if (TYPEOF(mu_sexp) != REALSXP) {
        Rcpp::stop("postDraw mu is not numeric, got type %d", TYPEOF(mu_sexp));  
      }
      
      if (TYPEOF(sig_sexp) != REALSXP) {
        Rcpp::stop("postDraw sig is not numeric, got type %d", TYPEOF(sig_sexp));
      }
      
      Rcpp::NumericVector new_mu = Rcpp::as<Rcpp::NumericVector>(mu_sexp);
      Rcpp::NumericVector new_sig = Rcpp::as<Rcpp::NumericVector>(sig_sexp);

      // Get dimensions with error checking
      SEXP dim_attr = mu_array.attr("dim");
      if (Rf_isNull(dim_attr)) {
        Rcpp::stop("mu array missing dim attribute in clusterParameterUpdate");
      }
      
      Rcpp::IntegerVector mu_dim = Rcpp::as<Rcpp::IntegerVector>(dim_attr);
      if (mu_dim.length() < 3) {
        Rcpp::stop("mu array must have 3 dimensions, got %d", mu_dim.length());
      }
      
      int d = mu_dim[1];
      int max_clusters = mu_dim[2];

      // Bounds check
      if (k >= max_clusters) {
        Rcpp::stop("Cluster index %d exceeds parameter array size %d in clusterParameterUpdate",
                   k, max_clusters);
      }

      // Update the k-th cluster parameters
      for (int j = 0; j < d; j++) {
        mu_array[j + k * d] = new_mu[j];
      }

      if (mixingDistribution->getCovarianceModel() == CovarianceModel::FULL) {
        // Ensure symmetry when storing precision matrix
        arma::mat sig_k(d, d);
        for (int i = 0; i < d; i++) {
          for (int j = 0; j < d; j++) {
            sig_k(i, j) = new_sig[i + j * d];
          }
        }
        sig_k = ensureSymmetric(sig_k);

        for (int i = 0; i < d; i++) {
          for (int j = 0; j < d; j++) {
            sig_array[i + j * d + k * d * d] = sig_k(i, j);
          }
        }
      } else {
        // Store model-specific parameters
        int nCovParams = mixingDistribution->getNumCovParams(d);
        for (int j = 0; j < nCovParams; j++) {
          sig_array[j + k * nCovParams] = new_sig[j];
        }
      }

      clusterParameters["mu"] = mu_array;
      clusterParameters["sig"] = sig_array;
    }
  }
}

Rcpp::List ConjugateMVNormalDP::updateClusterComponents() {
  // Initialize predictive array if needed
  initialisePredictive();

  // Update cluster assignments
  clusterComponentUpdate();

  return Rcpp::List::create(
    Rcpp::Named("clusterLabels") = Rcpp::IntegerVector(clusterLabels.begin(), clusterLabels.end()),
    Rcpp::Named("pointsPerCluster") = Rcpp::IntegerVector(pointsPerCluster.begin(), pointsPerCluster.end()),
    Rcpp::Named("numberClusters") = numberClusters,
    Rcpp::Named("clusterParameters") = clusterParameters
  );
}

Rcpp::List ConjugateMVNormalDP::updateClusterParameters() {
  clusterParameterUpdate();

  // Update alpha if needed
  if (!alphaPriorParameters.isNULL()) {
    updateAlpha();
  }

  return clusterParameters;
}

} // namespace dp

// Export functions (must be outside namespace for Rcpp)
//' @title Conjugate MVNormal Cluster Component Update (C++)
//' @description Update cluster components for conjugate multivariate normal Dirichlet process
//' @param dpObj Dirichlet process object as list
//' @return Updated Dirichlet process object
//' @export
// [[Rcpp::export]]
Rcpp::List conjugate_mvnormal_cluster_component_update_cpp(const Rcpp::List& dpObj) {
  dp::ConjugateMVNormalDP dp;
  dp.initialize(dpObj);
  return dp.updateClusterComponents();
}

//' @title Conjugate MVNormal Cluster Parameter Update (C++)
//' @description Update cluster parameters for conjugate multivariate normal Dirichlet process
//' @param dpObj Dirichlet process object as list
//' @return Updated cluster parameters
//' @export
// [[Rcpp::export]]
Rcpp::List conjugate_mvnormal_cluster_parameter_update_cpp(const Rcpp::List& dpObj) {
  dp::ConjugateMVNormalDP dp;
  dp.initialize(dpObj);
  return dp.updateClusterParameters();
}
