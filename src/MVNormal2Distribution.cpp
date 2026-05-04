// src/MVNormal2Distribution.cpp
#include "MVNormal2Distribution.h"
#include "RcppConversions.h"
#include <RcppArmadillo.h>
#include <cmath>

namespace dp {

// MVNormal2MixingDistribution implementation
MVNormal2MixingDistribution::MVNormal2MixingDistribution(const Rcpp::List& priorParams) {
  distribution = "mvnormal2";
  conjugate = false;
  priorParameters = priorParams;

  // Extract prior parameters
  if (priorParams.containsElementNamed("mu0")) {
    SEXP mu0_sexp = priorParams["mu0"];
    if (Rf_isMatrix(mu0_sexp)) {
      arma::mat temp_mu0_mat = Rcpp::as<arma::mat>(mu0_sexp); // Convert to arma::mat
      if (temp_mu0_mat.n_rows == 1) { // If R matrix is 1xN (already a row vector shape)
        mu0 = temp_mu0_mat; // Assign directly (arma::mat to arma::rowvec if mat is 1xN)
      } else if (temp_mu0_mat.n_cols == 1) { // If R matrix is Nx1 (a column vector shape)
        mu0 = temp_mu0_mat.t(); // Transpose to 1xN and assign
      } else {
        Rcpp::stop("mu0 in priorParams, if a matrix, must be a row or column vector.");
      }
    } else { // It's an R vector (NumericVector)
      // Rcpp::as<arma::vec> converts an R vector to an Armadillo column vector
      arma::vec temp_mu0_col_vec = Rcpp::as<arma::vec>(mu0_sexp);
      mu0 = temp_mu0_col_vec.t(); // Transpose the column vector to a row vector for mu0
    }
  }

  if (priorParams.containsElementNamed("sigma0")) {
    sigma0 = Rcpp::as<arma::mat>(priorParams["sigma0"]);
  }

  if (priorParams.containsElementNamed("phi0")) {
    phi0 = Rcpp::as<arma::mat>(priorParams["phi0"]);
  }

  if (priorParams.containsElementNamed("nu0")) {
    nu0 = Rcpp::as<double>(priorParams["nu0"]);
  }

  // Set default MH step size if not provided
  if (!priorParameters.containsElementNamed("mhStepSize")) {
    mhStepSize = Rcpp::NumericVector::create(1.0, 1.0);
  }
}

MVNormal2MixingDistribution::~MVNormal2MixingDistribution() {
  // Destructor
}

Rcpp::NumericVector MVNormal2MixingDistribution::likelihood(const arma::vec& x, const Rcpp::List& theta) const {
  // Validate input
  if (theta.size() < 2) {
    Rcpp::stop("theta list must have at least 2 elements (mu and sig)");
  }
  
  if (x.n_elem == 0) {
    Rcpp::stop("x vector cannot be empty");
  }
  
  // Extract parameters from theta
  Rcpp::NumericVector mu_array = theta[0];
  Rcpp::NumericVector sig_array = theta[1];
  
  // Check if arrays are empty
  if (mu_array.size() == 0) {
    Rcpp::stop("mu_array cannot be empty");
  }
  
  if (sig_array.size() == 0) {
    Rcpp::stop("sig_array cannot be empty");
  }

  // Get dimensions
  Rcpp::IntegerVector mu_dim = mu_array.attr("dim");
  Rcpp::IntegerVector sig_dim = sig_array.attr("dim");
  
  // Validate dimension vectors
  if (mu_dim.size() < 3) {
    Rcpp::stop("mu_array must be a 3D array (dim length must be >= 3)");
  }
  
  if (sig_dim.size() < 3) {
    Rcpp::stop("sig_array must be a 3D array (dim length must be >= 3)");
  }

  int d = mu_dim[1];  // Number of dimensions
  int n_clusters = mu_dim[2];  // Number of clusters
  
  // Validate dimensions
  if (d <= 0) {
    Rcpp::stop("Number of dimensions must be positive");
  }
  
  if (n_clusters <= 0) {
    Rcpp::stop("Number of clusters must be positive");
  }
  
  if (x.n_elem != static_cast<arma::uword>(d)) {
    Rcpp::stop("x vector length must match number of dimensions");
  }

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

    // Extract sigma for cluster k
    arma::mat sig_k(d, d);
    for (int i = 0; i < d; i++) {
      for (int j = 0; j < d; j++) {
        sig_k(i, j) = sig_array[i + j * d + k * d * d];
      }
    }

    // Calculate multivariate normal likelihood
    double log_det_val;
    double sign;
    arma::log_det(log_det_val, sign, sig_k);

    if (sign <= 0) {
      result[k] = 1e-300;
      continue;
    }

    arma::mat sig_inv;
    try {
      sig_inv = arma::inv_sympd(sig_k);
    } catch(...) {
      result[k] = 1e-300;
      continue;
    }

    double log_const = -0.5 * d * std::log(2.0 * M_PI) - 0.5 * log_det_val;
    arma::vec x_centered = x - mu_k;
    double quad_form = arma::as_scalar(x_centered.t() * sig_inv * x_centered);
    result[k] = std::exp(log_const - 0.5 * quad_form);
  }

  return result;
}

Rcpp::List MVNormal2MixingDistribution::priorDraw(int n) const {
  // Validate input parameters
  if (n <= 0) {
    Rcpp::stop("Number of draws must be positive");
  }
  
  int d = mu0.n_elem;
  if (d <= 0) {
    Rcpp::stop("Dimension must be positive");
  }

  Rcpp::NumericVector mu_arr = Rcpp::NumericVector(Rcpp::Dimension(1, d, n));
  Rcpp::NumericVector sig_arr = Rcpp::NumericVector(Rcpp::Dimension(d, d, n));

  // Check if sigma0 is well-conditioned
  arma::mat sigma0_reg = sigma0;
  arma::vec eigvals = arma::eig_sym(sigma0);
  double min_eigenval = eigvals.min();
  double max_eigenval = eigvals.max();

  // If nearly singular, add regularization
  if (min_eigenval < 1e-10 || max_eigenval / min_eigenval > 1e10) {
    double regularization = std::max(1e-8, max_eigenval * 1e-8);
    sigma0_reg = sigma0 + arma::eye(d, d) * regularization;
  }

  for (int i = 0; i < n; i++) {
    // Draw Sigma from Inverse-Wishart with regularization if needed
    arma::mat sig_draw;
    try {
      sig_draw = arma::iwishrnd(phi0, nu0);
    } catch(...) {
      // If phi0 is problematic, regularize it
      arma::mat phi0_reg = phi0 + arma::eye(d, d) * 1e-8;
      sig_draw = arma::iwishrnd(phi0_reg, nu0);
    }

    // Ensure sig_draw is well-conditioned
    eigvals = arma::eig_sym(sig_draw);
    min_eigenval = eigvals.min();
    if (min_eigenval < 1e-10) {
      sig_draw += arma::eye(d, d) * (1e-8 - min_eigenval);
    }

    // Draw mu from Multivariate Normal given Sigma
    arma::vec mu_draw = arma::mvnrnd(mu0.t(), sigma0_reg);

    // Store in arrays (mu_arr has dimensions 1 x d x n)
    for (int j = 0; j < d; j++) {
      mu_arr[0 + j * 1 + i * 1 * d] = mu_draw(j);
    }

    for (int j = 0; j < d; j++) {
      for (int k = 0; k < d; k++) {
        sig_arr[j + k * d + i * d * d] = sig_draw(j, k);
      }
    }
  }

  return Rcpp::List::create(
    Rcpp::Named("mu") = mu_arr,
    Rcpp::Named("sig") = sig_arr
  );
}

Rcpp::List MVNormal2MixingDistribution::posteriorDraw(const arma::mat& x, int n) const {
  // Validate input parameters
  if (n <= 0) {
    Rcpp::stop("Number of draws must be positive");
  }
  if (!x.is_finite()) {
    Rcpp::stop("Input data contains non-finite values");
  }

  int d = x.n_cols;
  if (d == 0 || x.n_rows == 0) {
    // Return prior draw if no data
    return priorDraw(n);
  }

  // Arrays to store results
  Rcpp::NumericVector mu_arr = Rcpp::NumericVector(Rcpp::Dimension(1, d, n));
  Rcpp::NumericVector sig_arr = Rcpp::NumericVector(Rcpp::Dimension(d, d, n));

  // Standardize extreme data to improve numerical stability
  arma::mat x_scaled = x;
  arma::vec scale_factors = arma::ones(d);

  for (int j = 0; j < d; j++) {
    double col_max = arma::abs(x.col(j)).max();
    if (col_max > 1e6) {
      scale_factors(j) = col_max / 1e3;
      x_scaled.col(j) = x.col(j) / scale_factors(j);
    } else if (col_max < 1e-6 && col_max > 0) {
      scale_factors(j) = col_max * 1e3;
      x_scaled.col(j) = x.col(j) / scale_factors(j);
    }
  }

  // Initialize with a reasonable starting value
  arma::vec mu_samp = arma::mean(x_scaled, 0).t();

  for (int i = 0; i < n; i++) {
    // Update Sigma given current mu
    double nu_n = x_scaled.n_rows + nu0;
    arma::mat phi_n = phi0;

    // Compute scatter matrix with numerical stability
    arma::mat scatter = arma::zeros(d, d);
    for (arma::uword j = 0; j < x_scaled.n_rows; j++) {
      arma::vec diff = x_scaled.row(j).t() - mu_samp;

      // Check for extreme differences
      double max_diff = arma::abs(diff).max();
      if (max_diff > 1e8) {
        diff = diff / (max_diff / 1e4);
      }

      scatter += diff * diff.t();
    }

    // Add scatter to phi_n with regularization
    phi_n += scatter;

    // More aggressive regularization for ill-conditioned matrices
    double trace_phi = arma::trace(phi_n);
    double regularization = std::max(1e-8, trace_phi * 1e-10);
    phi_n += arma::eye(d, d) * regularization;

    // Ensure phi_n is well-conditioned
    arma::vec eigvals = arma::eig_sym(phi_n);
    double min_eigenval = eigvals.min();
    double max_eigenval = eigvals.max();

    // Check condition number
    if (max_eigenval / min_eigenval > 1e10 || min_eigenval < 1e-10) {
      double target_min = std::max(1e-6, max_eigenval * 1e-8);
      phi_n += arma::eye(d, d) * (target_min - min_eigenval);
    }

    // Draw new Sigma using more stable inversion
    arma::mat sig_samp;
    try {
      // Try standard inverse Wishart
      arma::mat phi_n_inv = arma::inv_sympd(phi_n);
      sig_samp = arma::iwishrnd(phi_n_inv, nu_n);
    } catch(...) {
      // If that fails, use SVD-based approach
      arma::mat U;
      arma::vec s;
      arma::mat V;
      arma::svd(U, s, V, phi_n);

      // Regularize small singular values
      for (arma::uword j = 0; j < s.n_elem; j++) {
        if (s(j) < 1e-10) s(j) = 1e-10;
      }

      arma::mat phi_n_inv = V * arma::diagmat(1.0 / s) * U.t();
      sig_samp = arma::iwishrnd(phi_n_inv, nu_n);
    }

    // Ensure sig_samp is well-conditioned
    eigvals = arma::eig_sym(sig_samp);
    min_eigenval = eigvals.min();
    if (min_eigenval < 1e-10) {
      sig_samp += arma::eye(d, d) * (1e-8 - min_eigenval);
    }

    // Update mu given new Sigma
    arma::mat sig_n;
    try {
      arma::mat sig_samp_inv = arma::inv_sympd(sig_samp);
      arma::mat sigma0_inv = arma::inv_sympd(sigma0);

      sig_n = arma::inv_sympd(sigma0_inv + x_scaled.n_rows * sig_samp_inv);
      arma::vec mu_n = sig_n * (x_scaled.n_rows * sig_samp_inv * arma::mean(x_scaled, 0).t() +
        sigma0_inv * mu0.t());

      // Draw new mu
      mu_samp = arma::mvnrnd(mu_n, sig_n);
    } catch(...) {
      // If matrix operations fail, use regularized versions
      arma::mat sig_samp_reg = sig_samp + arma::eye(d, d) * 1e-6;
      arma::mat sigma0_reg = sigma0 + arma::eye(d, d) * 1e-6;

      arma::mat sig_samp_inv = arma::inv(sig_samp_reg);
      arma::mat sigma0_inv = arma::inv(sigma0_reg);

      sig_n = arma::inv(sigma0_inv + x_scaled.n_rows * sig_samp_inv);
      arma::vec mu_n = sig_n * (x_scaled.n_rows * sig_samp_inv * arma::mean(x_scaled, 0).t() +
        sigma0_inv * mu0.t());

      mu_samp = arma::mvnrnd(mu_n, sig_n);
    }

    // Scale mu back to original scale
    arma::vec mu_original = mu_samp;
    for (int j = 0; j < d; j++) {
      mu_original(j) *= scale_factors(j);
    }

    // Scale sig back to original scale
    arma::mat sig_original = sig_samp;
    for (int j1 = 0; j1 < d; j1++) {
      for (int j2 = 0; j2 < d; j2++) {
        sig_original(j1, j2) *= scale_factors(j1) * scale_factors(j2);
      }
    }

    // Store results (mu_arr has dimensions 1 x d x n)
    for (int j = 0; j < d; j++) {
      mu_arr[0 + j * 1 + i * 1 * d] = mu_original(j);
    }

    for (int j = 0; j < d; j++) {
      for (int k = 0; k < d; k++) {
        sig_arr[j + k * d + i * d * d] = sig_original(j, k);
      }
    }
  }

  return Rcpp::List::create(
    Rcpp::Named("mu") = mu_arr,
    Rcpp::Named("sig") = sig_arr
  );
}


Rcpp::List MVNormal2MixingDistribution::toR() const {
  return Rcpp::List::create(
    Rcpp::Named("distribution") = distribution,
    Rcpp::Named("priorParameters") = priorParameters,
    Rcpp::Named("conjugate") = conjugate
  );
}

// NonConjugateMVNormal2DP implementation
NonConjugateMVNormal2DP::NonConjugateMVNormal2DP() : mixingDistribution(nullptr), numberClusters(0), m(3) {
  // Constructor
}

NonConjugateMVNormal2DP::~NonConjugateMVNormal2DP() {
  if (mixingDistribution) {
    delete mixingDistribution;
  }
}

void NonConjugateMVNormal2DP::clusterComponentUpdate() {
  int n = data.n_rows;

  for (int i = 0; i < n; i++) {
    int currentLabel = clusterLabels[i];

    // Temporarily remove point from current cluster
    pointsPerCluster[currentLabel]--;

    // Generate auxiliary parameters
    Rcpp::List aux;
    bool currentClusterEmpty = (pointsPerCluster[currentLabel] == 0);

    if (currentClusterEmpty && currentLabel < numberClusters) {
      // If cluster is now empty, include its parameters as auxiliary
      aux = mixingDistribution->priorDraw(m - 1);

      Rcpp::NumericVector mu_vec = Rcpp::as<Rcpp::NumericVector>(clusterParameters[0]);
      Rcpp::NumericVector sig_vec = Rcpp::as<Rcpp::NumericVector>(clusterParameters[1]);
      Rcpp::NumericVector mu_aux = aux[0];
      Rcpp::NumericVector sig_aux = aux[1];

      Rcpp::IntegerVector mu_dim = mu_vec.attr("dim");
      int d = mu_dim[1];

      // Create combined arrays
      Rcpp::NumericVector mu_combined = Rcpp::NumericVector(Rcpp::Dimension(1, d, m));
      Rcpp::NumericVector sig_combined = Rcpp::NumericVector(Rcpp::Dimension(d, d, m));

      // First slot: current (empty) cluster's parameters
      for (int j = 0; j < d; j++) {
        mu_combined[j] = mu_vec[j + currentLabel * d];
      }
      for (int j = 0; j < d; j++) {
        for (int k = 0; k < d; k++) {
          sig_combined[j + k * d] = sig_vec[j + k * d + currentLabel * d * d];
        }
      }

      // Remaining slots: auxiliary parameters
      for (int idx = 1; idx < m; idx++) {
        for (int j = 0; j < d; j++) {
          mu_combined[j + idx * d] = mu_aux[j + (idx-1) * d];
        }
        for (int j = 0; j < d; j++) {
          for (int k = 0; k < d; k++) {
            sig_combined[j + k * d + idx * d * d] = sig_aux[j + k * d + (idx-1) * d * d];
          }
        }
      }

      aux = Rcpp::List::create(mu_combined, sig_combined);
    } else {
      // Generate m new auxiliary parameters
      aux = mixingDistribution->priorDraw(m);
    }

    // Calculate probabilities for all possible assignments
    int totalOptions = numberClusters + m;
    Rcpp::NumericVector probs(totalOptions);

    // Get data point
    arma::vec x_i = data.row(i).t();

    // Calculate probabilities for existing clusters
    Rcpp::NumericVector mu_params = clusterParameters[0];
    Rcpp::NumericVector sig_params = clusterParameters[1];
    Rcpp::IntegerVector mu_dim = mu_params.attr("dim");
    int d = mu_dim[1];

    for (int j = 0; j < numberClusters; j++) {
      if (pointsPerCluster[j] > 0 || (j == currentLabel && currentClusterEmpty)) {
        // Extract parameters for cluster j
        Rcpp::NumericVector mu_j = Rcpp::NumericVector(Rcpp::Dimension(1, d, 1));
        Rcpp::NumericVector sig_j = Rcpp::NumericVector(Rcpp::Dimension(d, d, 1));

        for (int k = 0; k < d; k++) {
          mu_j[k] = mu_params[k + j * d];
        }
        for (int k1 = 0; k1 < d; k1++) {
          for (int k2 = 0; k2 < d; k2++) {
            sig_j[k1 + k2 * d] = sig_params[k1 + k2 * d + j * d * d];
          }
        }

        Rcpp::List theta_j = Rcpp::List::create(
          Rcpp::Named("0") = mu_j,
          Rcpp::Named("1") = sig_j
        );

        Rcpp::NumericVector lik = mixingDistribution->likelihood(x_i, theta_j);

        // Weight by number of points (but if empty cluster, use special handling)
        if (j == currentLabel && currentClusterEmpty) {
          probs[j] = (alpha / m) * lik[0];  // Treat as auxiliary
        } else {
          probs[j] = pointsPerCluster[j] * lik[0];
        }
      } else {
        probs[j] = 0.0;
      }
    }

    // Calculate probabilities for auxiliary clusters
    Rcpp::NumericVector aux_mu = aux[0];
    Rcpp::NumericVector aux_sig = aux[1];

    for (int j = 0; j < m; j++) {
      // Extract auxiliary parameter j
      Rcpp::NumericVector mu_j = Rcpp::NumericVector(Rcpp::Dimension(1, d, 1));
      Rcpp::NumericVector sig_j = Rcpp::NumericVector(Rcpp::Dimension(d, d, 1));

      for (int k = 0; k < d; k++) {
        mu_j[k] = aux_mu[k + j * d];
      }
      for (int k1 = 0; k1 < d; k1++) {
        for (int k2 = 0; k2 < d; k2++) {
          sig_j[k1 + k2 * d] = aux_sig[k1 + k2 * d + j * d * d];
        }
      }

      Rcpp::List theta_j = Rcpp::List::create(
        Rcpp::Named("0") = mu_j,
        Rcpp::Named("1") = sig_j
      );

      Rcpp::NumericVector lik = mixingDistribution->likelihood(x_i, theta_j);
      probs[numberClusters + j] = (alpha / m) * lik[0];
    }

    // Handle numerical issues
    for (int j = 0; j < probs.size(); j++) {
      if (!std::isfinite(probs[j])) probs[j] = 0.0;
    }

    if (Rcpp::sum(probs) == 0.0) {
      probs.fill(1.0 / probs.size());
    }

    // Sample new label
    double cumProb = 0.0;
    double u = R::runif(0, 1);
    int newLabel = 0;
    double probSum = Rcpp::sum(probs);

    for (int j = 0; j < probs.size(); j++) {
      cumProb += probs[j] / probSum;
      if (u <= cumProb) {
        newLabel = j;
        break;
      }
    }

    // Now perform the actual update (pointsPerCluster[currentLabel] is already decremented)
    Rcpp::List updateResult = clusterLabelChange(i, newLabel, currentLabel, aux);

    // Extract updated values
    clusterLabels = Rcpp::as<arma::uvec>(updateResult["clusterLabels"]);
    pointsPerCluster = Rcpp::as<arma::uvec>(updateResult["pointsPerCluster"]);
    clusterParameters = updateResult["clusterParameters"];
    numberClusters = updateResult["numberClusters"];
  }
}

void NonConjugateMVNormal2DP::clusterParameterUpdate() {
  for (int k = 0; k < numberClusters; k++) {
    arma::uvec clusterIndices = arma::find(clusterLabels == k);
    if (clusterIndices.n_elem > 0) {
      arma::mat clusterData = data.rows(clusterIndices);

      // Draw from posterior
      Rcpp::List postDraw = mixingDistribution->posteriorDraw(clusterData, mhDraws);

      // Update cluster parameters - extract last sample
      Rcpp::NumericVector mu_samples = postDraw[0];
      Rcpp::NumericVector sig_samples = postDraw[1];

      // Get dimensions
      Rcpp::IntegerVector mu_dim = mu_samples.attr("dim");
      int d = mu_dim[1];

      // Extract current parameter arrays
      Rcpp::NumericVector mu_params = Rcpp::as<Rcpp::NumericVector>(clusterParameters[0]);
      Rcpp::NumericVector sig_params = Rcpp::as<Rcpp::NumericVector>(clusterParameters[1]);

      // Update with last sample
      int last_idx = mhDraws - 1;
      for (int j = 0; j < d; j++) {
        mu_params[j + k * d] = mu_samples[j + last_idx * d];
      }
      for (int i = 0; i < d; i++) {
        for (int j = 0; j < d; j++) {
          sig_params[i + j * d + k * d * d] = sig_samples[i + j * d + last_idx * d * d];
        }
      }

      clusterParameters[0] = mu_params;
      clusterParameters[1] = sig_params;
    }
  }
}

void NonConjugateMVNormal2DP::updateAlpha() {
  // Same implementation as other non-conjugate cases
  double x = R::rbeta(alpha + 1.0, n);
  Rcpp::NumericVector currentAlphaPrior = Rcpp::as<Rcpp::NumericVector>(alphaPriorParameters);

  double log_x = std::log(x);
  double pi1 = currentAlphaPrior[0] + numberClusters - 1.0;
  double pi2 = n * (currentAlphaPrior[1] - log_x);

  double pi_val = pi1 / (pi1 + pi2);
  if (!std::isfinite(pi_val)) {
    pi_val = 0.5;
  }

  double postShape;
  if (R::runif(0, 1) < pi_val) {
    postShape = currentAlphaPrior[0] + numberClusters;
  } else {
    postShape = currentAlphaPrior[0] + numberClusters - 1.0;
  }

  double postRate = currentAlphaPrior[1] - log_x;
  if (postRate <= 0) postRate = 1e-6;

  alpha = R::rgamma(postShape, 1.0 / postRate);
  if (alpha <= 0) alpha = 1e-6;
}

Rcpp::List NonConjugateMVNormal2DP::clusterLabelChange(int i, int newLabel, int currentLabel,
                                                       const Rcpp::List& aux) {
  if (newLabel == currentLabel) {
    // CRITICAL FIX: The caller has already decremented pointsPerCluster[currentLabel]
    // so we need to increment it back since the point is staying in the same cluster
    pointsPerCluster[currentLabel]++;

    return Rcpp::List::create(
      Rcpp::Named("clusterLabels") = clusterLabels,
      Rcpp::Named("pointsPerCluster") = pointsPerCluster,
      Rcpp::Named("clusterParameters") = clusterParameters,
      Rcpp::Named("numberClusters") = numberClusters
    );
  }

  // Extract current parameters
  Rcpp::NumericVector mu_vec = Rcpp::clone(Rcpp::as<Rcpp::NumericVector>(clusterParameters[0]));
  Rcpp::NumericVector sig_vec = Rcpp::clone(Rcpp::as<Rcpp::NumericVector>(clusterParameters[1]));

  // Get dimensions
  Rcpp::IntegerVector mu_dim = mu_vec.attr("dim");
  int d = mu_dim[1];

  // 1. Remove point from old cluster (already done by caller)

  // 2. Assign to new cluster
  if (newLabel < numberClusters) {
    // Existing cluster
    pointsPerCluster[newLabel]++;
    clusterLabels[i] = newLabel;

    // If old cluster is now empty, remove it
    if (pointsPerCluster[currentLabel] == 0) {
      numberClusters--;
      pointsPerCluster.shed_row(currentLabel);

      // Create new arrays with reduced size
      Rcpp::NumericVector new_mu = Rcpp::NumericVector(Rcpp::Dimension(1, d, numberClusters));
      Rcpp::NumericVector new_sig = Rcpp::NumericVector(Rcpp::Dimension(d, d, numberClusters));

      // Copy parameters, skipping the removed cluster
      int new_k = 0;
      for (int k = 0; k < numberClusters + 1; k++) {
        if (k != currentLabel) {
          for (int j = 0; j < d; j++) {
            new_mu[j + new_k * d] = mu_vec[j + k * d];
          }
          for (int i = 0; i < d; i++) {
            for (int j = 0; j < d; j++) {
              new_sig[i + j * d + new_k * d * d] = sig_vec[i + j * d + k * d * d];
            }
          }
          new_k++;
        }
      }

      clusterParameters[0] = new_mu;
      clusterParameters[1] = new_sig;

      // Update labels
      for (arma::uword j = 0; j < clusterLabels.n_elem; j++) {
        if (clusterLabels[j] > (unsigned int)currentLabel) {
          clusterLabels[j]--;
        }
      }
    }
  } else {
    // New cluster from auxiliary parameters
    int auxIndex = newLabel - numberClusters;

    if (pointsPerCluster[currentLabel] == 0) {
      // Replace empty cluster with auxiliary
      Rcpp::NumericVector aux_mu = aux[0];
      Rcpp::NumericVector aux_sig = aux[1];

      // Copy auxiliary parameters to current cluster position
      for (int j = 0; j < d; j++) {
        mu_vec[j + currentLabel * d] = aux_mu[j + auxIndex * d];
      }
      for (int i = 0; i < d; i++) {
        for (int j = 0; j < d; j++) {
          sig_vec[i + j * d + currentLabel * d * d] =
            aux_sig[i + j * d + auxIndex * d * d];
        }
      }

      pointsPerCluster[currentLabel] = 1;
      clusterLabels[i] = currentLabel;
    } else {
      // Create new cluster
      Rcpp::NumericVector aux_mu = aux[0];
      Rcpp::NumericVector aux_sig = aux[1];

      // Create expanded arrays
      Rcpp::NumericVector new_mu = Rcpp::NumericVector(Rcpp::Dimension(1, d, numberClusters + 1));
      Rcpp::NumericVector new_sig = Rcpp::NumericVector(Rcpp::Dimension(d, d, numberClusters + 1));

      // Copy existing parameters
      for (int k = 0; k < numberClusters; k++) {
        for (int j = 0; j < d; j++) {
          new_mu[j + k * d] = mu_vec[j + k * d];
        }
        for (int i = 0; i < d; i++) {
          for (int j = 0; j < d; j++) {
            new_sig[i + j * d + k * d * d] = sig_vec[i + j * d + k * d * d];
          }
        }
      }

      // Add new cluster parameters
      for (int j = 0; j < d; j++) {
        new_mu[j + numberClusters * d] = aux_mu[j + auxIndex * d];
      }
      for (int i = 0; i < d; i++) {
        for (int j = 0; j < d; j++) {
          new_sig[i + j * d + numberClusters * d * d] =
            aux_sig[i + j * d + auxIndex * d * d];
        }
      }

      clusterParameters[0] = new_mu;
      clusterParameters[1] = new_sig;

      clusterLabels[i] = numberClusters;
      pointsPerCluster.resize(numberClusters + 1);
      pointsPerCluster[numberClusters] = 1;
      numberClusters++;
    }
  }

  return Rcpp::List::create(
    Rcpp::Named("clusterLabels") = clusterLabels,
    Rcpp::Named("pointsPerCluster") = pointsPerCluster,
    Rcpp::Named("clusterParameters") = clusterParameters,
    Rcpp::Named("numberClusters") = numberClusters
  );
}

// Add this method implementation
MixingDistribution* NonConjugateMVNormal2DP::getMixingDistribution() {
  return mixingDistribution;
}

Rcpp::List NonConjugateMVNormal2DP::toR() const {
  return Rcpp::List::create(
    Rcpp::Named("data") = data,
    Rcpp::Named("n") = n,
    Rcpp::Named("alpha") = alpha,
    Rcpp::Named("alphaPriorParameters") = alphaPriorParameters,
    Rcpp::Named("clusterLabels") = clusterLabels,  // Already 0-indexed in C++
    Rcpp::Named("pointsPerCluster") = pointsPerCluster,
    Rcpp::Named("numberClusters") = numberClusters,
    Rcpp::Named("clusterParameters") = clusterParameters,
    Rcpp::Named("mixingDistribution") = mixingDistribution->toR(),
    Rcpp::Named("m") = m,
    Rcpp::Named("mhDraws") = mhDraws
  );
}

} // namespace dp
