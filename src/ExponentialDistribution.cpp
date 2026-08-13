// src/ExponentialDistribution.cpp
#include "ExponentialDistribution.h"
#include "RcppConversions.h"
#include <cmath>

namespace dp {

// ExponentialMixingDistribution implementation
ExponentialMixingDistribution::ExponentialMixingDistribution(const Rcpp::NumericVector& priorParams) {
  distribution = "exponential";
  conjugate = true;
  priorParameters = priorParams;
}

ExponentialMixingDistribution::~ExponentialMixingDistribution() {
  // Destructor
}

Rcpp::NumericVector ExponentialMixingDistribution::likelihood(const arma::vec& x, const Rcpp::List& theta) const {
  // Direct extraction - avoid copies
  const Rcpp::NumericVector& lambda_array = theta[0];  // Use reference!
  const double lambda = lambda_array[0];

  const int n_data = x.n_elem;
  Rcpp::NumericVector result(n_data);

  if (lambda <= 0) {
    result.fill(1e-300);
    return result;
  }

  // Direct pointer access for speed
  double* result_ptr = &result[0];
  const double* x_ptr = x.memptr();

  // Vectorized calculation
  for (int i = 0; i < n_data; ++i) {
    result_ptr[i] = (x_ptr[i] >= 0) ?
    (lambda * std::exp(-lambda * x_ptr[i])) : 0.0;
  }

  return result;
}

Rcpp::List ExponentialMixingDistribution::priorDraw(int n) const {
  Rcpp::NumericVector priorParams = Rcpp::as<Rcpp::NumericVector>(priorParameters);

  // Prior parameters: alpha0 (shape), beta0 (rate)
  double alpha0 = priorParams[0];
  double beta0 = priorParams[1];

  Rcpp::NumericVector lambda(n);

  // Draw from Gamma(alpha0, beta0)
  for (int i = 0; i < n; i++) {
    lambda[i] = R::rgamma(alpha0, 1.0/beta0); // Note: R::rgamma uses scale = 1/rate
  }

  // Convert to 3D array with dimension (1,1,n)
  Rcpp::NumericVector lambda_arr(n);
  lambda_arr.attr("dim") = Rcpp::IntegerVector::create(1, 1, n);

  for (int i = 0; i < n; i++) {
    lambda_arr[i] = lambda[i];
  }

  return Rcpp::List::create(Rcpp::Named("lambda") = lambda_arr);
}

Rcpp::NumericMatrix ExponentialMixingDistribution::posteriorParameters(const arma::mat& x) const {
  Rcpp::NumericVector priorParams = Rcpp::as<Rcpp::NumericVector>(priorParameters);

  int n_x = x.n_rows;
  double sum_x = arma::sum(arma::vectorise(x));

  double alpha0 = priorParams[0];
  double beta0 = priorParams[1];

  // Posterior parameters for Gamma distribution
  double alpha_n = alpha0 + n_x;
  double beta_n = beta0 + sum_x;

  Rcpp::NumericMatrix result(1, 2);
  result(0, 0) = alpha_n;
  result(0, 1) = beta_n;

  return result;
}

Rcpp::List ExponentialMixingDistribution::posteriorDraw(const arma::mat& x, int n) const {
  // First compute posterior parameters
  Rcpp::NumericMatrix postParams = posteriorParameters(x);

  double alpha_n = postParams(0, 0);
  double beta_n = postParams(0, 1);

  Rcpp::NumericVector lambda(n);

  // Draw from posterior Gamma(alpha_n, beta_n)
  for (int i = 0; i < n; i++) {
    lambda[i] = R::rgamma(alpha_n, 1.0/beta_n);
  }

  // Convert to 3D array with dimension (1,1,n)
  Rcpp::NumericVector lambda_arr(n);
  lambda_arr.attr("dim") = Rcpp::IntegerVector::create(1, 1, n);

  for (int i = 0; i < n; i++) {
    lambda_arr[i] = lambda[i];
  }

  return Rcpp::List::create(Rcpp::Named("lambda") = lambda_arr);
}

Rcpp::NumericVector ExponentialMixingDistribution::predictive(const arma::vec& x) const {
  Rcpp::NumericVector priorParams = Rcpp::as<Rcpp::NumericVector>(priorParameters);

  int n = x.n_elem;
  Rcpp::NumericVector result(n);

  double alpha0 = priorParams[0];
  double beta0 = priorParams[1];

  for (int i = 0; i < n; i++) {
    // For single observation
    double alpha_post = alpha0 + 1;
    double beta_post = beta0 + x[i];

    // Predictive distribution calculation
    result[i] = (R::gammafn(alpha_post) / R::gammafn(alpha0)) *
      std::pow(beta0, alpha0) / std::pow(beta_post, alpha_post);
  }

  return result;
}

// ConjugateExponentialDP implementation
ConjugateExponentialDP::ConjugateExponentialDP(Rcpp::List dpObj) {
  // Initialize from the R list object
  data = Rcpp::as<arma::mat>(dpObj["data"]);
  n = data.n_rows;
  alpha = dpObj["alpha"];
  clusterLabels = Rcpp::as<arma::uvec>(dpObj["clusterLabels"]);
  pointsPerCluster = Rcpp::as<arma::uvec>(dpObj["pointsPerCluster"]);
  numberClusters = dpObj["numberClusters"];
  clusterParameters = dpObj["clusterParameters"];
  predictiveArray = Rcpp::as<arma::vec>(dpObj["predictiveArray"]);

  Rcpp::List mixingDistributionList = dpObj["mixingDistribution"];
  Rcpp::NumericVector priorParams = mixingDistributionList["priorParameters"];
  mixingDistribution = new ExponentialMixingDistribution(priorParams);
}


ConjugateExponentialDP::~ConjugateExponentialDP() {
  if (mixingDistribution) {
    delete mixingDistribution;
  }
}

void ConjugateExponentialDP::initialisePredictive() {
  // Calculate predictive probabilities for all data points
  predictiveArray = mixingDistribution->predictive(arma::vectorise(data));
}

void ConjugateExponentialDP::clusterComponentUpdate() {
  const int n = data.n_rows;

  // Pre-allocate probability vector - CRITICAL
  arma::vec probs(numberClusters + 1);

  // Cache cluster parameters for fast access - CRITICAL
  const Rcpp::NumericVector& lambda_vec = clusterParameters[0];
  std::vector<double> cluster_lambdas(numberClusters);
  for (int j = 0; j < numberClusters; ++j) {
    cluster_lambdas[j] = lambda_vec[j];
  }

  // Cache data as vector for faster access
  const arma::vec data_vec = arma::vectorise(data);
  const double* data_ptr = data_vec.memptr();
  const double* pred_ptr = predictiveArray.memptr();

  for (int i = 0; i < n; ++i) {
    const int currentLabel = clusterLabels[i];
    const double x_i = data_ptr[i];

    // Remove point from current cluster
    pointsPerCluster[currentLabel]--;

    // Calculate probabilities for existing clusters
    double* probs_ptr = probs.memptr();

    for (int j = 0; j < numberClusters; ++j) {
      if (pointsPerCluster[j] > 0) {
        // Direct exponential likelihood calculation - NO FUNCTION CALLS!
        const double lambda = cluster_lambdas[j];
        const double likelihood = (x_i >= 0) ?
        (lambda * std::exp(-lambda * x_i)) : 0.0;
        probs_ptr[j] = pointsPerCluster[j] * likelihood;
      } else {
        probs_ptr[j] = 0.0;
      }
    }

    // Probability for new cluster
    probs_ptr[numberClusters] = alpha * pred_ptr[i];

    // Normalize probabilities efficiently
    const double probSum = arma::sum(probs);
    if (probSum > 0) {
      probs /= probSum;
    } else {
      probs.fill(1.0 / (numberClusters + 1));
    }

    // Sample new label using cumulative sum
    const double u = R::runif(0, 1);
    double cumProb = 0.0;
    int newLabel = numberClusters;

    for (int j = 0; j <= numberClusters; ++j) {
      cumProb += probs_ptr[j];
      if (u <= cumProb) {
        newLabel = j;
        break;
      }
    }

    // Restore point count before the change
    pointsPerCluster[currentLabel]++;

    // Update cluster assignment using optimized clusterLabelChange
    clusterLabelChangeOptimized(i, newLabel, currentLabel);
  }
}

void ConjugateExponentialDP::clusterLabelChangeOptimized(int i, int newLabel, int currentLabel) {
  if (newLabel == currentLabel) {
    return;
  }

  const arma::rowvec x_i = data.row(i);

  // Remove point from old cluster
  pointsPerCluster[currentLabel]--;

  // Handle cluster assignment
  if (newLabel == numberClusters) {
    // New cluster case
    if (pointsPerCluster[currentLabel] == 0) {
      // Reuse empty slot - AVOID MEMORY REALLOCATION
      clusterLabels[i] = currentLabel;
      pointsPerCluster[currentLabel] = 1;

      // Update parameters directly
      Rcpp::NumericVector lambda_vec = Rcpp::clone(Rcpp::as<Rcpp::NumericVector>(clusterParameters[0]));
      const Rcpp::NumericVector& priorParams = Rcpp::as<Rcpp::NumericVector>(
        mixingDistribution->priorParameters);

      const double alpha_n = priorParams[0] + 1;
      const double beta_n = priorParams[1] + x_i[0];
      lambda_vec[currentLabel] = R::rgamma(alpha_n, 1.0/beta_n);

      clusterParameters[0] = lambda_vec;
    } else {
      // Create new cluster
      clusterLabels[i] = numberClusters;
      pointsPerCluster.resize(numberClusters + 1);
      pointsPerCluster[numberClusters] = 1;

      // Expand parameters
      Rcpp::NumericVector lambda_vec = Rcpp::clone(Rcpp::as<Rcpp::NumericVector>(clusterParameters[0]));
      const Rcpp::NumericVector& priorParams = Rcpp::as<Rcpp::NumericVector>(
        mixingDistribution->priorParameters);

      const double alpha_n = priorParams[0] + 1;
      const double beta_n = priorParams[1] + x_i[0];
      lambda_vec.push_back(R::rgamma(alpha_n, 1.0/beta_n));

      clusterParameters[0] = lambda_vec;
      numberClusters++;
    }
  } else {
    // Existing cluster
    clusterLabels[i] = newLabel;
    pointsPerCluster[newLabel]++;

    // Handle empty cluster removal
    if (pointsPerCluster[currentLabel] == 0) {
      // Remove empty cluster efficiently
      pointsPerCluster.shed_row(currentLabel);

      Rcpp::NumericVector lambda_vec = Rcpp::clone(Rcpp::as<Rcpp::NumericVector>(clusterParameters[0]));
      lambda_vec.erase(currentLabel);
      clusterParameters[0] = lambda_vec;

      numberClusters--;

      // Adjust labels
      for (arma::uword j = 0; j < clusterLabels.n_elem; ++j) {
        if (clusterLabels[j] > (unsigned int)currentLabel) {
          clusterLabels[j]--;
        }
      }
    }
  }
}

void ConjugateExponentialDP::clusterParameterUpdate() {
  // Update parameters for each cluster
  for (int k = 0; k < numberClusters; k++) {
    // Get data points assigned to this cluster
    arma::uvec clusterIndices = arma::find(clusterLabels == k);

    if (clusterIndices.n_elem > 0) {
      arma::mat clusterData = data.rows(clusterIndices);

      // Draw from posterior
      Rcpp::List postDraw = mixingDistribution->posteriorDraw(clusterData, 1);

      // Update cluster parameters
      Rcpp::NumericVector lambda_vec = Rcpp::as<Rcpp::NumericVector>(clusterParameters[0]);
      Rcpp::NumericVector new_lambda = postDraw["lambda"];

      lambda_vec[k] = new_lambda[0];
      clusterParameters[0] = lambda_vec;
    }
  }
}

void ConjugateExponentialDP::updateAlpha() {
  // Implementation of alpha update using auxiliary variable method
  double x = R::rbeta(alpha + 1.0, n);

  // Cast alphaPriorParameters to NumericVector
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

Rcpp::List ConjugateExponentialDP::clusterLabelChange(int i, int newLabel, int currentLabel) {
  if (newLabel == currentLabel) {
    return Rcpp::List::create(
      Rcpp::Named("clusterLabels") = clusterLabels,
      Rcpp::Named("pointsPerCluster") = pointsPerCluster,
      Rcpp::Named("clusterParameters") = clusterParameters,
      Rcpp::Named("numberClusters") = numberClusters
    );
  }

  arma::mat x_i = data.row(i);

  // 1. Remove point from old cluster
  pointsPerCluster[currentLabel]--;

  // 2. Assign point to new cluster
  clusterLabels[i] = newLabel;
  if (newLabel == numberClusters) { // This is a new cluster
    numberClusters++;
    pointsPerCluster.resize(numberClusters);
    pointsPerCluster(newLabel) = 1;

    // Safely create copy of parameter vector, modify, and assign back
    Rcpp::NumericVector lambda_vec = Rcpp::clone(Rcpp::as<Rcpp::NumericVector>(clusterParameters[0]));

    Rcpp::List postDraw = mixingDistribution->posteriorDraw(x_i, 1);
    lambda_vec.push_back(Rcpp::as<Rcpp::NumericVector>(postDraw["lambda"])[0]);

    clusterParameters[0] = lambda_vec;

  } else { // This is an existing cluster
    pointsPerCluster[newLabel]++;
  }

  // 3. If the old cluster is now empty, remove it
  if (pointsPerCluster[currentLabel] == 0) {
    pointsPerCluster.shed_row(currentLabel);

    // Safely create copy, modify, and assign back
    Rcpp::NumericVector lambda_vec = Rcpp::clone(Rcpp::as<Rcpp::NumericVector>(clusterParameters[0]));
    lambda_vec.erase(currentLabel);
    clusterParameters[0] = lambda_vec;

    numberClusters--;

    // Shift all labels that were greater than the removed cluster's label
    for (arma::uword j = 0; j < clusterLabels.n_elem; j++) {
      if (clusterLabels[j] > (unsigned int)currentLabel) {
        clusterLabels[j]--;
      }
    }
  }

  return Rcpp::List::create(
    Rcpp::Named("clusterLabels") = clusterLabels,
    Rcpp::Named("pointsPerCluster") = pointsPerCluster,
    Rcpp::Named("clusterParameters") = clusterParameters,
    Rcpp::Named("numberClusters") = numberClusters
  );
}

} // namespace dp
