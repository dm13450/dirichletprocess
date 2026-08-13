// src/WeibullDistribution.cpp
#include "WeibullDistribution.h"
#include "RcppConversions.h"
#include <cmath>
#include <limits>
#include <memory>

namespace dp {

// WeibullMixingDistribution implementation
WeibullMixingDistribution::WeibullMixingDistribution(const Rcpp::NumericVector& priorParams,
                                                     const Rcpp::NumericVector& mhStepSize,
                                                     const Rcpp::NumericVector& hyperPriorParams) {
  distribution = "weibull";
  conjugate = false;
  priorParameters = priorParams;
  this->mhStepSize = mhStepSize;

  if (hyperPriorParams.size() > 0) {
    hyperPriorParameters = hyperPriorParams;
  } else {
    hyperPriorParameters = Rcpp::NumericVector::create(6.0, 2.0, 1.0, 0.5);
  }
}

WeibullMixingDistribution::~WeibullMixingDistribution() {
  // Destructor
}

Rcpp::NumericVector WeibullMixingDistribution::likelihood(const arma::vec& x, const Rcpp::List& theta) const {
  Rcpp::NumericVector alpha_array = theta[0];
  Rcpp::NumericVector lambda_array = theta[1];

  int n_data = x.n_elem;
  Rcpp::NumericVector result(n_data);

  double alpha = alpha_array[0];
  double lambda = lambda_array[0];

  // Quick validation
  if (alpha <= 0 || lambda <= 0 || !std::isfinite(alpha) || !std::isfinite(lambda)) {
    std::fill(result.begin(), result.end(), 1e-300);
    return result;
  }

  // Pre-compute constants
  double log_const = std::log(alpha) - std::log(lambda);
  double inv_lambda = 1.0 / lambda;

  // Vectorized computation
  for (int i = 0; i < n_data; i++) {
    if (x[i] < 0) {
      result[i] = 0.0;
    } else if (x[i] == 0) {
      result[i] = (alpha == 1.0) ? inv_lambda : 0.0;
    } else {
      // Standard Weibull likelihood
      double log_x = std::log(x[i]);
      double x_alpha = std::exp(alpha * log_x);

      double log_lik = log_const + (alpha - 1.0) * log_x - inv_lambda * x_alpha;

      if (log_lik > -700) {  // Prevent underflow
        result[i] = std::exp(log_lik);
      } else {
        result[i] = 1e-300;
      }
    }
  }

  return result;
}

Rcpp::List WeibullMixingDistribution::priorDraw(int n) const {
  Rcpp::NumericVector priorParams = Rcpp::as<Rcpp::NumericVector>(priorParameters);

  Rcpp::NumericVector alpha_values(n);
  Rcpp::NumericVector lambda_values(n);

  // priorParams[0] = phi, priorParams[1] = alpha0, priorParams[2] = beta0
  for (int i = 0; i < n; i++) {
    alpha_values[i] = R::runif(0.0, priorParams[0]);
    // R's rgamma uses shape and scale, convert rate to scale: scale = 1/rate
    double gamma_draw = R::rgamma(priorParams[1], 1.0 / priorParams[2]);
    lambda_values[i] = 1.0 / gamma_draw;
  }

  // Convert to 3D arrays
  Rcpp::NumericVector alpha_arr(n);
  Rcpp::NumericVector lambda_arr(n);
  alpha_arr.attr("dim") = Rcpp::IntegerVector::create(1, 1, n);
  lambda_arr.attr("dim") = Rcpp::IntegerVector::create(1, 1, n);

  for (int i = 0; i < n; i++) {
    alpha_arr[i] = alpha_values[i];
    lambda_arr[i] = lambda_values[i];
  }

  return Rcpp::List::create(
    Rcpp::Named("alpha") = alpha_arr,
    Rcpp::Named("lambda") = lambda_arr
  );
}

Rcpp::NumericVector WeibullMixingDistribution::priorDensity(const Rcpp::List& theta) const {
  Rcpp::NumericVector priorParams = Rcpp::as<Rcpp::NumericVector>(priorParameters);
  Rcpp::NumericVector alpha_array = theta[0];

  double alpha = alpha_array[0];
  double density = 0.0;

  if (alpha > 0 && alpha < priorParams[0]) {
    density = 1.0 / priorParams[0];
  } else {
    density = 0.0;
  }

  return Rcpp::NumericVector::create(density);
}

Rcpp::List WeibullMixingDistribution::mhParameterProposal(const Rcpp::List& oldParams) const {
  Rcpp::NumericVector mhStep = Rcpp::as<Rcpp::NumericVector>(mhStepSize);
  Rcpp::NumericVector old_alpha = oldParams[0];
  Rcpp::NumericVector old_lambda = oldParams[1];

  double alpha_old = old_alpha[0];
  double new_alpha = std::abs(alpha_old + mhStep[0] * R::rnorm(0.0, 1.7));

  // Create return arrays
  Rcpp::NumericVector alpha_arr(1);
  Rcpp::NumericVector lambda_arr(1);
  alpha_arr.attr("dim") = Rcpp::IntegerVector::create(1, 1, 1);
  lambda_arr.attr("dim") = Rcpp::IntegerVector::create(1, 1, 1);

  alpha_arr[0] = new_alpha;
  lambda_arr[0] = old_lambda[0]; // Lambda will be updated analytically later

  return Rcpp::List::create(
    Rcpp::Named("alpha") = alpha_arr,
    Rcpp::Named("lambda") = lambda_arr
  );
}

Rcpp::List WeibullMixingDistribution::posteriorDraw(const arma::mat& x, int n) const {
  Rcpp::NumericVector priorParams = Rcpp::as<Rcpp::NumericVector>(priorParameters);
  Rcpp::NumericVector mhStep = Rcpp::as<Rcpp::NumericVector>(mhStepSize);

  // Match R implementation - only n draws needed
  if (x.n_rows == 0) {
    return priorDraw(n);
  }

  int n_data = x.n_rows;
  arma::vec x_vec = x.col(0);

  // Storage for samples
  Rcpp::NumericVector alpha_samples(n);
  Rcpp::NumericVector lambda_samples(n);

  // Initialize
  Rcpp::List initial_draw = priorDraw(1);
  Rcpp::NumericVector alpha_init = initial_draw[0];
  Rcpp::NumericVector lambda_init = initial_draw[1];

  double alpha_current = alpha_init[0];
  double lambda_current = lambda_init[0];

  // Pre-compute sum(x^alpha) for current alpha and sample initial lambda
  double sum_x_alpha_current = 0.0;
  for (int i = 0; i < n_data; i++) {
    if (x_vec[i] > 0) {
      sum_x_alpha_current += std::pow(x_vec[i], alpha_current);
    }
  }

  // Sample lambda given current alpha (Gibbs step)
  double shape_post = priorParams[1] + n_data;
  double rate_post_current = sum_x_alpha_current + priorParams[2];
  lambda_current = 1.0 / R::rgamma(shape_post, 1.0 / rate_post_current);

  // Compute initial log likelihood and prior
  double current_log_lik = 0.0;
  for (int i = 0; i < n_data; i++) {
    if (x_vec[i] > 0) {
      current_log_lik += std::log(alpha_current) - std::log(lambda_current) +
        (alpha_current - 1.0) * std::log(x_vec[i]) -
        std::pow(x_vec[i], alpha_current) / lambda_current;
    }
  }
  double current_log_prior = (alpha_current > 0 && alpha_current <= priorParams[0]) ?
  -std::log(priorParams[0]) : -std::numeric_limits<double>::infinity();

  int accept_count = 0;
  double adaptive_mh_step = mhStep[0];  // Start with provided step size

  // Main MCMC loop - matching R's MetropolisHastings.weibull
  for (int iter = 0; iter < n; iter++) {
    // Propose new alpha (matching R's use of abs())
    double alpha_prop = std::abs(alpha_current + adaptive_mh_step * R::rnorm(0.0, 1.7));

    // Bound check
    if (alpha_prop > priorParams[0]) {
      alpha_prop = priorParams[0] * R::runif(0.5, 1.0); // Keep within bounds
    }

    // Compute sum(x^alpha) for proposed alpha
    double sum_x_alpha_prop = 0.0;
    bool valid_sum = true;
    for (int i = 0; i < n_data; i++) {
      if (x_vec[i] > 0) {
        double x_alpha = std::pow(x_vec[i], alpha_prop);
        if (std::isfinite(x_alpha)) {
          sum_x_alpha_prop += x_alpha;
        } else {
          valid_sum = false;
          break;
        }
      }
    }

    if (!valid_sum || sum_x_alpha_prop <= 0) {
      // Reject this proposal
      alpha_samples[iter] = alpha_current;
      lambda_samples[iter] = lambda_current;
      continue;
    }

    // Sample lambda given proposed alpha (Gibbs step - matching R)
    double rate_post_prop = sum_x_alpha_prop + priorParams[2];
    double lambda_prop = 1.0 / R::rgamma(shape_post, 1.0 / rate_post_prop);

    // Compute proposed log likelihood
    double proposed_log_lik = 0.0;
    for (int i = 0; i < n_data; i++) {
      if (x_vec[i] > 0) {
        proposed_log_lik += std::log(alpha_prop) - std::log(lambda_prop) +
          (alpha_prop - 1.0) * std::log(x_vec[i]) -
          std::pow(x_vec[i], alpha_prop) / lambda_prop;
      }
    }

    // Compute proposed log prior
    double proposed_log_prior = (alpha_prop > 0 && alpha_prop <= priorParams[0]) ?
    -std::log(priorParams[0]) : -std::numeric_limits<double>::infinity();

    // MH acceptance ratio
    double log_ratio = (proposed_log_lik + proposed_log_prior) -
    (current_log_lik + current_log_prior);

    double accept_prob = std::min(1.0, std::exp(log_ratio));
    if (!std::isfinite(accept_prob)) {
      accept_prob = 0.0;
    }

    // Accept/reject
    if (R::runif(0, 1) < accept_prob) {
      alpha_current = alpha_prop;
      lambda_current = lambda_prop;
      current_log_lik = proposed_log_lik;
      current_log_prior = proposed_log_prior;
      sum_x_alpha_current = sum_x_alpha_prop;
      accept_count++;
    }

    // POINT 4: Adaptive step sizing
    if (iter > 0 && iter % 50 == 0) {
      double recent_accept_rate = (double)accept_count / 50.0;

      if (recent_accept_rate < 0.15) {
        adaptive_mh_step *= 0.7;  // Decrease step size if acceptance too low
      } else if (recent_accept_rate > 0.5) {
        adaptive_mh_step *= 1.3;  // Increase step size if acceptance too high
      }

      // Reset counter
      accept_count = 0;

      // Keep step size in reasonable bounds
      adaptive_mh_step = std::max(0.01, std::min(5.0, adaptive_mh_step));
    }

    // Store current values
    alpha_samples[iter] = alpha_current;
    lambda_samples[iter] = lambda_current;
  }

  // Format output to match R structure
  alpha_samples.attr("dim") = Rcpp::IntegerVector::create(1, 1, n);
  lambda_samples.attr("dim") = Rcpp::IntegerVector::create(1, 1, n);

  return Rcpp::List::create(
    Rcpp::Named("alpha") = alpha_samples,
    Rcpp::Named("lambda") = lambda_samples
  );
}

void WeibullMixingDistribution::updatePriorParameters(const Rcpp::List& clusterParameters, int n) {
  Rcpp::NumericVector hyperPrior = Rcpp::as<Rcpp::NumericVector>(hyperPriorParameters);
  Rcpp::NumericVector currentPriorParams = Rcpp::as<Rcpp::NumericVector>(priorParameters);
  Rcpp::NumericVector alpha_params = Rcpp::as<Rcpp::NumericVector>(clusterParameters[0]);
  Rcpp::NumericVector lambda_params = Rcpp::as<Rcpp::NumericVector>(clusterParameters[1]);

  int numClusters = alpha_params.size();

  // Find max alpha
  double max_alpha = 0.0;
  for (int i = 0; i < numClusters; i++) {
    if (alpha_params[i] > max_alpha) {
      max_alpha = alpha_params[i];
    }
  }

  // Update phi using Pareto distribution
  double xm = std::max(max_alpha, hyperPrior[0]);
  double newPhi = qpareto(R::runif(0, 1), xm, hyperPrior[1] + numClusters);

  // Update gamma parameters
  double sum_inv_lambda = 0.0;
  for (int i = 0; i < numClusters; i++) {
    if (lambda_params[i] > 0) {
      sum_inv_lambda += 1.0 / lambda_params[i];
    }
  }

  double newGamma = R::rgamma(hyperPrior[2] + 2 * numClusters,
                              1.0 / (hyperPrior[3] + sum_inv_lambda));

  Rcpp::NumericMatrix newPriorParams(1, 3);
  newPriorParams(0, 0) = newPhi;
  newPriorParams(0, 1) = currentPriorParams[1];
  newPriorParams(0, 2) = newGamma;

  priorParameters = newPriorParams;
}

// Helper function for Pareto quantile
double WeibullMixingDistribution::qpareto(double p, double xm, double alpha) const {
  if (p < 0 || p > 1) return R_NaN;
  return xm * std::pow(1 - p, -1.0 / alpha);
}

// NonConjugateWeibullDP implementation
NonConjugateWeibullDP::NonConjugateWeibullDP() : mixingDistribution(nullptr), numberClusters(0), m(3) {
  // Constructor
}

NonConjugateWeibullDP::~NonConjugateWeibullDP() {
  if (mixingDistribution) {
    delete mixingDistribution;
  }
}

void NonConjugateWeibullDP::clusterComponentUpdate() {
  int n = data.n_rows;

  for (int i = 0; i < n; i++) {
    int currentLabel = clusterLabels[i];

    // Validate current label
    if (currentLabel < 0 || currentLabel >= (int)pointsPerCluster.n_elem) {
      Rcpp::Rcerr << "Warning: Invalid cluster label " << currentLabel
                  << " for point " << i << ". Resetting to 0.\n";
      currentLabel = 0;
      clusterLabels[i] = 0;
    }

    // Create auxiliary parameters
    Rcpp::List aux;
    bool needAux = (pointsPerCluster[currentLabel] == 1);

    if (needAux) {
      // Generate m auxiliary parameters
      aux = mixingDistribution->priorDraw(m);
    } else {
      // Generate m-1 auxiliary parameters
      aux = mixingDistribution->priorDraw(m - 1);
    }

    // Calculate probabilities for each possible cluster (including auxiliary)
    int totalOptions = numberClusters + m - 1;
    if (!needAux) totalOptions = numberClusters + m - 1;

    Rcpp::NumericVector probs(totalOptions);

    // Existing clusters
    for (int j = 0; j < numberClusters; j++) {
      double count = (j == currentLabel) ?
      pointsPerCluster[j] - 1 : pointsPerCluster[j];

      if (count > 0 || j != currentLabel) {
        Rcpp::NumericVector alpha_j = Rcpp::as<Rcpp::NumericVector>(clusterParameters[0]);
        Rcpp::NumericVector lambda_j = Rcpp::as<Rcpp::NumericVector>(clusterParameters[1]);

        Rcpp::NumericVector alpha_temp(1), lambda_temp(1);
        alpha_temp.attr("dim") = Rcpp::IntegerVector::create(1, 1, 1);
        lambda_temp.attr("dim") = Rcpp::IntegerVector::create(1, 1, 1);

        if (j < alpha_j.size() && j < lambda_j.size()) {
          alpha_temp[0] = alpha_j[j];
          lambda_temp[0] = lambda_j[j];

          Rcpp::List theta_j = Rcpp::List::create(alpha_temp, lambda_temp);
          arma::vec xi = data.row(i).t();
          Rcpp::NumericVector lik = mixingDistribution->likelihood(xi, theta_j);

          probs[j] = count * lik[0];
        } else {
          probs[j] = 0.0;
        }
      } else {
        probs[j] = 0.0;
      }
    }

    // Auxiliary clusters
    Rcpp::NumericVector aux_alpha = aux[0];
    Rcpp::NumericVector aux_lambda = aux[1];

    for (int j = 0; j < m - 1; j++) {
      int prob_idx = numberClusters + j;
      if (prob_idx < probs.size() && j < aux_alpha.size()) {
        Rcpp::NumericVector alpha_temp(1), lambda_temp(1);
        alpha_temp.attr("dim") = Rcpp::IntegerVector::create(1, 1, 1);
        lambda_temp.attr("dim") = Rcpp::IntegerVector::create(1, 1, 1);
        alpha_temp[0] = aux_alpha[j];
        lambda_temp[0] = aux_lambda[j];

        Rcpp::List theta_aux = Rcpp::List::create(alpha_temp, lambda_temp);
        arma::vec xi = data.row(i).t();
        Rcpp::NumericVector lik = mixingDistribution->likelihood(xi, theta_aux);

        probs[prob_idx] = (alpha / (m - 1)) * lik[0];
      }
    }

    // Handle numerical issues
    double prob_sum = 0.0;
    for (int j = 0; j < probs.size(); j++) {
      if (!std::isfinite(probs[j]) || probs[j] < 0) {
        probs[j] = 0.0;
      }
      prob_sum += probs[j];
    }

    if (prob_sum <= 0) {
      // Fallback to uniform
      for (int j = 0; j < probs.size(); j++) {
        probs[j] = 1.0 / probs.size();
      }
      prob_sum = 1.0;
    }

    // Normalize
    probs = probs / prob_sum;

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

    // Update the state
    pointsPerCluster[currentLabel]--;

    // Perform the label change
    Rcpp::List updateResult = clusterLabelChange(i, newLabel, currentLabel, aux);

    // Update state from result
    clusterLabels = Rcpp::as<arma::uvec>(updateResult["clusterLabels"]);
    pointsPerCluster = Rcpp::as<arma::uvec>(updateResult["pointsPerCluster"]);
    clusterParameters = updateResult["clusterParameters"];
    numberClusters = updateResult["numberClusters"];
  }
}

void NonConjugateWeibullDP::clusterParameterUpdate() {
  for (int k = 0; k < numberClusters; k++) {
    arma::uvec clusterIndices = arma::find(clusterLabels == k);
    if (clusterIndices.n_elem > 0) {
      arma::mat clusterData = data.rows(clusterIndices);

      // Extract current parameters
      Rcpp::NumericVector alpha_params = Rcpp::as<Rcpp::NumericVector>(clusterParameters[0]);
      Rcpp::NumericVector lambda_params = Rcpp::as<Rcpp::NumericVector>(clusterParameters[1]);

      if (k < alpha_params.size() && k < lambda_params.size()) {
        // Draw from posterior
        Rcpp::List postDraw = mixingDistribution->posteriorDraw(clusterData, 1);

        Rcpp::NumericVector new_alpha = postDraw[0];
        Rcpp::NumericVector new_lambda = postDraw[1];

        alpha_params[k] = new_alpha[0];
        lambda_params[k] = new_lambda[0];

        clusterParameters[0] = alpha_params;
        clusterParameters[1] = lambda_params;
      }
    }
  }
}

void NonConjugateWeibullDP::updateAlpha() {
  // Same implementation as Beta/Normal
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

Rcpp::List NonConjugateWeibullDP::clusterLabelChange(int i, int newLabel, int currentLabel,
                                                     const Rcpp::List& aux) {
  if (newLabel == currentLabel) {
    // No change needed
    pointsPerCluster[currentLabel]++;
    return Rcpp::List::create(
      Rcpp::Named("clusterLabels") = clusterLabels,
      Rcpp::Named("pointsPerCluster") = pointsPerCluster,
      Rcpp::Named("clusterParameters") = clusterParameters,
      Rcpp::Named("numberClusters") = numberClusters
    );
  }

  // Extract current parameters
  Rcpp::NumericVector alpha_vec = Rcpp::clone(Rcpp::as<Rcpp::NumericVector>(clusterParameters[0]));
  Rcpp::NumericVector lambda_vec = Rcpp::clone(Rcpp::as<Rcpp::NumericVector>(clusterParameters[1]));

  // Handle new cluster assignment
  if (newLabel < numberClusters) {
    // Existing cluster
    pointsPerCluster[newLabel]++;
    clusterLabels[i] = newLabel;
  } else {
    // New cluster from auxiliary
    int aux_idx = newLabel - numberClusters;
    Rcpp::NumericVector aux_alpha = aux[0];
    Rcpp::NumericVector aux_lambda = aux[1];

    if (aux_idx < aux_alpha.size()) {
      // Add new cluster
      numberClusters++;

      // Extend parameter vectors
      Rcpp::NumericVector new_alpha_vec(numberClusters);
      Rcpp::NumericVector new_lambda_vec(numberClusters);

      for (int j = 0; j < alpha_vec.size(); j++) {
        new_alpha_vec[j] = alpha_vec[j];
        new_lambda_vec[j] = lambda_vec[j];
      }

      new_alpha_vec[numberClusters - 1] = aux_alpha[aux_idx];
      new_lambda_vec[numberClusters - 1] = aux_lambda[aux_idx];

      alpha_vec = new_alpha_vec;
      lambda_vec = new_lambda_vec;

      // Extend pointsPerCluster
      arma::uvec new_points = arma::zeros<arma::uvec>(numberClusters);
      for (int j = 0; j < (int)pointsPerCluster.n_elem; j++) {
        new_points[j] = pointsPerCluster[j];
      }
      new_points[numberClusters - 1] = 1;
      pointsPerCluster = new_points;

      clusterLabels[i] = numberClusters - 1;
    }
  }

  // Clean up empty clusters
  if (pointsPerCluster[currentLabel] == 0 && numberClusters > 1) {
    // Remove empty cluster
    numberClusters--;

    // Create new vectors without the empty cluster
    Rcpp::NumericVector new_alpha_vec(numberClusters);
    Rcpp::NumericVector new_lambda_vec(numberClusters);
    arma::uvec new_points = arma::zeros<arma::uvec>(numberClusters);

    int new_idx = 0;
    std::map<int, int> label_map;

    for (int j = 0; j < alpha_vec.size(); j++) {
      if (j != currentLabel && j < (int)pointsPerCluster.n_elem) {
        new_alpha_vec[new_idx] = alpha_vec[j];
        new_lambda_vec[new_idx] = lambda_vec[j];
        new_points[new_idx] = pointsPerCluster[j];
        label_map[j] = new_idx;
        new_idx++;
      }
    }

    // Update all cluster labels
    for (int k = 0; k < (int)clusterLabels.size(); k++) {
      if (label_map.find(clusterLabels[k]) != label_map.end()) {
        clusterLabels[k] = label_map[clusterLabels[k]];
      }
    }

    alpha_vec = new_alpha_vec;
    lambda_vec = new_lambda_vec;
    pointsPerCluster = new_points;
  }

  // Update cluster parameters
  clusterParameters = Rcpp::List::create(alpha_vec, lambda_vec);

  return Rcpp::List::create(
    Rcpp::Named("clusterLabels") = clusterLabels,
    Rcpp::Named("pointsPerCluster") = pointsPerCluster,
    Rcpp::Named("clusterParameters") = clusterParameters,
    Rcpp::Named("numberClusters") = numberClusters
  );
}

} // namespace dp
