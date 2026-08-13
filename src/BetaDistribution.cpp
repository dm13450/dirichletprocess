// src/BetaDistribution.cpp
#include "BetaDistribution.h"
#include "RcppConversions.h"
#include <cmath>
#include <limits>

namespace dp {

// BetaMixingDistribution implementation
BetaMixingDistribution::BetaMixingDistribution(const Rcpp::NumericVector& priorParams) : maxT(1.0) {
  distribution = "beta";
  conjugate = false;
  priorParameters = priorParams;
  mhStepSize = Rcpp::NumericVector::create(1.0, 1.0);
  hyperPriorParameters = Rcpp::NumericVector::create(1.0, 0.125);
}

BetaMixingDistribution::~BetaMixingDistribution() {
  // Destructor
}

Rcpp::NumericVector BetaMixingDistribution::likelihood(const arma::vec& x_data, const Rcpp::List& theta) const {
  Rcpp::NumericVector mu_array = theta[0];
  Rcpp::NumericVector nu_array = theta[1];
  int n_data = x_data.n_elem;
  Rcpp::NumericVector result(n_data);
  double mu = mu_array[0];
  double tau = nu_array[0];

  if (tau <= 1e-10) { // If precision is too low, likelihood is ill-defined or zero
    result.fill(1e-300);
    return result;
  }

  double a = (mu * tau) / maxT;
  double b = (1.0 - mu/maxT) * tau;

  for (int i = 0; i < n_data; i++) {
    if (x_data[i] >= 0 && x_data[i] <= maxT && a > 0 && b > 0 && std::isfinite(a) && std::isfinite(b)) {
      result[i] = (1.0/maxT) * R::dbeta(x_data[i]/maxT, a, b, false);
    } else {
      result[i] = 1e-300;
    }
  }
  return result;
}

Rcpp::List BetaMixingDistribution::priorDraw(int n_draws) const {
  Rcpp::NumericVector priorParams_local = Rcpp::as<Rcpp::NumericVector>(this->priorParameters);
  Rcpp::NumericVector mu_values(n_draws);
  Rcpp::NumericVector nu_values(n_draws);

  if (n_draws > 0) {
    Rcpp::Function r_runif("runif");
    Rcpp::Function r_rgamma("rgamma"); // R's rgamma(n, shape, rate)

    double gamma_shape = priorParams_local[0];
    double gamma_rate = priorParams_local[1]; // This is the RATE for Gamma distribution of (1/nu)

    if (gamma_shape <= 0 || gamma_rate <= 0) {
      Rcpp::stop("Invalid shape or rate parameter for Gamma distribution in BetaMixingDistribution::priorDraw.");
    }

    if (n_draws == 1) {
      mu_values[0] = Rcpp::as<double>(r_runif(1, 0.0, this->maxT));
      // Explicitly name arguments for r_rgamma to ensure correct parameter matching
      Rcpp::NumericVector gamma_draw_vec = r_rgamma(1, Rcpp::Named("shape", gamma_shape), Rcpp::Named("rate", gamma_rate));
      if (gamma_draw_vec[0] > 1e-10) {
        nu_values[0] = 1.0 / gamma_draw_vec[0];
      } else {
        nu_values[0] = std::numeric_limits<double>::max();
      }
    } else {
      mu_values = r_runif(n_draws, 0.0, this->maxT);
      Rcpp::NumericVector gamma_draws = r_rgamma(n_draws, Rcpp::Named("shape", gamma_shape), Rcpp::Named("rate", gamma_rate));
      for (int i = 0; i < n_draws; ++i) {
        if (gamma_draws[i] > 1e-10) {
          nu_values[i] = 1.0 / gamma_draws[i];
        } else {
          nu_values[i] = std::numeric_limits<double>::max();
        }
      }
    }
  }

  Rcpp::NumericVector mu_arr = Rcpp::clone(mu_values);
  Rcpp::NumericVector nu_arr = Rcpp::clone(nu_values);
  mu_arr.attr("dim") = Rcpp::IntegerVector::create(1, 1, n_draws);
  nu_arr.attr("dim") = Rcpp::IntegerVector::create(1, 1, n_draws);

  return Rcpp::List::create(
    Rcpp::Named("mu") = mu_arr,
    Rcpp::Named("nu") = nu_arr
  );
}

double BetaMixingDistribution::priorDensity(const Rcpp::List& theta) const {
  Rcpp::NumericVector priorParams = Rcpp::as<Rcpp::NumericVector>(priorParameters);
  Rcpp::NumericVector mu_array = theta[0];
  Rcpp::NumericVector nu_array = theta[1];

  double mu = mu_array[0];
  double nu = nu_array[0];

  double muDensity = (mu > 1e-10 && mu < (maxT - 1e-10)) ? 1.0/maxT : 1e-10;
  double nuDensity = 1e-10;

  if (nu > 1e-10) {
    double gamma_shape = priorParams[0];
    double r_gamma_rate_param = priorParams[1]; // This is the RATE for R's dgamma for (1/nu)
    // R::dgamma (C API) takes (x, shape, SCALE, log)

    if (gamma_shape > 0 && r_gamma_rate_param > 0) {
      double c_api_gamma_scale_param = 1.0 / r_gamma_rate_param; // Convert rate to scale for C API R::dgamma
      double val = R::dgamma(1.0/nu, gamma_shape, c_api_gamma_scale_param, false) / (nu * nu);
      if (val > 1e-300 && std::isfinite(val)) {
        nuDensity = val;
      }
    }
  }
  return muDensity * nuDensity;
}

Rcpp::List BetaMixingDistribution::mhParameterProposal(const Rcpp::List& oldParams) const {
  Rcpp::NumericVector mhStep = Rcpp::as<Rcpp::NumericVector>(mhStepSize);
  Rcpp::NumericVector old_mu_vec = oldParams[0];
  Rcpp::NumericVector old_nu_vec = oldParams[1];
  double old_mu = old_mu_vec[0];
  double old_nu = old_nu_vec[0];
  double new_mu = old_mu + mhStep[0] * R::rnorm(0.0, 1.0);

  // Reflecting boundaries for mu
  if (new_mu <= 1e-6) new_mu = 1e-6 + (1e-6 - new_mu);
  if (new_mu >= maxT - 1e-6) new_mu = (maxT - 1e-6) - (new_mu - (maxT - 1e-6));
  if (new_mu <= 1e-6) new_mu = 1e-6;
  if (new_mu >= maxT - 1e-6) new_mu = maxT - 1e-6;


  double new_nu = old_nu + mhStep[1] * R::rnorm(0.0, 1.0);
  if (new_nu <= 1e-6) new_nu = std::abs(old_nu - mhStep[1] * R::rnorm(0.0,1.0)) + 1e-6; // Try reflecting if proposed is bad
  if (new_nu <= 1e-6) new_nu = 1e-6; // Fallback

  Rcpp::NumericVector mu_arr(1);
  Rcpp::NumericVector nu_arr(1);
  mu_arr.attr("dim") = Rcpp::IntegerVector::create(1, 1, 1);
  nu_arr.attr("dim") = Rcpp::IntegerVector::create(1, 1, 1);
  mu_arr[0] = new_mu;
  nu_arr[0] = new_nu;
  return Rcpp::List::create(
    Rcpp::Named("mu") = mu_arr,
    Rcpp::Named("nu") = nu_arr
  );
}

Rcpp::List BetaMixingDistribution::metropolisHastings(const arma::mat& x_data,
                                                      const Rcpp::List& startPos,
                                                      int noDraws) const {
  Rcpp::NumericVector mu_samples(noDraws);
  Rcpp::NumericVector nu_samples(noDraws);
  Rcpp::NumericVector start_mu_vec = startPos[0];
  Rcpp::NumericVector start_nu_vec = startPos[1];
  Rcpp::List current_params = Rcpp::clone(startPos);
  double current_mu = start_mu_vec[0];
  double current_nu = start_nu_vec[0];
  double current_log_lik = 0.0;
  Rcpp::NumericVector lik_vals = likelihood(arma::vectorise(x_data), current_params);
  for (int k = 0; k < lik_vals.size(); k++) {
    if (lik_vals[k] > 1e-300 && std::isfinite(lik_vals[k])) {
      current_log_lik += std::log(lik_vals[k]);
    } else {
      current_log_lik = -std::numeric_limits<double>::infinity();
      break;
    }
  }
  double current_log_prior_dens = priorDensity(current_params);
  double current_log_prior = (current_log_prior_dens > 1e-300 && std::isfinite(current_log_prior_dens)) ? std::log(current_log_prior_dens) : -std::numeric_limits<double>::infinity();
  int accept_count = 0;
  mu_samples[0] = current_mu;
  nu_samples[0] = current_nu;

  for (int iter = 1; iter < noDraws; iter++) {
    Rcpp::List proposed_params = mhParameterProposal(current_params);
    double proposed_mu = Rcpp::as<Rcpp::NumericVector>(proposed_params[0])[0];
    double proposed_nu = Rcpp::as<Rcpp::NumericVector>(proposed_params[1])[0];
    double proposed_log_lik = 0.0;
    Rcpp::NumericVector prop_lik_vals = likelihood(arma::vectorise(x_data), proposed_params);
    for (int k = 0; k < prop_lik_vals.size(); k++) {
      if (prop_lik_vals[k] > 1e-300 && std::isfinite(prop_lik_vals[k])) {
        proposed_log_lik += std::log(prop_lik_vals[k]);
      } else {
        proposed_log_lik = -std::numeric_limits<double>::infinity();
        break;
      }
    }
    double proposed_log_prior_dens = priorDensity(proposed_params);
    double proposed_log_prior = (proposed_log_prior_dens > 1e-300 && std::isfinite(proposed_log_prior_dens)) ? std::log(proposed_log_prior_dens) : -std::numeric_limits<double>::infinity();

    double log_ratio = (proposed_log_lik + proposed_log_prior) -
      (current_log_lik + current_log_prior);

    double accept_prob = 0.0;
    if (std::isfinite(log_ratio)) {
      accept_prob = std::min(1.0, std::exp(log_ratio));
    } else if (proposed_log_lik > current_log_lik) {
      if (!std::isfinite(current_log_lik) && std::isfinite(proposed_log_lik) && std::isfinite(proposed_log_prior)) {
        accept_prob = 1.0;
      }
    }


    if (R::runif(0, 1) < accept_prob) {
      current_params = proposed_params;
      current_mu = proposed_mu;
      current_nu = proposed_nu;
      current_log_lik = proposed_log_lik;
      current_log_prior = proposed_log_prior;
      accept_count++;
    }
    mu_samples[iter] = current_mu;
    nu_samples[iter] = current_nu;
  }

  // Suppress unused variable warning - accept_count tracked for potential future diagnostics
  (void)accept_count;

  return Rcpp::List::create(
    Rcpp::Named("mu") = mu_samples,
    Rcpp::Named("nu") = nu_samples
  );
}

Rcpp::List BetaMixingDistribution::posteriorDraw(const arma::mat& x, int n) const {
  // Handle empty cluster
  if (x.n_rows == 0) {
    return priorDraw(n);
  }

  // Use Metropolis-Hastings for non-conjugate case
  Rcpp::List startPos = priorDraw(1);

  // Ensure mhStepSize is properly set
  Rcpp::NumericVector stepSize = Rcpp::as<Rcpp::NumericVector>(this->mhStepSize);
  if (stepSize.size() < 2) {
    stepSize = Rcpp::NumericVector::create(0.1, 0.1);
  }

  // Create a temporary mdObj for MH sampling
  Rcpp::List mdObj = Rcpp::List::create(
    Rcpp::Named("priorParameters") = this->priorParameters,
    Rcpp::Named("mhStepSize") = stepSize,
    Rcpp::Named("maxT") = this->maxT
  );
  mdObj.attr("class") = Rcpp::CharacterVector::create("beta", "nonconjugate", "list");

  // Run Metropolis-Hastings
  // For hierarchical models, use fewer draws to avoid nested MCMC performance issues
  int mhDraws = std::max(10, n * 2); // Minimum 10 draws for convergence, but much less than 250
  Rcpp::List mhResult = metropolisHastings(x, startPos, mhDraws);

  // Extract samples
  Rcpp::List paramSamples = mhResult["parameter_samples"];
  if (paramSamples.size() >= 2) {
    Rcpp::NumericVector muAll = paramSamples[0];
    Rcpp::NumericVector nuAll = paramSamples[1];

    // Thin samples to get n draws
    int thin = std::max(1, mhDraws / n);
    Rcpp::NumericVector muSamples(n);
    Rcpp::NumericVector nuSamples(n);

    for (int i = 0; i < n; i++) {
      int idx = std::min(i * thin, mhDraws - 1);
      muSamples[i] = muAll[idx];
      nuSamples[i] = nuAll[idx];
    }

    muSamples.attr("dim") = Rcpp::IntegerVector::create(1, 1, n);
    nuSamples.attr("dim") = Rcpp::IntegerVector::create(1, 1, n);

    return Rcpp::List::create(
      Rcpp::Named("mu") = muSamples,
      Rcpp::Named("nu") = nuSamples
    );
  }

  // Fallback to prior
  return priorDraw(n);
}

void BetaMixingDistribution::updatePriorParameters(const Rcpp::List& clusterParametersList, int n_clusters_unused_arg) {
  Rcpp::NumericVector hyperPrior = Rcpp::as<Rcpp::NumericVector>(hyperPriorParameters);
  Rcpp::NumericVector currentPriorParams = Rcpp::as<Rcpp::NumericVector>(priorParameters);
  Rcpp::NumericVector nu_params_array = Rcpp::as<Rcpp::NumericVector>(clusterParametersList[1]); // Assumes this is [1,1,N] or flat [N]

  int num_clusters_found = 0;
  if(nu_params_array.attr("dim") != R_NilValue) { // Check if it has dimensions (e.g. [1,1,N])
    Rcpp::IntegerVector dims = nu_params_array.attr("dim");
    if (dims.length() == 3) num_clusters_found = dims[2];
    else if (dims.length() == 1 || dims.length() == 2) num_clusters_found = nu_params_array.length(); // Flat or 2D
    else num_clusters_found = nu_params_array.length(); // Fallback
  } else {
    num_clusters_found = nu_params_array.length(); // If no dim, it's flat
  }

  double sum_inv_nu = 0.0;
  if (num_clusters_found > 0) {
    for (int i = 0; i < num_clusters_found; ++i) {
      if (nu_params_array[i] > 1e-10) {
        sum_inv_nu += 1.0 / nu_params_array[i];
      }
    }
  }

  double posterior_shape_for_beta_nu = hyperPrior[0] + num_clusters_found * currentPriorParams[0];
  double posterior_rate_for_beta_nu = hyperPrior[1] + sum_inv_nu;

  if (posterior_shape_for_beta_nu <= 0) posterior_shape_for_beta_nu = 1e-6;
  if (posterior_rate_for_beta_nu <= 0) posterior_rate_for_beta_nu = 1e-6;

  double new_beta_nu = R::rgamma(posterior_shape_for_beta_nu, 1.0 / posterior_rate_for_beta_nu); // R::rgamma needs scale = 1/rate
  if (new_beta_nu <= 0) new_beta_nu = 1e-6;

  Rcpp::NumericVector newPriorParams = Rcpp::NumericVector::create(currentPriorParams[0], new_beta_nu);
  priorParameters = newPriorParams;
}


Rcpp::List BetaMixingDistribution::priorDrawStatic(const Rcpp::NumericVector& priorParams,
                                                   double maxT_val, int n_draws) {
  BetaMixingDistribution md(priorParams);
  md.maxT = maxT_val;
  return md.priorDraw(n_draws);
}

Rcpp::List BetaMixingDistribution::posteriorDrawStatic(const Rcpp::NumericVector& priorParams,
                                                       double maxT_val,
                                                       const Rcpp::NumericVector& mhStepSize_val,
                                                       const arma::mat& x_data,
                                                       int n_draws, int mhDrawsVal) {
  BetaMixingDistribution md(priorParams);
  md.maxT = maxT_val;
  md.mhStepSize = mhStepSize_val;
  Rcpp::List start_pos = md.priorDraw(1);
  Rcpp::List mh_result = md.metropolisHastings(x_data, start_pos, mhDrawsVal);
  Rcpp::NumericVector mu_all = mh_result["mu"];
  Rcpp::NumericVector nu_all = mh_result["nu"];
  Rcpp::NumericVector mu_final(n_draws);
  Rcpp::NumericVector nu_final(n_draws);

  if (mhDrawsVal >= n_draws) {
    int current_mh_idx = mhDrawsVal - n_draws;
    for(int i = 0; i < n_draws; ++i) {
      mu_final[i] = mu_all[current_mh_idx + i];
      nu_final[i] = nu_all[current_mh_idx + i];
    }
  } else {
    for(int i = 0; i < mhDrawsVal; ++i) {
      mu_final[i] = mu_all[i];
      nu_final[i] = nu_all[i];
    }
    for(int i = mhDrawsVal; i < n_draws; ++i) {
      mu_final[i] = mu_all[mhDrawsVal > 0 ? mhDrawsVal - 1 : 0];
      nu_final[i] = nu_all[mhDrawsVal > 0 ? mhDrawsVal - 1 : 0];
    }
  }

  mu_final.attr("dim") = Rcpp::IntegerVector::create(1, 1, n_draws);
  nu_final.attr("dim") = Rcpp::IntegerVector::create(1, 1, n_draws);
  return Rcpp::List::create(
    Rcpp::Named("mu") = mu_final,
    Rcpp::Named("nu") = nu_final
  );
}

Rcpp::NumericVector BetaMixingDistribution::likelihoodStatic(const arma::vec& x_data,
                                                             double mu_val, double nu_val,
                                                             double maxT_val) {
  Rcpp::NumericVector mu_arr(1);
  Rcpp::NumericVector nu_arr(1);
  mu_arr.attr("dim") = Rcpp::IntegerVector::create(1, 1, 1);
  nu_arr.attr("dim") = Rcpp::IntegerVector::create(1, 1, 1);
  mu_arr[0] = mu_val;
  nu_arr[0] = nu_val;
  Rcpp::List theta = Rcpp::List::create(mu_arr, nu_arr);
  BetaMixingDistribution md(Rcpp::NumericVector::create(2, 8));
  md.maxT = maxT_val;
  return md.likelihood(x_data, theta);
}

// NonConjugateBetaDP methods
void NonConjugateBetaDP::clusterParameterUpdate() {
  for (int k = 0; k < numberClusters; k++) {
    arma::uvec clusterIndices = arma::find(clusterLabels == k);
    if (clusterIndices.n_elem > 0) {
      arma::mat clusterData = data.rows(clusterIndices);
      Rcpp::List currentClusterParamsList = Rcpp::as<Rcpp::List>(clusterParameters);
      Rcpp::NumericVector mu_params_array = Rcpp::as<Rcpp::NumericVector>(currentClusterParamsList[0]);
      Rcpp::NumericVector nu_params_array = Rcpp::as<Rcpp::NumericVector>(currentClusterParamsList[1]);
      Rcpp::NumericVector mu_start_val(1);
      Rcpp::NumericVector nu_start_val(1);
      mu_start_val[0] = mu_params_array[k]; // Assumes mu_params_array is correctly indexed for cluster k
      nu_start_val[0] = nu_params_array[k]; // Assumes nu_params_array is correctly indexed for cluster k
      mu_start_val.attr("dim") = Rcpp::IntegerVector::create(1,1,1);
      nu_start_val.attr("dim") = Rcpp::IntegerVector::create(1,1,1);
      Rcpp::List start_pos = Rcpp::List::create(Rcpp::Named("mu") = mu_start_val,
                                                Rcpp::Named("nu") = nu_start_val);

      int n_mh_draws = this->mhDraws; // Accessing inherited member

      Rcpp::List mh_result = mixingDistribution->metropolisHastings(clusterData, start_pos, n_mh_draws);
      Rcpp::NumericVector mu_samples = mh_result["mu"];
      Rcpp::NumericVector nu_samples = mh_result["nu"];
      if(mu_samples.size() > 0) {
        mu_params_array[k] = mu_samples[mu_samples.size() - 1];
        nu_params_array[k] = nu_samples[nu_samples.size() - 1];
      }
    }
  }
}

Rcpp::List NonConjugateBetaDP::toR() const {
  Rcpp::List result;

  result["data"] = data;
  result["n"] = n;
  result["alpha"] = alpha;
  result["alphaPriorParameters"] = alphaPriorParameters;

  // Ensure cluster labels are valid (1-indexed for R)
  if (clusterLabels.n_elem == 0 && n > 0) {
    // Initialize with all points in one cluster if empty
    result["clusterLabels"] = arma::uvec(n, arma::fill::ones);
    result["numberClusters"] = 1;
    result["pointsPerCluster"] = arma::uvec({static_cast<arma::uword>(n)});

    // Initialize cluster parameters
    Rcpp::List init_params = mixingDistribution->priorDraw(1);
    result["clusterParameters"] = init_params;
  } else {
    // Convert to 1-indexed for R
    arma::uvec r_labels = clusterLabels + 1;
    result["clusterLabels"] = r_labels;
    result["numberClusters"] = numberClusters;
    result["pointsPerCluster"] = pointsPerCluster;
    result["clusterParameters"] = clusterParameters;
  }

  result["m"] = m;

  if (mixingDistribution) {
    result["mixingDistribution"] = mixingDistribution->toR();
  }

  return result;
}

void NonConjugateBetaDP::updateAlpha() {
  double x_draw_val = R::rbeta(alpha + 1.0, n);
  Rcpp::NumericVector currentAlphaPrior = Rcpp::as<Rcpp::NumericVector>(alphaPriorParameters);
  double log_x_draw_val = 0.0;
  if (x_draw_val <= 1e-10 || x_draw_val >=1.0 - 1e-10) {
    log_x_draw_val = std::log(1e-10);
  } else {
    log_x_draw_val = std::log(x_draw_val);
  }
  double pi1_num = currentAlphaPrior[0] + numberClusters -1.0;
  double term_for_pi2 = currentAlphaPrior[1] - log_x_draw_val;

  double pi_val;
  if (term_for_pi2 <= 0 || pi1_num < 0 ) { // Adjusted condition for pi1_num to allow 0
    if (pi1_num <=0 && (n * term_for_pi2) <=0 ) pi_val = 0.5; // Both non-positive or ambiguous
    else pi_val = (pi1_num > (pi1_num + n * term_for_pi2)) ? 1.0 : 0.0;
  } else {
    double pi2_num = n * term_for_pi2;
    if (std::abs(pi1_num + pi2_num) < 1e-10) { // Avoid division by zero
      pi_val = (pi1_num > 0) ? 1.0 : 0.5;
    } else {
      pi_val = pi1_num / (pi1_num + pi2_num);
    }
  }
  if (pi_val < 0) pi_val = 0; // Ensure probability is not negative
  if (pi_val > 1) pi_val = 1; // Ensure probability is not > 1


  double postShape;
  if (R::runif(0,1) < pi_val){
    postShape = currentAlphaPrior[0] + numberClusters;
  } else {
    postShape = currentAlphaPrior[0] + numberClusters - 1.0;
  }
  if (postShape <=0) postShape = 1e-6;

  double postRate = currentAlphaPrior[1] - log_x_draw_val;
  if (postRate <=0) postRate = 1e-6;

  alpha = R::rgamma(postShape, 1.0/postRate);
  if (alpha <=0) alpha = 1e-6;
}

} // namespace dp
