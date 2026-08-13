// src/mixing_distribution_base.cpp
#include "mixing_distribution_base.h"
#include "gaussian_mixing.h"
#include "beta_mixing.h"
#include "mvnormal_mixing.h"
#include "mvnormal_covariance_mixing.h"
#include "weibull_mixing.h"
#include "exponential_mixing.h"
#include "hierarchical_beta_mixing.h"
#include "beta2_mixing.h"
#include "normal_fixed_variance_mixing.h"
#include "mvnormal2_mixing.h"
#include <RcppArmadillo.h>

namespace dirichletprocess {

std::unique_ptr<MixingDistribution> MixingDistribution::create(
    const std::string& type,
    const Rcpp::List& params) {

  if (type == "gaussian") {
    double mu0 = Rcpp::as<double>(params["mu0"]);
    double kappa0 = Rcpp::as<double>(params["kappa0"]);
    double alpha0 = Rcpp::as<double>(params["alpha0"]);
    double beta0 = Rcpp::as<double>(params["beta0"]);
    return std::unique_ptr<MixingDistribution>(
      new GaussianMixing(mu0, kappa0, alpha0, beta0));
  } else if (type == "normalFixedVariance") {
    double mu0 = params.containsElementNamed("mu0") ?
    Rcpp::as<double>(params["mu0"]) : 0.0;
    double sigma0 = params.containsElementNamed("sigma0") ?
    Rcpp::as<double>(params["sigma0"]) : 1.0;
    double sigma = Rcpp::as<double>(params["sigma"]);  // Required parameter

    return std::unique_ptr<MixingDistribution>(
      new NormalFixedVarianceMixing(mu0, sigma0, sigma));
  } else if (type == "beta") {
    double alpha0 = Rcpp::as<double>(params["alpha0"]);
    double beta0 = Rcpp::as<double>(params["beta0"]);
    double maxT = params.containsElementNamed("maxT") ?
    Rcpp::as<double>(params["maxT"]) : 1.0;
    Rcpp::NumericVector mhStepSize = params.containsElementNamed("mhStepSize") ?
      Rcpp::as<Rcpp::NumericVector>(params["mhStepSize"]) :
      Rcpp::NumericVector::create(1.0, 1.0);
    if (mhStepSize.size() < 2) {
      mhStepSize = Rcpp::NumericVector::create(mhStepSize[0], mhStepSize[0]);
    }
    int mh_draws = params.containsElementNamed("mh_draws") ?
      Rcpp::as<int>(params["mh_draws"]) : 250;

    return std::unique_ptr<MixingDistribution>(
      new BetaMixing(alpha0, beta0, maxT, mhStepSize[0], mhStepSize[1], mh_draws));
  } else if (type == "beta2") {
    double gamma_prior = params.containsElementNamed("gamma_prior") ?
    Rcpp::as<double>(params["gamma_prior"]) : 2.0;
    double maxT = params.containsElementNamed("maxT") ?
    Rcpp::as<double>(params["maxT"]) : 1.0;

    arma::vec mh_step_size(2);
    if (params.containsElementNamed("mh_step_size")) {
      mh_step_size = Rcpp::as<arma::vec>(params["mh_step_size"]);
    } else {
      mh_step_size.fill(1.0);
    }

    int mh_draws = params.containsElementNamed("mh_draws") ?
    Rcpp::as<int>(params["mh_draws"]) : 250;

    return std::unique_ptr<MixingDistribution>(
      new Beta2Mixing(gamma_prior, maxT, mh_step_size, mh_draws));

  } else if (type == "mvnormal") {
    arma::vec mu0 = Rcpp::as<arma::vec>(params["mu0"]);
    double kappa0 = Rcpp::as<double>(params["kappa0"]);
    arma::mat Lambda = Rcpp::as<arma::mat>(params["Lambda"]);
    double nu = Rcpp::as<double>(params["nu"]);
    
    // Check if covariance model is specified
    std::string covModel = "FULL"; // Default
    if (params.containsElementNamed("covModel")) {
      covModel = Rcpp::as<std::string>(params["covModel"]);
    }
    
    // Use enhanced covariance mixing distribution only for non-FULL models
    if (covModel != "FULL") {
      return std::unique_ptr<MixingDistribution>(
        new MVNormalCovarianceMixing(mu0, kappa0, Lambda, nu, covModel));
    } else {
      // Use original MVNormalMixing for FULL model (more stable)
      return std::unique_ptr<MixingDistribution>(
        new MVNormalMixing(mu0, kappa0, Lambda, nu));
    }
  } else if (type == "weibull") {
    double phi = Rcpp::as<double>(params["phi"]);
    double alpha0 = Rcpp::as<double>(params["alpha0"]);
    double beta0 = Rcpp::as<double>(params["beta0"]);

    // Optional hyperprior parameters
    double hyper_a1 = params.containsElementNamed("hyper_a1") ?
    Rcpp::as<double>(params["hyper_a1"]) : 6.0;
    double hyper_a2 = params.containsElementNamed("hyper_a2") ?
    Rcpp::as<double>(params["hyper_a2"]) : 2.0;
    double hyper_b1 = params.containsElementNamed("hyper_b1") ?
    Rcpp::as<double>(params["hyper_b1"]) : 1.0;
    double hyper_b2 = params.containsElementNamed("hyper_b2") ?
    Rcpp::as<double>(params["hyper_b2"]) : 0.5;

    // MH parameters
    double mh_step_alpha = params.containsElementNamed("mh_step_alpha") ?
    Rcpp::as<double>(params["mh_step_alpha"]) : 0.1;
    int mh_draws = params.containsElementNamed("mh_draws") ?
    Rcpp::as<int>(params["mh_draws"]) : 100;

    return std::unique_ptr<MixingDistribution>(
      new WeibullMixing(phi, alpha0, beta0, hyper_a1, hyper_a2,
                        hyper_b1, hyper_b2, mh_step_alpha, mh_draws));
  } else if (type == "exponential") {
    // Extract prior parameters
    double alpha0 = Rcpp::as<double>(params["alpha0"]);
    double beta0 = Rcpp::as<double>(params["beta0"]);

    return std::unique_ptr<MixingDistribution>(
      new ExponentialMixing(alpha0, beta0));
  } else if (type == "hierarchical_beta") {
    double alpha0 = Rcpp::as<double>(params["alpha0"]);
    double beta0 = Rcpp::as<double>(params["beta0"]);
    double maxT = Rcpp::as<double>(params["maxT"]);

    double gamma_shape = 2.0;
    double gamma_rate = 4.0;
    if (params.containsElementNamed("gamma_prior_shape")) {
      gamma_shape = Rcpp::as<double>(params["gamma_prior_shape"]);
    }
    if (params.containsElementNamed("gamma_prior_rate")) {
      gamma_rate = Rcpp::as<double>(params["gamma_prior_rate"]);
    }

    return std::unique_ptr<MixingDistribution>(
      new HierarchicalBetaMixing(alpha0, beta0, maxT, gamma_shape, gamma_rate));
  } else if (type == "hierarchical_mvnormal") {
    // Hierarchical MVNormal uses the standard MVNormal as base
    return std::unique_ptr<MixingDistribution>(
      new MVNormalMixing(
          Rcpp::as<arma::vec>(params["mu0"]),
          Rcpp::as<double>(params["kappa0"]),
          Rcpp::as<arma::mat>(params["Lambda"]),
          Rcpp::as<double>(params["nu"])
      )
    );
  } else if (type == "mvnormal2") {
    // MVNormal2 semi-conjugate distribution
    arma::mat mu0 = Rcpp::as<arma::mat>(params["mu0"]);
    arma::mat sigma0 = Rcpp::as<arma::mat>(params["sigma0"]);
    arma::mat phi0 = Rcpp::as<arma::mat>(params["phi0"]);
    double nu0 = Rcpp::as<double>(params["nu0"]);
    int mh_draws = params.containsElementNamed("mh_draws") ?
      Rcpp::as<int>(params["mh_draws"]) : 250;

    return std::unique_ptr<MixingDistribution>(
      new MVNormal2Mixing(mu0, sigma0, phi0, nu0, mh_draws)
    );
  }

  Rcpp::stop("Unknown mixing distribution type: " + type);
}

} // namespace dirichletprocess
