// src/DirichletProcess.cpp
#include "DirichletProcessBase.h"
#include "RcppConversions.h" // For Rcpp::as and other conversions

namespace dp {

// Default constructor
DirichletProcess::DirichletProcess() :
n(0), alpha(1.0), verbose(true), mhDraws(250), numberClusters(0) { // Sensible defaults
  // alphaPriorParameters will be default constructed (e.g., R_NilValue for Rcpp::RObject)
  // clusterParameters will be an empty Rcpp::List
  // rObject will be R_NilValue
  // arma::mat data, arma::uvec clusterLabels, arma::uvec pointsPerCluster are default constructed
}

// Constructor from R SEXP
DirichletProcess::DirichletProcess(SEXP r_dpObj) : rObject(r_dpObj) { // Store the SEXP
  Rcpp::List dpList(r_dpObj); // Convert SEXP to Rcpp::List for easier access

  // Initialize members from the R list
  if (dpList.containsElementNamed("data")) {
    this->data = Rcpp::as<arma::mat>(dpList["data"]);
    this->n = this->data.n_rows;
  } else {
    this->data = arma::mat(); // Empty matrix
    this->n = 0;
    // Rcpp::warning("'data' not found in R DP object during C++ construction."); // Optional warning
  }

  if (dpList.containsElementNamed("alpha")) {
    this->alpha = Rcpp::as<double>(dpList["alpha"]);
  } else {
    this->alpha = 1.0; // Default alpha
  }

  if (dpList.containsElementNamed("alphaPriorParameters")) {
    this->alphaPriorParameters = dpList["alphaPriorParameters"];
  } else {
    this->alphaPriorParameters = Rcpp::NumericVector::create(1.0, 1.0); // Default e.g. Gamma(1,1)
  }

  if (dpList.containsElementNamed("verbose")) {
    this->verbose = Rcpp::as<bool>(dpList["verbose"]);
  } else {
    this->verbose = false; // Default verbose
  }

  if (dpList.containsElementNamed("mhDraws")) {
    this->mhDraws = Rcpp::as<int>(dpList["mhDraws"]);
  } else {
    this->mhDraws = 250; // Default mhDraws
  }

  // Initialize cluster-related members
  if (dpList.containsElementNamed("clusterLabels")) {
    Rcpp::IntegerVector r_labels = Rcpp::as<Rcpp::IntegerVector>(dpList["clusterLabels"]);
    if (r_labels.size() > 0) {
      this->clusterLabels = Rcpp::as<arma::uvec>(r_labels) - 1; // Convert R (1-indexed) to C++ (0-indexed)
    } else {
      this->clusterLabels = arma::uvec(); // Empty uvec
    }
  } else {
    if (this->n > 0) this->clusterLabels = arma::zeros<arma::uvec>(this->n); // Default: all in cluster 0
    else this->clusterLabels = arma::uvec();
  }

  if (dpList.containsElementNamed("pointsPerCluster")) {
    this->pointsPerCluster = Rcpp::as<arma::uvec>(dpList["pointsPerCluster"]);
  } else {
    // Default: calculate from clusterLabels if possible
    if (this->n > 0 && this->clusterLabels.n_elem == (unsigned int)this->n) {
      if (this->clusterLabels.n_elem > 0) {
        arma::uword max_label_val = 0;
        if (this->clusterLabels.is_empty() == false) {
          max_label_val = this->clusterLabels.max();
        }
        this->pointsPerCluster.zeros(max_label_val + 1); // Size for 0-indexed labels
        for(arma::uword i = 0; i < (unsigned int)this->n; ++i) {
          if (this->clusterLabels(i) < this->pointsPerCluster.n_elem) { // Boundary check
            this->pointsPerCluster(this->clusterLabels(i))++;
          }
        }
      } else {
        this->pointsPerCluster = arma::uvec();
      }
    } else {
      this->pointsPerCluster = arma::uvec();
    }
  }

  if (dpList.containsElementNamed("numberClusters")) {
    this->numberClusters = Rcpp::as<int>(dpList["numberClusters"]);
  } else {
    // Default: calculate from pointsPerCluster or clusterLabels
    if (this->pointsPerCluster.n_elem > 0) {
      this->numberClusters = arma::accu(this->pointsPerCluster > 0); // Count non-empty clusters
    } else if (this->clusterLabels.n_elem > 0) {
      arma::uvec unique_labels_vec = arma::unique(this->clusterLabels);
      this->numberClusters = unique_labels_vec.n_elem;
    } else {
      this->numberClusters = 0;
    }
  }

  if (dpList.containsElementNamed("clusterParameters")) {
    this->clusterParameters = Rcpp::as<Rcpp::List>(dpList["clusterParameters"]);
  } else {
    this->clusterParameters = Rcpp::List(); // Empty list
  }
}


DirichletProcess::~DirichletProcess() {
  // Destructor
}

Rcpp::List DirichletProcess::toR() const {
  Rcpp::List result;
  result["data"] = Rcpp::wrap(data);
  result["n"] = n;
  result["alpha"] = alpha;
  result["alphaPriorParameters"] = alphaPriorParameters;
  result["verbose"] = verbose;
  result["mhDraws"] = mhDraws;
  if (clusterLabels.n_elem > 0) {
    result["clusterLabels"] = Rcpp::wrap(clusterLabels + 1); // Convert 0-indexed C++ to 1-indexed R
  } else {
    result["clusterLabels"] = Rcpp::IntegerVector(0);
  }
  result["pointsPerCluster"] = Rcpp::wrap(pointsPerCluster);
  result["numberClusters"] = numberClusters;
  result["clusterParameters"] = clusterParameters;
  // result["rObject"] = rObject; // Be cautious about SEXP lifecycle if returning stored SEXP

  result.attr("class") = Rcpp::CharacterVector::create("list", "dirichletprocess");
  return result; // **ADDED MISSING RETURN STATEMENT**
} // **ADDED MISSING CLOSING BRACE FOR toR()**

void DirichletProcess::fit(int iterations, bool use_progress_bar) {
  if (this->verbose && use_progress_bar) {
    Rcpp::Rcout << "Starting MCMC fitting..." << std::endl;
  }
  for (int i = 0; i < iterations; ++i) {
    this->clusterComponentUpdate();
    this->clusterParameterUpdate();
    this->updateAlpha();
    this->updateG0();

    if (this->verbose && use_progress_bar && (iterations <= 10 || (i + 1) % (iterations / 10) == 0 || i == iterations - 1) ) {
      Rcpp::Rcout << "Iteration: " << i + 1 << "/" << iterations << " Number of Clusters: " << this->numberClusters << std::endl;
    }
  }
  if (this->verbose && use_progress_bar) {
    Rcpp::Rcout << "MCMC fitting complete." << std::endl;
  }
}


MixingDistribution::MixingDistribution() : conjugate(true) {
  // Rcpp::RObject members are default initialized
}

MixingDistribution::~MixingDistribution() {
  // Destructor
}

Rcpp::List MixingDistribution::toR() const {
  Rcpp::List result;
  result["distribution"] = distribution;
  result["priorParameters"] = priorParameters;
  result["conjugate"] = conjugate;
  if (mhStepSize.sexp_type() != NILSXP) {
    result["mhStepSize"] = mhStepSize;
  }
  if (hyperPriorParameters.sexp_type() != NILSXP) {
    result["hyperPriorParameters"] = hyperPriorParameters;
  }

  Rcpp::CharacterVector classes = Rcpp::CharacterVector::create(
    "list", "MixingDistribution");
  if (!distribution.empty()){ // Ensure distribution string is not empty before adding
    classes.push_back(distribution);
  }
  classes.push_back(conjugate ? "conjugate" : "nonconjugate");
  result.attr("class") = classes;

  return result;
}

} // namespace dp **ENSURED NAMESPACE IS CLOSED**
