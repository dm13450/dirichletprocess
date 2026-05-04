// src/RcppConversions.cpp
#include "RcppConversions.h"
#include "NormalDistribution.h"
#include "BetaDistribution.h"
#include "MVNormalDistribution.h"
#include "MVNormal2Distribution.h"
#include "WeibullDistribution.h"
#include "ExponentialDistribution.h"
#include "HierarchicalDP.h"
#include "MarkovDP.h"

namespace dp {

arma::mat convertMatrix(const Rcpp::NumericMatrix& rMatrix) {
  // Use the RcppArmadillo conversion mechanism
  return Rcpp::as<arma::mat>(rMatrix);
}

arma::vec convertVector(const Rcpp::NumericVector& rVector) {
  // Use the RcppArmadillo conversion mechanism
  return Rcpp::as<arma::vec>(rVector);
}

arma::cube convertArray(const Rcpp::NumericVector& rArray, const Rcpp::IntegerVector& dims) {
  if (dims.size() != 3) {
    Rcpp::stop("Array must be 3-dimensional");
  }

  // Create cube with specified dimensions
  arma::cube result(dims[0], dims[1], dims[2]);

  // Copy data
  std::copy(rArray.begin(), rArray.end(), result.memptr());

  return result;
}

Rcpp::List clusterParametersToR(const std::vector<arma::cube>& params) {
  Rcpp::List result(params.size());
  for (size_t i = 0; i < params.size(); i++) {
    Rcpp::NumericVector array(params[i].memptr(), params[i].memptr() + params[i].n_elem);
    array.attr("dim") = Rcpp::IntegerVector::create(params[i].n_rows, params[i].n_cols, params[i].n_slices);
    result[i] = array;
  }
  return result;
}

std::vector<arma::cube> clusterParametersFromR(const Rcpp::List& rParams) {
  std::vector<arma::cube> result(rParams.size());
  for (int i = 0; i < rParams.size(); i++) {
    Rcpp::NumericVector array = rParams[i];
    if (!array.hasAttribute("dim")) {
      Rcpp::stop("Parameter array must have dim attribute");
    }
    Rcpp::IntegerVector dims = array.attr("dim");
    if (dims.size() != 3) {
      Rcpp::stop("Parameter array must be 3-dimensional");
    }

    // Create cube and copy data
    arma::cube cube(dims[0], dims[1], dims[2]);
    std::copy(array.begin(), array.end(), cube.memptr());
    result[i] = cube;
  }
  return result;
}

bool isConjugate(const Rcpp::List& dpObj) {
  if (!dpObj.hasAttribute("class")) {
    return false;
  }
  Rcpp::CharacterVector classes = dpObj.attr("class");
  for (int i = 0; i < classes.size(); i++) {
    if (Rcpp::as<std::string>(classes[i]) == "conjugate") {
      return true;
    }
  }
  return false;
}

bool isNonConjugate(const Rcpp::List& dpObj) {
  if (!dpObj.hasAttribute("class")) {
    return false;
  }
  Rcpp::CharacterVector classes = dpObj.attr("class");
  for (int i = 0; i < classes.size(); i++) {
    if (Rcpp::as<std::string>(classes[i]) == "nonconjugate") {
      return true;
    }
  }
  return false;
}

bool isHierarchical(const Rcpp::List& dpObj) {
  if (!dpObj.hasAttribute("class")) {
    return false;
  }
  Rcpp::CharacterVector classes = dpObj.attr("class");
  for (int i = 0; i < classes.size(); i++) {
    if (Rcpp::as<std::string>(classes[i]) == "hierarchical") {
      return true;
    }
  }
  return false;
}

bool isMarkov(const Rcpp::List& dpObj) {
  if (!dpObj.hasAttribute("class")) {
    return false;
  }
  Rcpp::CharacterVector classes = dpObj.attr("class");
  for (int i = 0; i < classes.size(); i++) {
    if (Rcpp::as<std::string>(classes[i]) == "markov") {
      return true;
    }
  }
  return false;
}

std::string getDistributionType(const Rcpp::List& dpObj) {
  if (!dpObj.hasAttribute("class")) {
    Rcpp::stop("Object has no class attribute");
  }
  Rcpp::CharacterVector classes = dpObj.attr("class");
  for (int i = 0; i < classes.size(); i++) {
    std::string cls = Rcpp::as<std::string>(classes[i]);
    if (cls == "normal" || cls == "beta" || cls == "mvnormal" ||
        cls == "mvnormal2" || cls == "weibull" || cls == "exponential") {
      return cls;
    }
  }
  // Default fallback
  return "normal";
}

std::unique_ptr<DirichletProcess> createDPFromR(const Rcpp::List& rObj) {
  // Create base DirichletProcess object using smart pointer
  std::unique_ptr<DirichletProcess> dp(new DirichletProcess());

  try {
    if (dp && rObj.containsElementNamed("data") && rObj.containsElementNamed("n")
          && rObj.containsElementNamed("alpha") && rObj.containsElementNamed("alphaPriorParameters")) {

      // Convert data safely
      Rcpp::NumericMatrix dataMatrix;
      if (Rcpp::is<Rcpp::NumericMatrix>(rObj["data"])) {
        dataMatrix = Rcpp::as<Rcpp::NumericMatrix>(rObj["data"]);
      } else if (Rcpp::is<Rcpp::NumericVector>(rObj["data"])) {
        Rcpp::NumericVector dataVec = Rcpp::as<Rcpp::NumericVector>(rObj["data"]);
        dataMatrix = Rcpp::NumericMatrix(dataVec.size(), 1, dataVec.begin());
      } else {
        Rcpp::stop("Data must be numeric matrix or vector");
      }

      // Set common properties
      dp->data = convertMatrix(dataMatrix);
      dp->n = Rcpp::as<int>(rObj["n"]);
      dp->alpha = Rcpp::as<double>(rObj["alpha"]);
      dp->alphaPriorParameters = rObj["alphaPriorParameters"];
    }
  } catch (const std::exception& e) {
    Rcpp::stop("Failed to create DirichletProcess: %s", e.what());
  }

  return dp;
}

MixingDistribution* createMDFromR(const Rcpp::List& rObj) {
  if (!rObj.containsElementNamed("distribution")) {
    Rcpp::stop("Mixing distribution object must have 'distribution' field");
  }

  std::string distType = Rcpp::as<std::string>(rObj["distribution"]);

  MixingDistribution* md = nullptr;

  if (distType == "normal") {
    if (rObj.containsElementNamed("priorParameters")) {
      // Explicitly cast to avoid constructor ambiguity
      Rcpp::NumericVector priorParams = Rcpp::as<Rcpp::NumericVector>(rObj["priorParameters"]);
      md = new NormalMixingDistribution(priorParams);
    }
  } else if (distType == "beta") {
    if (rObj.containsElementNamed("priorParameters")) {
      Rcpp::NumericVector priorParams = Rcpp::as<Rcpp::NumericVector>(rObj["priorParameters"]);
      md = new BetaMixingDistribution(priorParams);
      if (rObj.containsElementNamed("maxT")) {
        dynamic_cast<BetaMixingDistribution*>(md)->maxT =
          Rcpp::as<double>(rObj["maxT"]);
      }
    }
  } else if (distType == "mvnormal") {
    if (rObj.containsElementNamed("priorParameters")) {
      Rcpp::List priorParams = Rcpp::as<Rcpp::List>(rObj["priorParameters"]);
      md = new MVNormalMixingDistribution(priorParams);
    }
  } else if (distType == "mvnormal2") {
    if (rObj.containsElementNamed("priorParameters")) {
      Rcpp::List priorParams = Rcpp::as<Rcpp::List>(rObj["priorParameters"]);
      md = new MVNormal2MixingDistribution(priorParams);
    }
  } else if (distType == "weibull") {
    if (rObj.containsElementNamed("priorParameters") &&
        rObj.containsElementNamed("mhStepSize")) {
      Rcpp::NumericVector priorParams = Rcpp::as<Rcpp::NumericVector>(rObj["priorParameters"]);
      Rcpp::NumericVector mhStep = Rcpp::as<Rcpp::NumericVector>(rObj["mhStepSize"]);
      Rcpp::NumericVector hyperPrior = rObj.containsElementNamed("hyperPriorParameters") ?
      Rcpp::as<Rcpp::NumericVector>(rObj["hyperPriorParameters"]) :
        Rcpp::NumericVector::create();

      md = new WeibullMixingDistribution(priorParams, mhStep, hyperPrior);
    }
  } else if (distType == "exponential") {
    if (rObj.containsElementNamed("priorParameters")) {
      Rcpp::NumericVector priorParams = Rcpp::as<Rcpp::NumericVector>(rObj["priorParameters"]);
      md = new ExponentialMixingDistribution(priorParams);
    }
  }

  if (md) {
    md->distribution = distType;

    // Updated logic for handling the 'conjugate' field
    if (rObj.containsElementNamed("conjugate")) {
      SEXP conjugate_sexp = rObj["conjugate"]; // Get the SEXP for the 'conjugate' field

      if (TYPEOF(conjugate_sexp) == STRSXP) { // Check if it's a string
        Rcpp::CharacterVector conjugate_r_str_vec(conjugate_sexp);
        if (conjugate_r_str_vec.length() > 0) {
          std::string conjugate_str = Rcpp::as<std::string>(conjugate_r_str_vec[0]);
          if (conjugate_str == "conjugate") {
            md->conjugate = true;
          } else if (conjugate_str == "nonconjugate") {
            md->conjugate = false;
          } else {
            // Unknown string: issue a warning and default, or stop with an error
            Rcpp::warning("Unknown string value '%s' for 'conjugate' field in MixingDistribution. Defaulting to false for C++ object.", conjugate_str.c_str());
            md->conjugate = false;
          }
        } else {
          Rcpp::warning("Empty string vector for 'conjugate' field in MixingDistribution. Defaulting to false for C++ object.");
          md->conjugate = false;
        }
      } else if (TYPEOF(conjugate_sexp) == LGLSXP) { // Optionally handle if it's already boolean
        md->conjugate = Rcpp::as<bool>(conjugate_sexp);
      } else {
        // Type is neither string nor logical
        Rcpp::stop("MixingDistribution 'conjugate' parameter from R must be a string ('conjugate'/'nonconjugate') or a logical value. Received type: %s", Rf_type2char(TYPEOF(conjugate_sexp)));
      }
    } else {
      // If 'conjugate' field is missing entirely. This should ideally be caught by R-side checks.
      Rcpp::warning("'conjugate' field is missing in R object for MixingDistribution. Defaulting C++ 'conjugate' flag to false.");
      md->conjugate = false; // Default or error
    }

    if (rObj.containsElementNamed("priorParameters")) {
      md->priorParameters = rObj["priorParameters"];
    }

    if (rObj.containsElementNamed("mhStepSize")) {
      md->mhStepSize = rObj["mhStepSize"];
    }

    if (rObj.containsElementNamed("hyperPriorParameters")) {
      md->hyperPriorParameters = rObj["hyperPriorParameters"];
    }
  }

  return md;
}

} // namespace dp
