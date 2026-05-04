// src/HierarchicalDP.cpp
#include "HierarchicalDP.h"
#include "BetaDistribution.h"
#include "MVNormal2Distribution.h"
#include "RcppConversions.h"
#include <RcppArmadillo.h>

namespace dp {

// HierarchicalDP base class implementation
HierarchicalDP::HierarchicalDP() : gamma(1.0) {
  // Constructor
}

HierarchicalDP::~HierarchicalDP() {
  // Destructor - cleanup is handled in derived classes
}

void HierarchicalDP::clusterComponentUpdate() {
  // Update components for each individual DP
  for (auto& dp : indDP) {
    if (dp) {
      dp->clusterComponentUpdate();
    }
  }
}

void HierarchicalDP::clusterParameterUpdate() {
  // Update parameters for each individual DP
  for (auto& dp : indDP) {
    if (dp) {
      dp->clusterParameterUpdate();
    }
  }
}

void HierarchicalDP::updateAlpha() {
  // Update alpha for each individual DP
  for (auto& dp : indDP) {
    if (dp) {
      dp->updateAlpha();
    }
  }
}

void HierarchicalDP::globalParameterUpdate() {
  // This is a base implementation that can be overridden
  // For now, just a placeholder
  Rcpp::warning("Base HierarchicalDP::globalParameterUpdate called - should be overridden");
}

void HierarchicalDP::updateG0() {
  // This is a base implementation that can be overridden
  // For now, just a placeholder
  Rcpp::warning("Base HierarchicalDP::updateG0 called - should be overridden");
}

void HierarchicalDP::updateGamma() {
  // This is a base implementation that can be overridden
  // For now, just a placeholder
  Rcpp::warning("Base HierarchicalDP::updateGamma called - should be overridden");
}

Rcpp::List HierarchicalDP::toR() const {
  Rcpp::List result;

  // Convert individual DPs back to R format
  Rcpp::List indDP_list(indDP.size());
  for (size_t i = 0; i < indDP.size(); i++) {
    if (indDP[i]) {
      Rcpp::List dp_r = indDP[i]->toR();

      // Convert 0-indexed labels back to 1-indexed for R
      if (dp_r.containsElementNamed("clusterLabels")) {
        arma::uvec labels = Rcpp::as<arma::uvec>(dp_r["clusterLabels"]);
        dp_r["clusterLabels"] = labels + 1;
      }

      // Preserve S3 class attributes for individual DPs
      dp_r.attr("class") = Rcpp::CharacterVector::create("dirichletprocess", "beta", "nonconjugate");

      indDP_list[i] = dp_r;
    }
  }

  result["indDP"] = indDP_list;
  result["globalParameters"] = globalParameters;
  result["globalStick"] = globalStick;
  result["gamma"] = gamma;
  result["gammaPriors"] = gammaPriors;

  // Add gamma chain if available
  if (gammaChain.size() > 0) {
    result["gammaValues"] = gammaChain;
  }

  // Set S3 class for the hierarchical object
  result.attr("class") = Rcpp::CharacterVector::create("list", "dirichletprocess", "hierarchical");

  return result;
}

HierarchicalDP* HierarchicalDP::fromR(const Rcpp::List& rObj) {
  // This is a static factory method - derived classes should implement their own
  Rcpp::stop("Base HierarchicalDP::fromR called - use derived class implementations");
  return nullptr;
}

} // namespace dp
