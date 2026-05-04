// inst/include/HierarchicalDP.h
#ifndef HIERARCHICAL_DP_H
#define HIERARCHICAL_DP_H

#include "DirichletProcessBase.h"
#include <vector>
#include <RcppArmadillo.h> // Ensure Rcpp types like List and NumericVector are available

namespace dp {

class HierarchicalDP : public DirichletProcess {
public:
  HierarchicalDP();
  virtual ~HierarchicalDP();

  // Hierarchical DP specific attributes
  std::vector<DirichletProcess*> indDP;
  Rcpp::List globalParameters;
  arma::vec globalStick;
  double gamma;
  Rcpp::NumericVector gammaPriors;
  Rcpp::NumericVector gammaChain;

  // Implementation of core MCMC methods for hierarchical case
  // These override methods from DirichletProcess (which should be virtual)
  void clusterComponentUpdate() override;
  void clusterParameterUpdate() override;
  void updateAlpha() override;

  // Hierarchical specific methods (these should be virtual if intended to be overridden by further derived classes)
  virtual void globalParameterUpdate();
  virtual void updateG0() override;
  virtual void updateGamma();

  // Conversion methods
  Rcpp::List toR() const override; // Overrides from DirichletProcess
  static HierarchicalDP* fromR(const Rcpp::List& rObj);
};

// Specialized classes for specific distribution types
class HierarchicalBetaDP : public HierarchicalDP {
public:
  HierarchicalBetaDP();
  virtual ~HierarchicalBetaDP();

  // Unhide base class fit method from DirichletProcess
  using DirichletProcess::fit;

  // Additional methods specific to Beta hierarchical DP
  void fit(int iterations, bool updatePrior = false, bool progressBar = true);

  // Add these override declarations:
  void globalParameterUpdate() override;
  void updateG0() override;
  void updateGamma() override;
  void clusterComponentUpdate() override;
  void clusterParameterUpdate() override;
  void updateAlpha() override;
  Rcpp::List toR() const override;


  static HierarchicalBetaDP* fromR(const Rcpp::List& rObj);
};

class HierarchicalMVNormal2DP : public HierarchicalDP {
public:
  HierarchicalMVNormal2DP();
  virtual ~HierarchicalMVNormal2DP();

  // Unhide base class fit method
  using DirichletProcess::fit;

  // Additional methods specific to MVNormal2 hierarchical DP
  void fit(int iterations, bool updatePrior = false, bool progressBar = true);
  void clusterComponentUpdate() override; // <<< Added this line
  void globalParameterUpdate() override;
  void updateG0() override;
  void updateGamma() override;
  static HierarchicalMVNormal2DP* fromR(const Rcpp::List& rObj);
};

} // namespace dp

#endif
