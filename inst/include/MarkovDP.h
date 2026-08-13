// inst/include/MarkovDP.h
#ifndef MARKOV_DP_H
#define MARKOV_DP_H

#include "DirichletProcessBase.h"
#include "NormalDistribution.h"
#include <vector>

namespace dp {

class MarkovDP : public DirichletProcess {
public:
  MarkovDP();
  MarkovDP(const Rcpp::List& rObj); // Constructor from R object
  virtual ~MarkovDP();

  // Markov DP specific attributes
  arma::uvec states;                  // State sequence
  Rcpp::List uniqueParams;            // Unique state parameters
  std::vector<Rcpp::List> params;    // Parameters for each state
  double beta;                        // Transition concentration parameter
  MixingDistribution* mixingDistribution; // The mixing distribution object

  // Chain storage
  Rcpp::NumericVector alphaChain;
  Rcpp::NumericVector betaChain;
  Rcpp::List statesChain;
  Rcpp::List paramChain;

  // Implementation of core MCMC methods for Markov case
  void clusterComponentUpdate() override { updateStates(); }
  void clusterParameterUpdate() override { paramUpdate(); }
  void updateAlpha() override { updateAlphaBeta(); }

  // Markov specific methods
  void updateStates();
  void updateAlphaBeta();
  void paramUpdate();

  // Fitting method
  using DirichletProcess::fit;  // Bring base class fit() into scope to avoid hiding it
  void fit(int iterations, bool updatePrior = false, bool progressBar = true);

  // Helper methods
  arma::uvec relabelStates(const arma::uvec& dpStates);
  double alphabetaLogPosterior(double alpha, double beta, const arma::vec& nii);

  // NEW: Helper methods for parameter validation
  Rcpp::List getValidatedParams(int idx);
  Rcpp::List getDefaultNormalParams();

  // Conversion methods
  Rcpp::List toR() const override;
  static MarkovDP* fromR(const Rcpp::List& rObj);

  // Override getMixingDistribution
  MixingDistribution* getMixingDistribution() override { return mixingDistribution; }
};

} // namespace dp

#endif
