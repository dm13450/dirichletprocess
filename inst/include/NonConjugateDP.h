// inst/include/NonConjugateDP.h
#ifndef NONCONJUGATE_DP_H
#define NONCONJUGATE_DP_H

#include "dirichletprocess.h"

namespace dp {

class NonConjugateDP : public DirichletProcess {
public:
  NonConjugateDP();
  virtual ~NonConjugateDP();

  int m; // Number of auxiliary parameters

  // Implementation of core MCMC methods for non-conjugate case
  void clusterComponentUpdate() override;
  void clusterParameterUpdate() override;

  // Non-conjugate specific methods
  Rcpp::List metropolisHastings(const arma::mat& x, const Rcpp::List& startPos, int noDraws);
};

} // namespace dp

#endif
