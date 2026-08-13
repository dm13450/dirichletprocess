// inst/include/ConjugateDP.h
#ifndef CONJUGATE_DP_H
#define CONJUGATE_DP_H

#include "dirichletprocess.h"

namespace dp {

class ConjugateDP : public DirichletProcess {
public:
  ConjugateDP();
  virtual ~ConjugateDP();

  // Cluster information
  arma::uvec clusterLabels;
  arma::uvec pointsPerCluster;
  int numberClusters;
  Rcpp::List clusterParameters;
  arma::vec predictiveArray;

  // Implementation of core MCMC methods for conjugate case
  void clusterComponentUpdate() override;
  void clusterParameterUpdate() override;

  // Conjugate specific methods
  Rcpp::List clusterLabelChange(int i, int newLabel, int currentLabel);
  void initialisePredictive();
};

} // namespace dp

#endif
