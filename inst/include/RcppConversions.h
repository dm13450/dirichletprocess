// inst/include/RcppConversions.h
#ifndef RCPP_CONVERSIONS_H
#define RCPP_CONVERSIONS_H

#include <RcppArmadillo.h>
#include <memory>
#include "dirichletprocess.h"

namespace dp {

// Convert Rcpp types to C++ types
arma::mat convertMatrix(const Rcpp::NumericMatrix& rMatrix);
arma::vec convertVector(const Rcpp::NumericVector& rVector);
arma::cube convertArray(const Rcpp::NumericVector& rArray, const Rcpp::IntegerVector& dims);

// Convert between R lists and C++ objects
Rcpp::List clusterParametersToR(const std::vector<arma::cube>& params);
std::vector<arma::cube> clusterParametersFromR(const Rcpp::List& rParams);

// S3 class detection
bool isConjugate(const Rcpp::List& dpObj);
bool isNonConjugate(const Rcpp::List& dpObj);
bool isHierarchical(const Rcpp::List& dpObj);
bool isMarkov(const Rcpp::List& dpObj);
std::string getDistributionType(const Rcpp::List& dpObj);

// Factory functions to create appropriate C++ objects from R objects
std::unique_ptr<DirichletProcess> createDPFromR(const Rcpp::List& rObj);
MixingDistribution* createMDFromR(const Rcpp::List& rObj);

} // namespace dp

#endif
