// likelihood_functions.h
#ifndef LIKELIHOOD_FUNCTIONS_H
#define LIKELIHOOD_FUNCTIONS_H

#include <RcppArmadillo.h>

// Function declarations
Rcpp::NumericVector likelihood_normal_cpp(
    Rcpp::List mdObj,
    Rcpp::NumericVector x,
    Rcpp::List theta);

#endif // LIKELIHOOD_FUNCTIONS_H
