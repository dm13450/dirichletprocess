// inst/include/utilities.h
#ifndef DIRICHLETPROCESS_UTILITIES_H
#define DIRICHLETPROCESS_UTILITIES_H

#include <RcppArmadillo.h>

namespace dirichletprocess {

// Function to sample from a categorical distribution
inline int sample_categorical(const arma::vec& probs) {
  double u = R::runif(0, 1);
  double cumsum = 0.0;

  for (arma::uword i = 0; i < probs.n_elem; ++i) {
    cumsum += probs[i];
    if (u <= cumsum) {
      return i;
    }
  }

  return probs.n_elem - 1;
}

} // namespace dirichletprocess

#endif
