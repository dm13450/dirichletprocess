// src/BetaDP.cpp
#include "BetaDistribution.h"
#include "RcppConversions.h"

namespace dp {

// NonConjugateBetaDP constructor and destructor
NonConjugateBetaDP::NonConjugateBetaDP() : mixingDistribution(nullptr), numberClusters(0), m(3) {
  // Constructor
}

NonConjugateBetaDP::~NonConjugateBetaDP() {
  // No manual delete needed - unique_ptr handles it
}

void NonConjugateBetaDP::clusterComponentUpdate() {
  int n = data.n_rows;

  for (int i = 0; i < n; i++) {
    int currentLabel = clusterLabels[i];

    // Validate currentLabel
    if (currentLabel >= numberClusters) {
      Rcpp::stop("Invalid cluster label encountered");
    }

    // Remove point from current cluster temporarily
    pointsPerCluster[currentLabel]--;

    // Generate auxiliary parameters
    Rcpp::List aux;
    bool currentClusterEmpty = (pointsPerCluster[currentLabel] == 0);

    if (currentClusterEmpty) {
      // Current cluster is empty, include it as auxiliary
      aux = mixingDistribution->priorDraw(m - 1);

      // Include current cluster params as first auxiliary
      Rcpp::NumericVector mu_vec = Rcpp::as<Rcpp::NumericVector>(clusterParameters[0]);
      Rcpp::NumericVector nu_vec = Rcpp::as<Rcpp::NumericVector>(clusterParameters[1]);

      Rcpp::NumericVector mu_aux = aux[0];
      Rcpp::NumericVector nu_aux = aux[1];

      Rcpp::NumericVector mu_combined(m);
      Rcpp::NumericVector nu_combined(m);

      mu_combined[0] = mu_vec[currentLabel];
      nu_combined[0] = nu_vec[currentLabel];

      for (int j = 1; j < m; j++) {
        mu_combined[j] = mu_aux[j-1];
        nu_combined[j] = nu_aux[j-1];
      }

      mu_combined.attr("dim") = Rcpp::IntegerVector::create(1, 1, m);
      nu_combined.attr("dim") = Rcpp::IntegerVector::create(1, 1, m);

      aux = Rcpp::List::create(mu_combined, nu_combined);
    } else {
      // Generate m auxiliary parameters
      aux = mixingDistribution->priorDraw(m);
    }

    // Calculate probabilities
    Rcpp::NumericVector probs(numberClusters + m);

    // Existing clusters
    for (int j = 0; j < numberClusters; j++) {
      if (j == currentLabel && currentClusterEmpty) {
        // Skip probability calculation for empty current cluster
        probs[j] = 0.0;
      } else {
        Rcpp::NumericVector mu_vec = clusterParameters[0];
        Rcpp::NumericVector nu_vec = clusterParameters[1];

        Rcpp::List theta = Rcpp::List::create(
          Rcpp::Named("mu") = mu_vec[j],
                                    Rcpp::Named("nu") = nu_vec[j]
        );

        arma::vec data_point = data.row(i).t();
        Rcpp::NumericVector lik = mixingDistribution->likelihood(data_point, theta);

        if (j == currentLabel && !currentClusterEmpty) {
          // n-i,c in the paper, already decremented
          probs[j] = pointsPerCluster[j] * lik[0];
        } else {
          // n-i,c for other clusters
          probs[j] = pointsPerCluster[j] * lik[0];
        }
      }
    }

    // Auxiliary parameters
    for (int j = 0; j < m; j++) {
      Rcpp::NumericVector mu_aux = aux[0];
      Rcpp::NumericVector nu_aux = aux[1];

      Rcpp::List theta_aux = Rcpp::List::create(
        Rcpp::Named("mu") = mu_aux[j],
                                  Rcpp::Named("nu") = nu_aux[j]
      );

      arma::vec data_point = data.row(i).t();
      Rcpp::NumericVector lik = mixingDistribution->likelihood(data_point, theta_aux);
      probs[numberClusters + j] = (alpha / m) * lik[0];
    }

    // Sample new label
    int newLabel = 0;
    double probSum = Rcpp::sum(probs);

    if (probSum <= 0) {
      // If all probabilities are zero, assign uniformly
      newLabel = R::runif(0, 1) * (numberClusters + m);
    } else {
      // Normalize and sample
      probs = probs / probSum;
      double u = R::runif(0, 1);
      double cumsum = 0.0;

      for (int j = 0; j < numberClusters + m; j++) {
        cumsum += probs[j];
        if (u <= cumsum) {
          newLabel = j;
          break;
        }
      }
    }

    // Update cluster assignment
    Rcpp::List updateResult = clusterLabelChange(i, newLabel, currentLabel, aux);

    // Update state from result
    clusterLabels = Rcpp::as<arma::uvec>(updateResult["clusterLabels"]);
    pointsPerCluster = Rcpp::as<arma::uvec>(updateResult["pointsPerCluster"]);
    clusterParameters = updateResult["clusterParameters"];
    numberClusters = updateResult["numberClusters"];
  }

  // Final validation
  arma::uword totalPoints = arma::sum(pointsPerCluster);
  if (totalPoints != static_cast<arma::uword>(n)) {
    Rcpp::stop("Point count mismatch after cluster component update: expected " +
      std::to_string(n) + " but got " + std::to_string(totalPoints));
  }
}

Rcpp::List NonConjugateBetaDP::clusterLabelChange(int i, int newLabel, int currentLabel,
                                                  const Rcpp::List& aux) {
  if (newLabel == currentLabel) {
    // Restore the point count since we temporarily removed it
    pointsPerCluster[currentLabel]++;
    return Rcpp::List::create(
      Rcpp::Named("clusterLabels") = clusterLabels,
      Rcpp::Named("pointsPerCluster") = pointsPerCluster,
      Rcpp::Named("clusterParameters") = clusterParameters,
      Rcpp::Named("numberClusters") = numberClusters
    );
  }

  // Extract current parameters
  Rcpp::NumericVector mu_vec = Rcpp::clone(Rcpp::as<Rcpp::NumericVector>(clusterParameters[0]));
  Rcpp::NumericVector nu_vec = Rcpp::clone(Rcpp::as<Rcpp::NumericVector>(clusterParameters[1]));

  // Note: pointsPerCluster[currentLabel] has already been decremented in clusterComponentUpdate

  // Assign to new cluster
  if (newLabel < numberClusters) {
    // Existing cluster
    pointsPerCluster[newLabel]++;
    clusterLabels[i] = newLabel;

    // If old cluster is now empty, remove it
    if (pointsPerCluster[currentLabel] == 0) {
      numberClusters--;

      // Create new vectors without the empty cluster
      arma::uvec new_pointsPerCluster(numberClusters);
      Rcpp::NumericVector new_mu_vec(numberClusters);
      Rcpp::NumericVector new_nu_vec(numberClusters);

      int idx = 0;
      for (arma::uword j = 0; j < pointsPerCluster.n_elem; j++) {
        if (j != static_cast<arma::uword>(currentLabel)) {
          new_pointsPerCluster[idx] = pointsPerCluster[j];
          new_mu_vec[idx] = mu_vec[j];
          new_nu_vec[idx] = nu_vec[j];
          idx++;
        }
      }

      pointsPerCluster = new_pointsPerCluster;
      mu_vec = new_mu_vec;
      nu_vec = new_nu_vec;

      // Update ALL cluster labels (not just those > currentLabel)
      for (arma::uword j = 0; j < clusterLabels.n_elem; j++) {
        if ((int)clusterLabels[j] > currentLabel) {
          clusterLabels[j]--;
        }
      }

      // Important: Also update the label of the current point if needed
      if (newLabel > currentLabel) {
        clusterLabels[i] = newLabel - 1;
      }
    }
  } else {
    // New cluster from auxiliary parameters
    int auxIndex = newLabel - numberClusters;

    if (pointsPerCluster[currentLabel] == 0) {
      // Replace empty cluster with auxiliary
      Rcpp::NumericVector aux_mu = aux[0];
      Rcpp::NumericVector aux_nu = aux[1];

      mu_vec[currentLabel] = aux_mu[auxIndex];
      nu_vec[currentLabel] = aux_nu[auxIndex];

      pointsPerCluster[currentLabel] = 1;
      clusterLabels[i] = currentLabel;
    } else {
      // Create new cluster
      Rcpp::NumericVector aux_mu = aux[0];
      Rcpp::NumericVector aux_nu = aux[1];

      // Expand arrays
      Rcpp::NumericVector new_mu_vec(numberClusters + 1);
      Rcpp::NumericVector new_nu_vec(numberClusters + 1);

      for (int j = 0; j < numberClusters; j++) {
        new_mu_vec[j] = mu_vec[j];
        new_nu_vec[j] = nu_vec[j];
      }

      new_mu_vec[numberClusters] = aux_mu[auxIndex];
      new_nu_vec[numberClusters] = aux_nu[auxIndex];

      mu_vec = new_mu_vec;
      nu_vec = new_nu_vec;

      // Assign to new cluster
      clusterLabels[i] = numberClusters;

      // Resize pointsPerCluster correctly
      arma::uvec new_pointsPerCluster(numberClusters + 1);
      for (int j = 0; j < numberClusters; j++) {
        new_pointsPerCluster[j] = pointsPerCluster[j];
      }
      new_pointsPerCluster[numberClusters] = 1;
      pointsPerCluster = new_pointsPerCluster;

      numberClusters++;
    }
  }

  // Update cluster parameters
  clusterParameters[0] = mu_vec;
  clusterParameters[1] = nu_vec;

  // Final validation check
  bool valid = true;
  for (arma::uword j = 0; j < clusterLabels.n_elem; j++) {
    if ((int)clusterLabels[j] >= numberClusters || (int)clusterLabels[j] < 0) {
      valid = false;
      break;
    }
  }

  if (!valid) {
    Rcpp::stop("Invalid cluster labels after update");
  }

  return Rcpp::List::create(
    Rcpp::Named("clusterLabels") = clusterLabels,
    Rcpp::Named("pointsPerCluster") = pointsPerCluster,
    Rcpp::Named("clusterParameters") = clusterParameters,
    Rcpp::Named("numberClusters") = numberClusters
  );
}

} // namespace dp
