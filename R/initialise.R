#' Initialise a Dirichlet process object
#'
#' Initialise a Dirichlet process object by assigning all the data points to a single cluster with a posterior or prior draw for parameters.
#'
#' @param dpObj A Dirichlet process object.
#' @param posterior TRUE/FALSE value for whether the cluster parameters should be from the posterior. If false then the values are from the prior.
#' @param m Number of auxiliary variables to use for a non-conjugate mixing distribution. Defaults to m=3. See \code{\link{ClusterComponentUpdate}} for more details on m.
#' @param verbose Logical flag indicating whether to output the acceptance ratio for non-conjugate mixtures.
#' @return A Dirichlet process object that has initial cluster allocations.
#' @export
Initialise <- function(dpObj, posterior = TRUE, m=3, verbose=TRUE){
  UseMethod("Initialise", dpObj)
}

#' @export
Initialise.conjugate <- function(dpObj, posterior = TRUE, m=NULL, verbose=NULL) {

  dpObj$clusterLabels <- rep_len(1, dpObj$n)
  dpObj$numberClusters <- 1
  dpObj$pointsPerCluster <- dpObj$n

  if (posterior) {
    dpObj$clusterParameters <- PosteriorDraw(dpObj$mixingDistribution, dpObj$data, 1)
  } else {
    dpObj$clusterParameters <- PriorDraw(dpObj$mixingDistribution, 1)
  }

  dpObj <- InitialisePredictive(dpObj)

  return(dpObj)
}

#'@export
Initialise.nonconjugate <- function(dpObj, posterior = TRUE, m = 3, verbose = TRUE) {

  # dpObj$clusterLabels <- 1:dpObj$n dpObj$numberClusters <- dpObj$n
  # dpObj$pointsPerCluster <- rep(1, dpObj$n) dpObj$clusterParameters <-
  # PosteriorDraw(dpObj$MixingDistribution, dpObj$data, dpObj$n)
  dpObj$clusterLabels <- rep(1, dpObj$n)
  dpObj$numberClusters <- 1
  dpObj$pointsPerCluster <- dpObj$n

  if (posterior) {
    post_draws <- PosteriorDraw(dpObj$mixingDistribution, dpObj$data, 1000)

    if (verbose)
      cat(paste("Accept Ratio: ",
                length(unique(c(post_draws[[1]])))/1000,
                "\n"))

    dpObj$clusterParameters <- list(post_draws[[1]][, , 1000, drop = FALSE],
                                    post_draws[[2]][, , 1000, drop = FALSE])
  } else {
    dpObj$clusterParameters <- PriorDraw(dpObj$mixingDistribution, 1)
  }

  dpObj$m <- m

  return(dpObj)
}


InitialisePredictive <- function(dpObj) UseMethod("InitialisePredictive", dpObj)

InitialisePredictive.conjugate <- function(dpObj) {

  dpObj$predictiveArray <- Predictive(dpObj$mixingDistribution, dpObj$data)

  return(dpObj)
}

InitialisePredictive.nonconjugate <- function(dpObj) {
  return(dpObj)
}



