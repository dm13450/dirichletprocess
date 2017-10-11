#' Generate the posterior clusters of a Dirichlet Process
#'
#' Using the stick breaking representation the user can draw the posterior clusters and weights for a fitted Dirichlet Process.
#' See also \code{\link{PosteriorFunction}}.
#'
#' @param dpobj Fitted Dirichlet process
#' @param ind Index for which the posterior will be drawn from. Defaults to the last iteration of the fit.
#' @return A list with the weights and cluster parameters that form the posterior of the Dirichlet process.
#'
#' @examples
#' y <- rnorm(10)
#' dp <- DirichletProcessGaussian(y)
#' dp <- Fit(dp, 5)
#' postClusters <- PosteriorClusters(dp)
#'
#' @export
PosteriorClusters <- function(dpobj, ind) UseMethod("PosteriorClusters", dpobj)

#' @export
PosteriorClusters.dirichletprocess <- function(dpobj, ind) {

  if (!missing(ind)) {
    pointsPerCluster <- dpobj$weightsChain[[ind]]
    alpha <- dpobj$alphaChain[ind]
    clusterParams <- dpobj$clusterParametersChain[[ind]]
  } else {
    pointsPerCluster <- dpobj$pointsPerCluster
    alpha <- dpobj$alpha
    clusterParams <- dpobj$clusterParameters
  }

  numLabels <- length(pointsPerCluster)
  mdobj <- dpobj$mixingDistribution

  dirichlet_draws <- gtools::rdirichlet(1, c(pointsPerCluster, alpha))
  numBreaks <- ceiling(alpha + numLabels) * 20 + 5

  sticks <- StickBreaking(alpha + numLabels, numBreaks)
  sticks <- sticks * dirichlet_draws[numLabels + 1]

  sticks <- c(dirichlet_draws[-(numLabels + 1)], sticks)
  # postParams <- rbind(clusterParams, PriorDraw(mdobj, numBreaks))

  n_smps <- numBreaks + numLabels

  PriorDraws <- PriorDraw(mdobj, numBreaks)
  postParams <- list()

  for (i in seq_along(clusterParams)) {
    postParams[[i]] <- array(c(clusterParams[[i]], PriorDraws[[i]]), dim = c(dim(PriorDraws[[i]])[1:2],
      numBreaks + numLabels))
  }


  # smps <- sample.int(n_smps, replace = T, prob = sticks)
  # smpTable <- data.frame((table(smps)/n_smps))
  #   retParams <- list()
  # for (i in seq_along(clusterParams)) {
  #   retParams[[i]] <- postParams[[i]][, , smpTable$smp, drop = FALSE]
  # }
  # returnList <- list(weights = smpTable$Freq, params = retParams)
  #

  returnList <- list(weights=sticks, params=postParams)

  return(returnList)
}



