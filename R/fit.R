#' Fit the Dirichlet process object
#'
#' Using Neal's algorithm 4 or 8 depending on conjugacy the sampling procedure for a Dirichlet process is carried out.
#' Lists of both cluster parameters, weights and the sampled concentration values are included in the fitted \code{dpObj}.
#' When \code{update_prior} is set to \code{TRUE} the parameters of the base measure are also updated.
#'
#' @param dpObj Initialised Dirichlet Process object
#' @param its Number of iterations to use
#' @param updatePrior Logical flag, defaults to \code{FAlSE}. Set whether the parameters of the base measure are updated.
#' @param progressBar Logical flag indicating whether to display a progress bar.
#' @return A Dirichlet Process object with the fitted cluster parameters and labels.
#'
#' @references Neal, R. M. (2000). Markov chain sampling methods for Dirichlet process mixture models. Journal of computational and graphical statistics, 9(2), 249-265.
#'
#' @export
Fit <- function(dpObj, its, updatePrior = FALSE, progressBar=TRUE) UseMethod("Fit", dpObj)

#' @export
Fit.default <- function(dpObj, its, updatePrior = FALSE, progressBar = interactive()) {

  if (progressBar){
    pb <- txtProgressBar(min=0, max=its, width=50, char="-", style=3)
  }

  alphaChain <- numeric(its)
  likelihoodChain <- numeric(its)
  weightsChain <- vector("list", length = its)
  clusterParametersChain <- vector("list", length = its)
  priorParametersChain <- vector("list", length = its)
  labelsChain <- vector("list", length = its)

  for (i in seq_len(its)) {

    alphaChain[i] <- dpObj$alpha
    weightsChain[[i]] <- dpObj$pointsPerCluster / dpObj$n
    clusterParametersChain[[i]] <- dpObj$clusterParameters
    priorParametersChain[[i]] <- dpObj$mixingDistribution$priorParameters
    labelsChain[[i]] <- dpObj$clusterLabels


    likelihoodChain[i] <- sum(log(LikelihoodDP(dpObj)))

    dpObj <- ClusterComponentUpdate(dpObj)
    dpObj <- ClusterParameterUpdate(dpObj)
    dpObj <- UpdateAlpha(dpObj)

    if (updatePrior) {
      dpObj$mixingDistribution <- PriorParametersUpdate(dpObj$mixingDistribution,
                                                        dpObj$clusterParameters)
    }
    if (progressBar){
      setTxtProgressBar(pb, i)
    }
  }

  dpObj$weights <- dpObj$pointsPerCluster / dpObj$n
  dpObj$alphaChain <- alphaChain
  dpObj$likelihoodChain <- likelihoodChain
  dpObj$weightsChain <- weightsChain
  dpObj$clusterParametersChain <- clusterParametersChain
  dpObj$priorParametersChain <- priorParametersChain
  dpObj$labelsChain <- labelsChain

  if (progressBar) {
    close(pb)
  }
  return(dpObj)
}

#'@export
Fit.hierarchical <- function(dpObj, its, updatePrior = FALSE, progressBar = interactive()){
  if (progressBar) {
    pb <- txtProgressBar(min=0, max=its, width=50, char="-", style=3)
  }

  gammaValues <- numeric(its)

  for(i in seq_len(its)){

    dpObj <- ClusterComponentUpdate(dpObj)
    dpObj <- UpdateAlpha(dpObj)
    dpObj <- GlobalParameterUpdate(dpObj)
    dpObj <- UpdateG0(dpObj)
    dpObj <- UpdateGamma(dpObj)

    if (updatePrior) {

      clustParamLen <- length(unique(unlist(sapply(dpObj$indDP, function(x) x$clusterParameters[[1]]))))

      clustParam <- lapply(dpObj$globalParameters, function(x) x[,,1:clustParamLen, drop=FALSE])

      tempMD <- PriorParametersUpdate(dpObj$indDP[[1]]$mixingDistribution, clustParam)

      for(j in seq_along(dpObj$indDP)){
        dpObj$indDP[[j]]$mixingDistribution$priorParameters <- tempMD$priorParameters
      }
    }

    if (progressBar) {
      setTxtProgressBar(pb, i)
    }

    gammaValues[i] <- dpObj$gamma

  }
  dpObj$gammaValues <- gammaValues
  if (progressBar) {
    close(pb)
  }
  return(dpObj)
}

