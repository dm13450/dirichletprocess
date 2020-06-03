#' Fit a Hidden Markov Dirichlet Process Model


#' @param dpObj Initialised Dirichlet Process object
#' @param its Number of iterations to use
#' @param updatePrior Logical flag, defaults to \code{FAlSE}. Set whether the parameters of the base measure are updated.
#' @param progressBar Logical flag indicating whether to display a progress bar.
#' @return A Dirichlet Process object with the fitted cluster parameters and states.

#' @export
Fit.markov <- function(dpObj, its, updatePrior=F, progressBar = F){

  dpObj <- fit_hmm(dpObj, its, progressBar)

  return(dpObj)
}

fit_hmm <- function(dpObj, its, progressBar=F){

  if (progressBar){
    pb <- txtProgressBar(min=0, max=its, width=50, char="-", style=3)
  }

  alphaChain <- numeric(its)
  betaChain <- numeric(its)
  statesChain <- vector("list", its)
  paramChain <- vector("list", its)

  for(i in seq_len(its)){

    alphaChain[i] <- dpObj$alpha
    betaChain[i] <- dpObj$beta
    statesChain[[i]] <- dpObj$states


    paramChain[[i]] <- dpObj$uniqueParams

    dpObj <- UpdateStates(dpObj)
    dpObj <- UpdateAlphaBeta(dpObj)
    dpObj <- param_update(dpObj)


    if (progressBar) {
      setTxtProgressBar(pb, i)
    }

  }

  dpObj$alphaChain <- alphaChain
  dpObj$betaChain <- betaChain
  dpObj$statesChain <- statesChain
  dpObj$paramChain <- paramChain
  if (progressBar) {
    close(pb)
  }

  return(dpObj)
}


param_update <- function(dp){

  newParams <- cluster_parameter_update(dp$mixingDistribution, dp$data, dp$states, dp$params)

  dp$uniqueParams <- newParams
  dp$params <- newParams[dp$states]
  return(dp)
}
