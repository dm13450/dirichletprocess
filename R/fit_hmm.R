#' Fit a Hidden Markov Dirichlet Process Model


#' @param dpObj Initialised Dirichlet Process object
#' @param its Number of iterations to use
#' @param updatePrior Logical flag, defaults to \code{FAlSE}. Set whether the parameters of the base measure are updated.
#' @param progressBar Logical flag indicating whether to display a progress bar.
#' @return A Dirichlet Process object with the fitted cluster parameters and states.

#' @export
Fit.markov <- function(dp, its, updatePrior=F, progressBar = F){

  dp <- fit_hmm(dp, its, progressBar)

  return(dp)
}

fit_hmm <- function(dp, its, progressBar=F){

  if (progressBar){
    pb <- txtProgressBar(min=0, max=its, width=50, char="-", style=3)
  }

  alphaChain <- numeric(its)
  betaChain <- numeric(its)
  statesChain <- vector("list", its)

  for(i in seq_len(its)){

    alphaChain[i] <- dp$alpha
    betaChain[i] <- dp$beta
    statesChain[[i]] <- dp$states

    dp <- UpdateStates(dp)
    dp <- UpdateAlphaBeta(dp)
    dp <- param_update(dp)

    if (progressBar) {
      setTxtProgressBar(pb, i)
    }

  }

  dp$alphaChain <- alphaChain
  dp$betaChain <- betaChain
  dp$statesChain <- statesChain

  if (progressBar) {
    close(pb)
  }

  return(dp)
}


param_update <- function(dp){


  newParams <- cluster_parameter_update(dp$mdobj, dp$data, dp$states, dp$params)

  dp$params <- newParams
  return(dp)
}
