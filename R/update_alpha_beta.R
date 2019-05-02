#' Update the \eqn{\alpha} and \eqn{\beta} parameter of a hidden Markov Dirichlet process model.
#'
#' @param dp Dirichlet process object
#' @export
UpdateAlphaBeta <- function(dp){

  newparams <- update_alpha_beta(dp$states)

  dp$alpha <- newparams[1]
  dp$beta <- newparams[2]
  return(dp)
}


update_alpha_beta <- function(dp_states){

  uniqueStates <- unique(dp_states)

  nii <- sapply(uniqueStates, function(i) sum(dp_states == i) - 1)

  optim(c(1,1), function(x) -1*alphabeta_log_posterior(x[1], x[2], nii),
        lower = c(1e-8,1e-8),
        method="L-BFGS-B")$par -> startPos

  alphaParam <- numeric(100)
  betaParam <- numeric(100)

  alphaParam[1] <- startPos[1]
  betaParam[1] <- startPos[2]

  oldPosterior <- alphabeta_log_posterior(alphaParam[1], betaParam[2], nii)

  for(i in 1:99){

    newAlpha <- rnorm(1, alphaParam[i], 0.1)
    newBeta <- rnorm(1, betaParam[i], 0.1)

    newPosterior <- alphabeta_log_posterior(newAlpha, newBeta, nii)

    accept_prob <- min(1, exp(newPosterior -
                                oldPosterior))

    if (is.na(accept_prob) | !length(accept_prob) ) {
      accept_prob <- 0
    }

    if (runif(1) < accept_prob) {

      alphaParam[i+1] <- newAlpha
      betaParam[i+1] <- newBeta
      oldPosterior <- newPosterior
    } else {
      alphaParam[i+1] <- alphaParam[i]
      betaParam[i+1] <- betaParam[i]
    }

  }

  return(c(alphaParam[100], betaParam[100]))
}

alphabeta_log_posterior <- function(alpha, beta, nii){

  if(alpha < 0 | beta < 0){
    return(-Inf)
  }

  logTerm1 <- log(beta) + lgamma(alpha + beta) - lgamma(alpha)

  logTerm2 <- lgamma(nii + alpha) - lgamma(nii + 1 + alpha + beta)


  dgamma(alpha, 1, 1, log=T) +
    dgamma(beta, 1, 1, log=T) +
    sum(logTerm1 + logTerm2)

}
