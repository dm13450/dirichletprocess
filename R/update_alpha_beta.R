UpdateAlphaBeta <- function(dp){

  newparams <- update_alpha_beta(dp$states)

  dp$alpha <- newparams[1]
  dp$beta <- newparams[2]
  return(dp)
}


update_alpha_beta <- function(dp_states){

  uniqueStates <- unique(dp_states)

  nii <- sapply(uniqueStates, function(i) sum(dp_states == i) - 1)

  ###TODO: Add in MH sampiling.

  optim(c(1,1), function(x) -1*alphabeta_likelihood(x[1], x[2], nii), lower = c(1e-8,1e-8), method="L-BFGS-B")$par

}

alphabeta_likelihood <- function(alpha, beta, nii){

  term1 <- beta * gamma(alpha+beta) / gamma(alpha)
  term2 <- gamma(nii + alpha)/gamma(nii + 1 + alpha + beta)


  dgamma(alpha, 1, 1, log=T) + dgamma(beta, 1, 1, log=T) + sum(log(term1 * term2))

}
