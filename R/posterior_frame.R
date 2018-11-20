#' Calculate the posterior mean and quantiles from a Dirichlet process object.
#'
#' @param dpobj The Dirichlet process object to be drawn from.
#' @param xgrid The x values the posterior is to be evaluated at.
#' @param ndraws The number of posterior draws to take.
#' @param ciSize The size of the credible interval draw in terms of percentage.
#' @return A dataframe consisting of the posterior mean and credible intervals.
#' @export
PosteriorFrame <- function(dpobj, xgrid, ndraws=1000, ciSize=0.1){

  postDraws <- replicate(ndraws, PosteriorFunction(dpobj)(xgrid))

  posteriorMean <- rowMeans(postDraws)
  posteriorQuantiles <- apply(postDraws, 1, quantile, probs=c(ciSize/2, 1-ciSize/2))
  posteriorFrame <- data.frame(Mean=posteriorMean, t(posteriorQuantiles), x=xgrid)

  return(posteriorFrame)
}
