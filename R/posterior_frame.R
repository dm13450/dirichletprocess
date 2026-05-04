#' Legacy conditional-state posterior summary helper.
#'
#' `PosteriorFrame()` is retained for backward compatibility. It repeatedly
#' draws \code{PosteriorFunction(dpobj)} from the current fitted state only and
#' summarises those conditional draws on \code{xgrid}. For retained-sample
#' posterior summaries and pointwise credible intervals across stored MCMC
#' iterations, prefer \code{\link{PosteriorSummary}}.
#'
#' @param dpobj The Dirichlet process object to be drawn from.
#' @param xgrid The x values the conditional posterior is to be evaluated at.
#' @param ndraws The number of conditional-state posterior draws to take.
#' @param ci_size The size of the conditional credible interval in tail
#'   probability terms.
#' @return A data frame containing conditional-state posterior means and
#'   quantiles on \code{xgrid}.
#' @export
PosteriorFrame <- function(dpobj, xgrid, ndraws=1000, ci_size=0.1){
  if (inherits(dpobj, "hierarchical")) {
    stop(
      "PosteriorFrame() is not defined for top-level hierarchical HDP objects in the rewritten R path. Use PosteriorFrame(dpobj$indDP[[j]], xgrid, ...) for a restaurant-specific posterior summary.",
      call. = FALSE
    )
  }

  postDraws <- replicate(ndraws, PosteriorFunction(dpobj)(xgrid))

  posteriorMean <- rowMeans(postDraws)
  posteriorQuantiles <- apply(postDraws, 1, quantile, probs=c(ci_size/2, 1-ci_size/2))
  posteriorFrame <- data.frame(Mean=posteriorMean, t(posteriorQuantiles), x=xgrid)

  return(posteriorFrame)
}
