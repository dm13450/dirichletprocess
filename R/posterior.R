#' Draw a posterior function conditional on one fitted state
#'
#' Lower-level helper for drawing a posterior function conditional on the
#' current fitted state, or on one retained stored iteration when
#' \code{ind} is supplied. For retained-sample posterior summaries across
#' multiple stored iterations, prefer \code{\link{PosteriorSummary}}.
#'
#'@param dpobj Fitted Dirichlet Process object
#'@param ind Stored-iteration index to draw from. If omitted, the current
#'  fitted state is used.
#'@return A posterior function \code{f(x)}.
#'
#'@examples
#'
#'y <- rnorm(10)
#'dp <- DirichletProcessGaussian(y)
#'dp <- Fit(dp, 5, progressBar = FALSE)
#'postFuncDraw <- PosteriorFunction(dp)
#'plot(-3:3, postFuncDraw(-3:3))
#'
#'@export
PosteriorFunction <- function(dpobj, ind) UseMethod("PosteriorFunction")


#'@export
PosteriorFunction.dirichletprocess <- function(dpobj, ind) {

  post_clusters <- PosteriorClusters(dpobj, ind)
  base_function <- function(x, theta) Likelihood(dpobj$mixingDistribution, x, theta)

  post_func <- weighted_function_generator(base_function,
                                           post_clusters$weights,
                                           post_clusters$params)

  return(post_func)
}
