#' Generate the posterior function of the Dirichlet function
#'
#'@param dpobj Fitted Dirichlet Process object
#'@param ind What iteration to draw the posterior function from. Defaults to the last iteration.
#'@return A posterior function f(x).
#'
#'@examples
#'
#'y <- rnorm(10)
#'dp <- DirichletProcessGaussian(y)
#'dp <- Fit(dp, 5)
#'postFuncDraw <- PosteriorFunction(dp)
#'plot(-3:3, postFuncDraw(-3:3))
#'
#'@export
PosteriorFunction <- function(dpobj, ind) UseMethod("PosteriorFunction")


#'@export
PosteriorFunction.dirichletprocess <- function(dpobj, ind) {

  post_clusters <- PosteriorClusters(dpobj, ind)
  base_function <- function(x, theta) Likelihood(dpobj$mixingDistribution, x, theta)

  post_func <- weighted_function_generator(base_function, post_clusters$weights,
    post_clusters$params)

  return(post_func)
}
