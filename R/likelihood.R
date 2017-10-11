#' The Likelihood of a Dirichlet process object.
#'
#' Collecting the fitted cluster parameters and number of datapoints associated with each parameter a Likelihood can be calculated.
#' Each cluster is weighted by the number of datapoints assigned.
#'
#' @param dpobj Dirichlet process object.
#' @return A function f(x) that represents the Likelihood of the dpobj.
#'
#' @examples
#' y <- rnorm(10)
#' dp <- DirichletProcessGaussian(y)
#' dp <- Fit(dp, 5)
#' f <- LikelihoodFunction(dp)
#' plot(f(-2:2))
#'
#' @export
LikelihoodFunction <- function(dpobj) UseMethod("LikelihoodFunction", dpobj)

#' @export
LikelihoodFunction.dirichletprocess <- function(dpobj) {

  base_function <- function(x, theta) Likelihood(dpobj$mixingDistribution, x, theta)

  Likelihood_function <- weighted_function_generator(base_function, dpobj$pointsPerCluster,
    dpobj$clusterParameters)

  return(Likelihood_function)
}
