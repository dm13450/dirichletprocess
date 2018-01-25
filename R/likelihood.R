#' The Likelihood of a Dirichlet process object.
#'
#' Collecting the fitted cluster parameters and number of datapoints associated with each parameter a likelihood can be calculated.
#' Each cluster is weighted by the number of datapoints assigned.
#'
#' @param dpobj Dirichlet process object.
#' @param ind The iteration number. Defaults to the last iteration.
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
LikelihoodFunction <- function(dpobj, ind) UseMethod("LikelihoodFunction", dpobj)

#' @export
LikelihoodFunction.dirichletprocess <- function(dpobj, ind) {

  base_function <- function(x, theta) Likelihood(dpobj$mixingDistribution, x, theta)

  if (missing(ind)){
    likelihood_function <- weighted_function_generator(base_function, dpobj$pointsPerCluster,
      dpobj$clusterParameters)
  } else {
    likelihood_function <- weighted_function_generator(base_function,
                                                       dpobj$weightsChain[[ind]]*dpobj$n,
                                                       dpobj$clusterParametersChain[[ind]])
  }

  return(likelihood_function)
}
