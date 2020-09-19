#' Generate the prior function of the Dirichlet process
#'
#' @param dpobj A Dirichlet process object
#' @return A function f(x) that represents a draw from the prior distrubtion of the Dirichlet process.
#'
#'  @export
PriorFunction <- function(dpobj) UseMethod("PriorFunction")

#' @export
PriorFunction.dirichletprocess <- function(dpobj){

  prior_clusters <- PriorClusters(dpobj)

  base_function <- function(x, theta) Likelihood(dpobj$mixingDistribution, x, theta)

  prior_func <- weighted_function_generator(base_function,
                                            prior_clusters$weights,
                                            prior_clusters$params)
  return(prior_func)
}
