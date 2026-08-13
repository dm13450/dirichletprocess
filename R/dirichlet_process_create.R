#' Dirichlet Process Object
#'
#' Using a  previously created Mixing Distribution Object (\code{mdObject}) create a Dirichlet process object.
#' `\code{alphaPriorParameters} sets the parameters for alpha using the shape-rate specification of the gamma distribution.
#'
#' @param x Data
#' @param mdObject Mixing Distribution Object
#' @param alphaPriorParameters Prior parameters for the concentration parameter of the Dirichlet Process
#' @param mhDraws Number of posterior samples to take in the nonconjugate case
#' @export
DirichletProcessCreate <- function(x, mdObject, alphaPriorParameters = c(1, 1), mhDraws=250) {

  if (!is.matrix(x)) {
    x <- matrix(x, ncol = 1)
  }

  dpObj <- list(data = x,
                mixingDistribution = mdObject,
                n = dim(x)[1],
                alphaPriorParameters = alphaPriorParameters,
                alpha = rgamma(1, alphaPriorParameters[1], alphaPriorParameters[2]),
                kernel = mixing_distribution_kernel(mdObject),
                conjugacy = mixing_distribution_conjugacy(mdObject),
                mhDraws=mhDraws)

  class(dpObj) <- c("dirichletprocess", "list")

  return(dpObj)
}



