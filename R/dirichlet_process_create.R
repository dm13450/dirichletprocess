#' Create a Dirichlet Process object
#'
#' Using a  previously created Mixing Distribution Object (\code{mdObject}) create a Dirichlet process object.
#'
#' @param x Data
#' @param mdObject Mixing Distribution Object
#' @param alphaPriorParameters Prior parameters for the concentration parameter of the Dirichlet Process
#' @export
DirichletProcessCreate <- function(x, mdObject, alphaPriorParameters = c(1, 1)) {
  
  if (!is.matrix(x)) 
    x <- matrix(x, ncol = 1)
  
  dpObj <- list(data = x, mixingDistribution = mdObject, n = dim(x)[1], alphaPriorParameters = alphaPriorParameters, 
    alpha = rgamma(1, alphaPriorParameters[1], alphaPriorParameters[2]))
  
  class(dpObj) <- append(class(dpObj), c("dirichletprocess", class(mdObject)[-1]))
  
  return(dpObj)
}






