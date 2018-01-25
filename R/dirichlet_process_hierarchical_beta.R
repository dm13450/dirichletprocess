#' Create a Hierarchical Dirichlet Mixture of Beta Distributions
#'
#' @param dataList List of data for each separate Dirichlet mixture object
#' @param maxY Maximum value for the Beta distribution.
#' @param priorParameters Prior Parameters for the top level base distribution.
#' @param hyperPriorParameters Hyper prior parameters for the top level base distribution.
#' @param gammaPriors Prior parameters for the top level concentration parameter.
#' @param alphaPriors Prior parameters for the individual parameters.
#' @param mhStepSize Metropolis Hastings jump size.
#' @param numSticks Truncation level for the Stick Breaking formulation.
#' @return dpobjlist A Hierarchical Dirichlet Process object that can be fitted, plotted etc.
#' @export
DirichletProcessHierarchicalBeta <- function(dataList, maxY,
                                             priorParameters = c(2,8),
                                             hyperPriorParameters = c(1,0.125),
                                             gammaPriors = c(2,4), alphaPriors = c(2, 4),
                                             mhStepSize = c(0.1,0.1), numSticks = 50) {

  gammaParam <- rgamma(1, gammaPriors[1], gammaPriors[2])
  alphaParams <- rgamma(length(dataList), alphaPriors[1], alphaPriors[2])

  mdobj_list <- HierarchicalBetaCreate(n=length(dataList), priorParameters=priorParameters,
                                       hyperPriorParameters=hyperPriorParameters, gammaParam=gammaParam,
                                      alpha0 = alphaParams, maxT=maxY, mhStepSize=mhStepSize, num_sticks=numSticks)

  dpobjlist <- list()
  dpobjlist$indDP <- lapply(seq_along(dataList), function(x) DirichletProcessCreate(dataList[[x]],
    mdobj_list[[x]], alphaPriors))

  dpobjlist$indDP <- lapply(dpobjlist$indDP, Initialise, posterior=FALSE)

  for(i in seq_along(dpobjlist$indDP)){
    dpobjlist$indDP[[i]]$alpha <- alphaParams[i]
  }

  dpobjlist$globalParameters <- mdobj_list[[1]]$theta_k
  dpobjlist$globalStick <- mdobj_list[[1]]$beta_k
  dpobjlist$gamma <- gammaParam

  class(dpobjlist) <- c("list", "dirichletprocess", "hierarchical")

  return(dpobjlist)
}





