#' Create a mixing distribution object
#'
#' The constructor function for a mixing distribution object.
#' Use this function to prepare an object for use with the appropriate distribution functions.
#'
#' @param distribution The name of the distribution mixture
#' @param priorParameters The prior parameters
#' @param conjugate Whether the prior is conjugate to the Likelihood.
#' @param mhStepSize The scale of the proposal parameter for the Metropolis Hastings algorithm. Not needed for conjugate mixtures.
#' @param hyperPriorParameters Vector of hyperPriorParameters for the distribution.
#' @export
MixingDistribution <- function(distribution, priorParameters, conjugate, mhStepSize=NULL, hyperPriorParameters=NULL) {

  mdObj <- list(distribution = distribution,
                priorParameters = priorParameters,
                conjugate = conjugate,
                mhStepSize = mhStepSize,
                hyperPriorParameters = hyperPriorParameters)

  class(mdObj) <- append(class(mdObj), c(distribution, conjugate))
  return(mdObj)
}

MhParameterProposal <- function(mdObj, old_params){
  UseMethod("MhParameterProposal", mdObj)
}
