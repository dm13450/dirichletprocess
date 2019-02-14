#'Update the Dirichlet process concentration parameter.
#'
#'Using the procedure outlined in West (1992) we sample the concentration parameter of the Dirichlet process. See reference for further details.
#'
#'@param dpobj Dirichlet process object.
#'
#'@return A Dirichlet process object with updated concentration parameter.
#'
#'
#'@references West, M. (1992). Hyperparameter estimation in Dirichlet process mixture models. ISDS Discussion Paper# 92-A03: Duke University.
#'@export
UpdateAlpha <- function(dpobj) UseMethod("UpdateAlpha", dpobj)

#' @export
#' @rdname UpdateAlpha
UpdateAlpha.default <- function(dpobj) {

  newAlpha <- update_concentration(dpobj$alpha, dpobj$n, dpobj$numberClusters, dpobj$alphaPriorParameters)
  dpobj$alpha <- newAlpha

  return(dpobj)
}

#' @export
#' @rdname UpdateAlpha
UpdateAlpha.hierarchical <- function(dpobj) {

  for(i in seq_along(dpobj$indDP)){
    dpobj$indDP[[i]] <- UpdateAlpha(dpobj$indDP[[i]])
    dpobj$indDP[[i]]$mixingDistribution$alpha <- dpobj$indDP[[i]]$alpha
  }

  return(dpobj)
}

