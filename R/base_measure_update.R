#' Update the parameters of the base measure ($G_0$) using
#' the current cluster parameter samples.
#'
#' @param dpobj Dirichlet process object
#' @export
#'
BaseMeasureUpdate <- function(dpobj){


  newmdobj <- PriorParametersUpdate(dpobj$mixingDistribution,
                                    dpobj$clusterParameters,
                                    dpobj$clusterLabels)

  dpobj$mixingDistribution <- newmdobj

  return(dpobj)

}


