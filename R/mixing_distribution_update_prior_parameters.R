#' Update Prior Parameters
#'
#' @param mdObj Mixing Distribution Object
#' @param clusterParameters Current cluster parameters
#' @param n Number of samples
#' @return mdobj New Mixing Distribution object with updated cluster parameters
#' @export
PriorParametersUpdate <- function(mdObj, clusterParameters, n = 1){
  UseMethod("PriorParametersUpdate", mdObj)
}

PriorUpdateModelLabel <- function(mdObj) {
  model_class <- mixing_distribution_display_class(mdObj)

  if (!is.na(model_class)) {
    return(model_class)
  }

  return(paste(class(mdObj), collapse = "/"))
}

SupportsPriorUpdate <- function(mdObj) {
  inherits(mdObj, "beta") || inherits(mdObj, "weibull")
}

PriorUpdateUnsupportedMessage <- function(mdObj, context = "updatePrior=TRUE") {
  paste0(context,
         " is not supported for model family '",
         PriorUpdateModelLabel(mdObj),
         "'. This model does not define hyperprior parameters or a ",
         "PriorParametersUpdate() method. Supported families are 'beta' and ",
         "'weibull' (including hierarchical beta via inherited beta support).")
}

AssertPriorUpdateSupported <- function(mdObj, context = "updatePrior=TRUE") {
  if (!SupportsPriorUpdate(mdObj)) {
    stop(PriorUpdateUnsupportedMessage(mdObj, context), call. = FALSE)
  }

  invisible(TRUE)
}

#' @export
#' @rdname PriorParametersUpdate
PriorParametersUpdate.normal <- function(mdObj, clusterParameters, n = 1) {
  stop(PriorUpdateUnsupportedMessage(mdObj, "PriorParametersUpdate()"),
       call. = FALSE)
}

#' @export
#' @rdname PriorParametersUpdate
PriorParametersUpdate.conjugate <- function(mdObj, clusterParameters, n = 1) {
  stop(PriorUpdateUnsupportedMessage(mdObj, "PriorParametersUpdate()"),
       call. = FALSE)
}

#' @export
#' @rdname PriorParametersUpdate
PriorParametersUpdate.exponential <- function(mdObj, clusterParameters, n = 1) {
  stop(PriorUpdateUnsupportedMessage(mdObj, "PriorParametersUpdate()"),
       call. = FALSE)
}


#' @export
#' @rdname PriorParametersUpdate
PriorParametersUpdate.default <- function(mdObj, clusterParameters, n = 1) {
  stop(PriorUpdateUnsupportedMessage(mdObj, "PriorParametersUpdate()"),
       call. = FALSE)
}
