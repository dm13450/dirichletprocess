hdp_top_level_helper_error <- function(helper_name, detail = "restaurant-specific result") {
  stop(
    sprintf(
      "%s() is not defined for top-level hierarchical HDP objects in the rewritten R path. Use %s(dpobj$indDP[[j]], ...) for a %s.",
      helper_name,
      helper_name,
      detail
    ),
    call. = FALSE
  )
}

#' @export
#' @rdname PosteriorFunction
PosteriorFunction.hierarchical <- function(dpobj, ind) {
  hdp_top_level_helper_error("PosteriorFunction", "restaurant-specific posterior function")
}

#' @export
#' @rdname PosteriorClusters
PosteriorClusters.hierarchical <- function(dpobj, ind) {
  hdp_top_level_helper_error("PosteriorClusters", "restaurant-specific posterior cluster draw")
}

#' @export
plot.hierarchical <- function(x, ...) {
  stop(
    "plot() is not defined for top-level hierarchical HDP objects in the rewritten R path. Plot a restaurant-specific DP instead, for example plot(x$indDP[[j]], ...).",
    call. = FALSE
  )
}
