hdp_restaurant_table_count <- function(dp_obj) {
  as.integer(length(dp_obj$pointsPerCluster))
}

hdp_active_dish_count <- function(dpobjlist) {
  as.integer(length(dpobjlist$dishTableCounts))
}

# Phase 4 hierarchical override:
# under the CRF rewrite, auxiliary new-dish proposals must come from the
# base measure H rather than the legacy truncated theta_k / pi_k pool.
PriorDraw.hierarchical <- function(mdObj, n = 1, ...) {
  if (inherits(mdObj, "beta")) {
    return(PriorDraw.beta(mdObj, n = n, ...))
  }

  if (inherits(mdObj, "mvnormal2")) {
    return(PriorDraw.mvnormal2(mdObj, n = n, ...))
  }

  stop("Phase 4 hierarchical PriorDraw is only implemented for hierarchical beta and hierarchical mvnormal2.")
}

#' @export
#' @rdname UpdateAlpha
UpdateAlpha.hierarchical <- function(dpobj) {
  for (group_index in seq_along(dpobj$indDP)) {
    n_tables <- hdp_restaurant_table_count(dpobj$indDP[[group_index]])
    dpobj$indDP[[group_index]]$numberClusters <- n_tables
    dpobj$indDP[[group_index]]$alpha <- update_concentration(
      oldParam = dpobj$indDP[[group_index]]$alpha,
      n = dpobj$indDP[[group_index]]$n,
      nParams = n_tables,
      priorParameters = dpobj$indDP[[group_index]]$alphaPriorParameters
    )
    dpobj$indDP[[group_index]]$mixingDistribution$alpha <- dpobj$indDP[[group_index]]$alpha
  }

  dpobj
}

#' @export
#' @rdname ClusterParameterUpdate
ClusterParameterUpdate.hierarchical <- function(dpObj) {
  # Phase 4 compatibility refresh:
  # local table parameters are canonical derived views from
  # globalParameters and tableDishLabels, so no independent local
  # cluster-parameter update should occur here.
  hdp_refresh_all_cluster_parameters(dpObj)
}

UpdateGamma <- function(dpobjlist){
  if (!inherits(dpobjlist, "hierarchical")) {
    stop("UpdateGamma is only implemented for hierarchical HDP objects in the rewritten live path.")
  }

  total_tables <- sum(vapply(dpobjlist$indDP, hdp_restaurant_table_count, integer(1)))
  active_dishes <- hdp_active_dish_count(dpobjlist)

  dpobjlist$gamma <- update_concentration(
    oldParam = dpobjlist$gamma,
    n = total_tables,
    nParams = active_dishes,
    priorParameters = dpobjlist$gammaPriors
  )

  for (group_index in seq_along(dpobjlist$indDP)) {
    dpobjlist$indDP[[group_index]]$mixingDistribution$gamma <- dpobjlist$gamma
  }

  dpobjlist
}
