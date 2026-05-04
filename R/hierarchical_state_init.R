hdp_component_cluster_count <- function(parameter_component) {
  dims <- dim(parameter_component)
  if (is.null(dims) || length(dims) < 3) {
    stop("Hierarchical Phase 1 state init requires array-valued cluster parameters with a cluster axis.")
  }

  as.integer(dims[3])
}

hdp_empty_parameter_component <- function(parameter_component) {
  dims <- dim(parameter_component)
  if (is.null(dims) || length(dims) < 2) {
    stop("Hierarchical Phase 1 state init requires matrix/array-valued parameter components.")
  }

  array(vector(mode = typeof(parameter_component), length = 0L),
        dim = c(dims[1:2], 0L))
}

hdp_combine_parameter_component <- function(parameter_blocks) {
  template_index <- which(vapply(parameter_blocks,
                                 function(x) !is.null(dim(x)) && length(dim(x)) >= 3,
                                 logical(1)))[1]

  if (is.na(template_index)) {
    stop("Cannot initialise hierarchical Phase 1 state without array-valued cluster parameters.")
  }

  template <- parameter_blocks[[template_index]]
  total_clusters <- sum(vapply(parameter_blocks, hdp_component_cluster_count, integer(1)))

  if (total_clusters == 0L) {
    return(hdp_empty_parameter_component(template))
  }

  flattened_values <- unlist(lapply(parameter_blocks, as.vector), use.names = FALSE)
  array(flattened_values, dim = c(dim(template)[1:2], total_clusters))
}

hdp_combine_cluster_parameters <- function(cluster_parameter_blocks) {
  template_index <- which(vapply(cluster_parameter_blocks, length, integer(1)) > 0L)[1]

  if (is.na(template_index)) {
    return(list())
  }

  template_block <- cluster_parameter_blocks[[template_index]]

  combined <- lapply(seq_along(template_block), function(i) {
    parameter_blocks <- lapply(cluster_parameter_blocks, function(x) x[[i]])
    hdp_combine_parameter_component(parameter_blocks)
  })

  if (!is.null(names(template_block))) {
    names(combined) <- names(template_block)
  }

  combined
}

hdp_subset_cluster_parameters <- function(global_parameters, dish_labels) {
  subset_parameters <- lapply(global_parameters, function(component) {
    component[, , dish_labels, drop = FALSE]
  })

  if (!is.null(names(global_parameters))) {
    names(subset_parameters) <- names(global_parameters)
  }

  subset_parameters
}

hdp_initialise_phase1_state <- function(dpobjlist) {
  num_tables_per_group <- vapply(dpobjlist$indDP,
                                 function(dp_obj) as.integer(dp_obj$numberClusters),
                                 integer(1))

  table_dish_labels <- vector("list", length(dpobjlist$indDP))
  next_dish_label <- 1L

  for (i in seq_along(dpobjlist$indDP)) {
    n_tables <- num_tables_per_group[i]
    if (n_tables == 0L) {
      table_dish_labels[[i]] <- integer(0)
    } else {
      table_dish_labels[[i]] <- seq.int(next_dish_label, length.out = n_tables)
      next_dish_label <- next_dish_label + n_tables
    }
  }

  cluster_parameter_blocks <- lapply(dpobjlist$indDP, `[[`, "clusterParameters")
  global_parameters <- hdp_combine_cluster_parameters(cluster_parameter_blocks)
  dish_table_counts <- rep.int(1L, sum(num_tables_per_group))

  dpobjlist$tableDishLabels <- table_dish_labels
  dpobjlist$dishTableCounts <- dish_table_counts
  dpobjlist$globalParameters <- global_parameters

  for (i in seq_along(dpobjlist$indDP)) {
    dpobjlist$indDP[[i]]$clusterParameters <- hdp_subset_cluster_parameters(
      global_parameters,
      table_dish_labels[[i]]
    )
  }

  dpobjlist
}
