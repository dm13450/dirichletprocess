hdp_cluster_parameter_template <- function(dpobjlist, group_index) {
  template <- dpobjlist$indDP[[group_index]]$clusterParameters

  if (length(template) > 0L) {
    return(template)
  }

  dpobjlist$globalParameters
}

hdp_empty_cluster_parameters <- function(parameter_template) {
  if (length(parameter_template) == 0L) {
    return(list())
  }

  empty_parameters <- lapply(parameter_template, hdp_empty_parameter_component)

  if (!is.null(names(parameter_template))) {
    names(empty_parameters) <- names(parameter_template)
  }

  empty_parameters
}

hdp_refresh_local_cluster_parameters <- function(dpobjlist, group_index) {
  local_dishes <- dpobjlist$tableDishLabels[[group_index]]

  if (length(local_dishes) == 0L) {
    template <- hdp_cluster_parameter_template(dpobjlist, group_index)
    dpobjlist$indDP[[group_index]]$clusterParameters <- hdp_empty_cluster_parameters(template)
    return(dpobjlist)
  }

  dpobjlist$indDP[[group_index]]$clusterParameters <- hdp_subset_cluster_parameters(
    dpobjlist$globalParameters,
    local_dishes
  )

  dpobjlist
}

hdp_refresh_all_cluster_parameters <- function(dpobjlist) {
  for (group_index in seq_along(dpobjlist$indDP)) {
    dpobjlist <- hdp_refresh_local_cluster_parameters(dpobjlist, group_index)
  }

  dpobjlist
}

hdp_parameter_slice <- function(parameter_list, slice_index) {
  sliced <- lapply(parameter_list, function(component) {
    component[, , slice_index, drop = FALSE]
  })

  if (!is.null(names(parameter_list))) {
    names(sliced) <- names(parameter_list)
  }

  sliced
}

hdp_append_global_dish <- function(dpobjlist, dish_parameters) {
  if (length(dpobjlist$globalParameters) == 0L) {
    dpobjlist$globalParameters <- dish_parameters
  } else {
    updated_parameters <- lapply(seq_along(dpobjlist$globalParameters), function(i) {
      current_component <- dpobjlist$globalParameters[[i]]
      new_component <- dish_parameters[[i]]

      array(c(current_component, new_component),
            dim = c(dim(current_component)[1:2], dim(current_component)[3] + 1L))
    })

    if (!is.null(names(dpobjlist$globalParameters))) {
      names(updated_parameters) <- names(dpobjlist$globalParameters)
    }

    dpobjlist$globalParameters <- updated_parameters
  }

  dpobjlist$dishTableCounts <- c(dpobjlist$dishTableCounts, 0L)
  dpobjlist
}

hdp_remove_global_dish <- function(dpobjlist, dish_label, refresh = TRUE) {
  dpobjlist$globalParameters <- lapply(dpobjlist$globalParameters, function(component) {
    component[, , -dish_label, drop = FALSE]
  })

  if (!is.null(names(dpobjlist$globalParameters))) {
    names(dpobjlist$globalParameters) <- names(dpobjlist$indDP[[1]]$clusterParameters)
  }

  dpobjlist$dishTableCounts <- dpobjlist$dishTableCounts[-dish_label]

  for (group_index in seq_along(dpobjlist$tableDishLabels)) {
    local_dishes <- dpobjlist$tableDishLabels[[group_index]]
    shift_index <- which(!is.na(local_dishes) & local_dishes > dish_label)
    if (length(shift_index) > 0L) {
      local_dishes[shift_index] <- local_dishes[shift_index] - 1L
    }
    dpobjlist$tableDishLabels[[group_index]] <- local_dishes
  }

  if (refresh) {
    return(hdp_refresh_all_cluster_parameters(dpobjlist))
  }

  dpobjlist
}

hdp_remove_empty_table <- function(dpobjlist, group_index, table_label) {
  local_dp <- dpobjlist$indDP[[group_index]]
  removed_dish <- dpobjlist$tableDishLabels[[group_index]][table_label]

  local_dp$pointsPerCluster <- local_dp$pointsPerCluster[-table_label]

  cluster_labels <- local_dp$clusterLabels
  relabel_mask <- !is.na(cluster_labels) & cluster_labels > table_label
  cluster_labels[relabel_mask] <- cluster_labels[relabel_mask] - 1L
  local_dp$clusterLabels <- cluster_labels
  local_dp$numberClusters <- length(local_dp$pointsPerCluster)

  dpobjlist$tableDishLabels[[group_index]] <- dpobjlist$tableDishLabels[[group_index]][-table_label]
  dpobjlist$indDP[[group_index]] <- local_dp
  dpobjlist$dishTableCounts[removed_dish] <- dpobjlist$dishTableCounts[removed_dish] - 1L

  if (dpobjlist$dishTableCounts[removed_dish] == 0L) {
    return(hdp_remove_global_dish(dpobjlist, removed_dish))
  }

  hdp_refresh_local_cluster_parameters(dpobjlist, group_index)
}

hdp_remove_customer_from_table <- function(dpobjlist, group_index, observation_index) {
  local_dp <- dpobjlist$indDP[[group_index]]
  current_table <- as.integer(local_dp$clusterLabels[observation_index])

  local_dp$clusterLabels[observation_index] <- NA_integer_
  local_dp$pointsPerCluster[current_table] <- local_dp$pointsPerCluster[current_table] - 1L
  dpobjlist$indDP[[group_index]] <- local_dp

  if (dpobjlist$indDP[[group_index]]$pointsPerCluster[current_table] == 0L) {
    return(hdp_remove_empty_table(dpobjlist, group_index, current_table))
  }

  dpobjlist
}

hdp_seat_customer_existing_table <- function(dpobjlist, group_index, observation_index, table_label) {
  local_dp <- dpobjlist$indDP[[group_index]]
  local_dp$clusterLabels[observation_index] <- table_label
  local_dp$pointsPerCluster[table_label] <- local_dp$pointsPerCluster[table_label] + 1L
  dpobjlist$indDP[[group_index]] <- local_dp
  dpobjlist
}

hdp_seat_customer_new_table <- function(dpobjlist, group_index, observation_index, dish_label) {
  local_dp <- dpobjlist$indDP[[group_index]]
  new_table_label <- length(local_dp$pointsPerCluster) + 1L

  local_dp$clusterLabels[observation_index] <- new_table_label
  local_dp$pointsPerCluster <- c(local_dp$pointsPerCluster, 1L)
  local_dp$numberClusters <- new_table_label
  dpobjlist$indDP[[group_index]] <- local_dp

  dpobjlist$tableDishLabels[[group_index]] <- c(dpobjlist$tableDishLabels[[group_index]], dish_label)
  dpobjlist$dishTableCounts[dish_label] <- dpobjlist$dishTableCounts[dish_label] + 1L

  hdp_refresh_local_cluster_parameters(dpobjlist, group_index)
}

hdp_observation_row <- function(data, observation_index) {
  if (is.matrix(data)) {
    return(data[observation_index, , drop = FALSE])
  }

  matrix(data[observation_index], nrow = 1)
}

hdp_safe_likelihood <- function(mixing_distribution, y, theta) {
  if (length(theta) == 0L) {
    return(numeric(0))
  }

  first_component <- theta[[1]]
  dims <- dim(first_component)

  if (is.null(dims) || length(dims) < 3 || dims[3] == 0L) {
    return(numeric(0))
  }

  as.numeric(Likelihood(mixing_distribution, y, theta))
}

hdp_new_table_log_probs <- function(alpha, gamma, dish_table_counts, dish_likelihoods,
                                    auxiliary_likelihoods) {
  total_tables <- sum(dish_table_counts)
  denom <- total_tables + gamma

  existing_dish_log_probs <- numeric(0)
  if (length(dish_likelihoods) > 0L) {
    existing_dish_weights <- alpha * dish_table_counts / denom
    existing_dish_log_probs <- weighted_log_probs(existing_dish_weights, dish_likelihoods)
  }

  auxiliary_log_probs <- numeric(0)
  if (length(auxiliary_likelihoods) > 0L) {
    aux_weight <- alpha * gamma / (length(auxiliary_likelihoods) * denom)
    auxiliary_log_probs <- weighted_log_probs(rep_len(aux_weight, length(auxiliary_likelihoods)),
                                              auxiliary_likelihoods)
  }

  c(existing_dish_log_probs, auxiliary_log_probs)
}

#' @export
#' @rdname ClusterComponentUpdate
ClusterComponentUpdate.hierarchical <- function(dpObj){

  for (group_index in seq_along(dpObj$indDP)) {
    local_dp <- dpObj$indDP[[group_index]]
    md_obj <- local_dp$mixingDistribution
    n_obs <- local_dp$n
    m_aux <- if (is.null(local_dp$m) || local_dp$m < 1) 1L else as.integer(local_dp$m)

    for (observation_index in seq_len(n_obs)) {
      dpObj <- hdp_remove_customer_from_table(dpObj, group_index, observation_index)
      local_dp <- dpObj$indDP[[group_index]]

      y_obs <- hdp_observation_row(local_dp$data, observation_index)

      existing_table_likelihoods <- hdp_safe_likelihood(md_obj, y_obs, local_dp$clusterParameters)
      existing_table_log_probs <- weighted_log_probs(local_dp$pointsPerCluster,
                                                     existing_table_likelihoods)

      global_dish_likelihoods <- hdp_safe_likelihood(md_obj, y_obs, dpObj$globalParameters)
      auxiliary_dishes <- PriorDraw(md_obj, m_aux)
      auxiliary_likelihoods <- hdp_safe_likelihood(md_obj, y_obs, auxiliary_dishes)

      new_table_log_probs <- hdp_new_table_log_probs(
        alpha = local_dp$alpha,
        gamma = dpObj$gamma,
        dish_table_counts = dpObj$dishTableCounts,
        dish_likelihoods = global_dish_likelihoods,
        auxiliary_likelihoods = auxiliary_likelihoods
      )

      log_probs <- c(existing_table_log_probs, new_table_log_probs)
      probs <- stable_allocation_probs(log_probs)
      choice <- sample.int(length(log_probs), 1, prob = probs)

      n_existing_tables <- local_dp$numberClusters
      n_existing_dishes <- length(dpObj$dishTableCounts)

      if (choice <= n_existing_tables) {
        dpObj <- hdp_seat_customer_existing_table(dpObj, group_index, observation_index, choice)
        next
      }

      new_table_choice <- choice - n_existing_tables
      if (new_table_choice <= n_existing_dishes) {
        dpObj <- hdp_seat_customer_new_table(dpObj, group_index, observation_index, new_table_choice)
        next
      }

      aux_index <- new_table_choice - n_existing_dishes
      new_dish <- hdp_parameter_slice(auxiliary_dishes, aux_index)
      dpObj <- hdp_append_global_dish(dpObj, new_dish)
      dpObj <- hdp_seat_customer_new_table(dpObj, group_index, observation_index,
                                           length(dpObj$dishTableCounts))
    }
  }

  dpObj
}
