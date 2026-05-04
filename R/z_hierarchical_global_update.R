hdp_table_data <- function(dpobjlist, group_index, table_label) {
  local_dp <- dpobjlist$indDP[[group_index]]
  data_rows <- local_dp$clusterLabels == table_label

  if (is.matrix(local_dp$data)) {
    return(local_dp$data[data_rows, , drop = FALSE])
  }

  matrix(local_dp$data[data_rows], ncol = 1)
}

hdp_log_density_product <- function(likelihood_values) {
  likelihood_values <- as.numeric(likelihood_values)

  if (length(likelihood_values) == 0L) {
    return(-Inf)
  }

  if (any(is.infinite(likelihood_values) & likelihood_values > 0)) {
    return(Inf)
  }

  finite_positive <- is.finite(likelihood_values) & !is.na(likelihood_values) & likelihood_values > 0
  if (!all(finite_positive)) {
    return(-Inf)
  }

  sum(log(likelihood_values))
}

hdp_component_count <- function(parameter_list) {
  if (length(parameter_list) == 0L) {
    return(0L)
  }

  as.integer(dim(parameter_list[[1]])[3])
}

hdp_parameter_log_likelihoods <- function(mixing_distribution, data_block, parameter_list) {
  n_parameters <- hdp_component_count(parameter_list)

  if (n_parameters == 0L) {
    return(numeric(0))
  }

  vapply(seq_len(n_parameters), function(parameter_index) {
    dish_parameter <- hdp_parameter_slice(parameter_list, parameter_index)
    dish_likelihood <- Likelihood(mixing_distribution, data_block, dish_parameter)
    hdp_log_density_product(dish_likelihood)
  }, numeric(1))
}

hdp_unassign_table_from_dish <- function(dpobjlist, group_index, table_label) {
  old_dish <- dpobjlist$tableDishLabels[[group_index]][table_label]
  dpobjlist$tableDishLabels[[group_index]][table_label] <- NA_integer_
  dpobjlist$dishTableCounts[old_dish] <- dpobjlist$dishTableCounts[old_dish] - 1L

  if (dpobjlist$dishTableCounts[old_dish] == 0L) {
    return(hdp_remove_global_dish(dpobjlist, old_dish, refresh = FALSE))
  }

  dpobjlist
}

hdp_assign_table_to_dish <- function(dpobjlist, group_index, table_label, dish_label,
                                     refresh = FALSE) {
  dpobjlist$tableDishLabels[[group_index]][table_label] <- dish_label
  dpobjlist$dishTableCounts[dish_label] <- dpobjlist$dishTableCounts[dish_label] + 1L

  if (refresh) {
    return(hdp_refresh_local_cluster_parameters(dpobjlist, group_index))
  }

  dpobjlist
}

hdp_group_auxiliary_draw_count <- function(dpobjlist, group_index) {
  local_dp <- dpobjlist$indDP[[group_index]]

  if (!is.null(local_dp$m) && is.finite(local_dp$m) && local_dp$m >= 1) {
    return(as.integer(local_dp$m))
  }

  1L
}

hdp_group_posterior_draw_count <- function(dpobjlist, group_index) {
  local_dp <- dpobjlist$indDP[[group_index]]

  if (!is.null(local_dp$mhDraws) && is.finite(local_dp$mhDraws) && local_dp$mhDraws >= 1) {
    return(as.integer(local_dp$mhDraws))
  }

  1L
}

hdp_extract_last_draw <- function(parameter_draws) {
  if (length(parameter_draws) == 0L) {
    return(list())
  }

  n_draws <- dim(parameter_draws[[1]])[3]
  hdp_parameter_slice(parameter_draws, n_draws)
}

hdp_global_atom_draw <- function(dpobjlist, data_block, current_parameter) {
  md_obj <- dpobjlist$indDP[[1]]$mixingDistribution

  if (inherits(md_obj, "beta")) {
    n_draws <- hdp_group_posterior_draw_count(dpobjlist, 1L)
    return(hdp_extract_last_draw(
      PosteriorDraw(md_obj, data_block, n = n_draws, start_pos = current_parameter)
    ))
  }

  if (inherits(md_obj, "mvnormal2")) {
    return(hdp_extract_last_draw(
      PosteriorDraw(md_obj, data_block, n = 1L, start_pos = current_parameter)
    ))
  }

  stop("Phase 3 global atom update is only implemented for hierarchical beta and hierarchical mvnormal2.")
}

hdp_sync_legacy_hierarchical_state <- function(dpobjlist) {
  for (group_index in seq_along(dpobjlist$indDP)) {
    dpobjlist$indDP[[group_index]]$mixingDistribution$theta_k <- dpobjlist$globalParameters
  }

  dpobjlist
}

hdp_refresh_global_atoms <- function(dpobjlist) {
  n_dishes <- length(dpobjlist$dishTableCounts)

  if (n_dishes == 0L) {
    dpobjlist$globalParameters <- list()
    return(hdp_sync_legacy_hierarchical_state(dpobjlist))
  }

  for (dish_label in seq_len(n_dishes)) {
    pooled_blocks <- vector("list", length(dpobjlist$indDP))

    for (group_index in seq_along(dpobjlist$indDP)) {
      table_labels <- which(dpobjlist$tableDishLabels[[group_index]] == dish_label)

      if (length(table_labels) == 0L) {
        next
      }

      row_mask <- dpobjlist$indDP[[group_index]]$clusterLabels %in% table_labels
      if (is.matrix(dpobjlist$indDP[[group_index]]$data)) {
        pooled_blocks[[group_index]] <- dpobjlist$indDP[[group_index]]$data[row_mask, , drop = FALSE]
      } else {
        pooled_blocks[[group_index]] <- matrix(dpobjlist$indDP[[group_index]]$data[row_mask], ncol = 1)
      }
    }

    pooled_blocks <- pooled_blocks[!vapply(pooled_blocks, is.null, logical(1))]
    if (length(pooled_blocks) == 0L) {
      next
    }

    pooled_data <- do.call(rbind, pooled_blocks)
    current_parameter <- hdp_parameter_slice(dpobjlist$globalParameters, dish_label)
    new_parameter <- hdp_global_atom_draw(dpobjlist, pooled_data, current_parameter)

    for (component_index in seq_along(dpobjlist$globalParameters)) {
      dpobjlist$globalParameters[[component_index]][, , dish_label] <- new_parameter[[component_index]][, , 1]
    }
  }

  dpobjlist <- hdp_refresh_all_cluster_parameters(dpobjlist)
  hdp_sync_legacy_hierarchical_state(dpobjlist)
}

#' @export
GlobalParameterUpdate.hierarchical <- function(dpobjlist) {
  for (group_index in seq_along(dpobjlist$indDP)) {
    n_tables <- dpobjlist$indDP[[group_index]]$numberClusters
    if (n_tables == 0L) {
      next
    }

    md_obj <- dpobjlist$indDP[[group_index]]$mixingDistribution
    m_aux <- hdp_group_auxiliary_draw_count(dpobjlist, group_index)

    for (table_label in seq_len(n_tables)) {
      table_data <- hdp_table_data(dpobjlist, group_index, table_label)
      dpobjlist <- hdp_unassign_table_from_dish(dpobjlist, group_index, table_label)

      existing_dish_log_probs <- hdp_parameter_log_likelihoods(md_obj, table_data, dpobjlist$globalParameters)
      auxiliary_dishes <- PriorDraw(md_obj, m_aux)
      auxiliary_log_probs <- hdp_parameter_log_likelihoods(md_obj, table_data, auxiliary_dishes)

      existing_log_probs <- if (length(existing_dish_log_probs) > 0L) {
        log(dpobjlist$dishTableCounts) + existing_dish_log_probs
      } else {
        numeric(0)
      }

      new_dish_log_probs <- if (length(auxiliary_log_probs) > 0L) {
        rep_len(log(dpobjlist$gamma / m_aux), length(auxiliary_log_probs)) + auxiliary_log_probs
      } else {
        numeric(0)
      }

      log_probs <- c(existing_log_probs, new_dish_log_probs)
      probs <- stable_allocation_probs(log_probs)
      choice <- sample.int(length(log_probs), 1, prob = probs)

      n_existing_dishes <- length(dpobjlist$dishTableCounts)
      if (choice <= n_existing_dishes) {
        dpobjlist <- hdp_assign_table_to_dish(dpobjlist, group_index, table_label, choice)
      } else {
        aux_index <- choice - n_existing_dishes
        new_dish <- hdp_parameter_slice(auxiliary_dishes, aux_index)
        dpobjlist <- hdp_append_global_dish(dpobjlist, new_dish)
        dpobjlist <- hdp_assign_table_to_dish(dpobjlist, group_index, table_label,
                                              length(dpobjlist$dishTableCounts))
      }
    }
  }

  hdp_refresh_global_atoms(dpobjlist)
}

#' Compatibility shim for hierarchical global atom refresh.
#'
#' In the rewritten live hierarchical path, \code{GlobalParameterUpdate()}
#' handles the global atom refresh internally, so \code{UpdateG0()} is
#' retained only for backward compatibility and returns the hierarchical
#' object unchanged.
#'
#' @param dpobjlist Hierarchical HDP object.
#' @return The hierarchical object, unchanged.
#' @export
UpdateG0 <- function(dpobjlist){
  if (!inherits(dpobjlist, "hierarchical")) {
    stop("UpdateG0 is only available for hierarchical HDP objects.")
  }

  # Phase 3 compatibility shim:
  # the live hierarchical sampler now treats global dish assignment and
  # global atom refresh inside GlobalParameterUpdate(), so UpdateG0 is a no-op.
  dpobjlist
}
