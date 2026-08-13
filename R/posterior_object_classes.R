posterior_metadata_value <- function(x) {
  if (is.null(x) || length(x) != 1L) {
    return(NA)
  }

  x
}

posterior_dp_metadata <- function(dpobj, source_function) {
  list(
    source_function = source_function,
    dp_type = dp_type(dpobj),
    kernel = dp_kernel(dpobj),
    conjugacy = dp_conjugacy(dpobj),
    alpha = posterior_metadata_value(dpobj$alpha),
    n = posterior_metadata_value(dpobj$n)
  )
}

posterior_set_metadata <- function(x, metadata) {
  for (name in names(metadata)) {
    attr(x, name) <- metadata[[name]]
  }

  x
}

new_dirichletprocess_posterior_clusters <- function(x, dpobj, ind = NA,
                                                    alpha = NA) {
  metadata <- posterior_dp_metadata(dpobj, "PosteriorClusters")
  metadata$ind <- posterior_metadata_value(ind)
  metadata$alpha <- posterior_metadata_value(alpha)

  x <- posterior_set_metadata(x, metadata)
  class(x) <- c("dirichletprocess_posterior_clusters", "list")
  x
}

new_dirichletprocess_prior_clusters <- function(x, dpobj, alpha = NA) {
  metadata <- posterior_dp_metadata(dpobj, "PriorClusters")
  metadata$alpha <- posterior_metadata_value(alpha)

  x <- posterior_set_metadata(x, metadata)
  class(x) <- c("dirichletprocess_prior_clusters", "list")
  x
}

new_dirichletprocess_posterior_summary <- function(x, dpobj, burnin, thinning,
                                                   level, retained_samples,
                                                   retained_iterations) {
  metadata <- posterior_dp_metadata(dpobj, "PosteriorSummary")
  metadata$burnin <- posterior_metadata_value(burnin)
  metadata$thinning <- posterior_metadata_value(thinning)
  metadata$level <- posterior_metadata_value(level)
  metadata$retained_samples <- posterior_metadata_value(retained_samples)
  metadata$retained_iterations <- retained_iterations

  x <- posterior_set_metadata(x, metadata)
  class(x) <- c("dirichletprocess_posterior_summary", "data.frame")
  x
}

new_dirichletprocess_posterior_frame <- function(x, dpobj, ndraws, ci_size) {
  metadata <- posterior_dp_metadata(dpobj, "PosteriorFrame")
  metadata$ndraws <- posterior_metadata_value(ndraws)
  metadata$ci_size <- posterior_metadata_value(ci_size)
  metadata$level <- posterior_metadata_value(1 - ci_size)

  x <- posterior_set_metadata(x, metadata)
  class(x) <- c("dirichletprocess_posterior_frame",
                "dirichletprocess_posterior_summary",
                "data.frame")
  x
}

posterior_plain_data_frame <- function(x) {
  out <- x
  class(out) <- "data.frame"
  out
}

posterior_cluster_param_summary <- function(params) {
  if (!is.list(params) || length(params) == 0L) {
    return(NA_character_)
  }

  param_names <- names(params)
  if (is.null(param_names)) {
    param_names <- as.character(seq_along(params))
  }

  paste(vapply(seq_along(params), function(i) {
    param_dim <- dim(params[[i]])
    param_shape <- if (is.null(param_dim)) {
      paste0("length ", length(params[[i]]))
    } else {
      paste(param_dim, collapse = "x")
    }
    paste0(param_names[[i]], " [", param_shape, "]")
  }, character(1)), collapse = ", ")
}

posterior_cluster_summary_frame <- function(object, object_label) {
  weights <- if (!is.null(object$weights)) as.numeric(object$weights) else numeric()
  total_weight <- if (length(weights) == 0L) NA_real_ else sum(weights, na.rm = TRUE)

  data.frame(
    object = object_label,
    kernel = attr(object, "kernel", exact = TRUE),
    conjugacy = attr(object, "conjugacy", exact = TRUE),
    alpha = attr(object, "alpha", exact = TRUE),
    n = attr(object, "n", exact = TRUE),
    atoms = length(weights),
    nonzero_atoms = sum(weights > 0, na.rm = TRUE),
    total_weight = total_weight,
    parameter_components = if (is.list(object$params)) length(object$params) else NA_integer_,
    parameter_summary = posterior_cluster_param_summary(object$params),
    stringsAsFactors = FALSE
  )
}

posterior_print_clusters <- function(x, object_label) {
  weights <- if (!is.null(x$weights)) as.numeric(x$weights) else numeric()
  total_weight <- if (length(weights) == 0L) NA_real_ else sum(weights, na.rm = TRUE)

  cat("<", object_label, ">\n", sep = "")
  cat("  kernel: ", attr(x, "kernel", exact = TRUE),
      "; conjugacy: ", attr(x, "conjugacy", exact = TRUE), "\n", sep = "")
  cat("  atoms: ", length(weights),
      "; total weight: ", format(total_weight, digits = 4), "\n", sep = "")
  cat("  elements: ", paste(names(x), collapse = ", "), "\n", sep = "")

  invisible(x)
}

posterior_plot_clusters <- function(x, main, xlab = "Atom", ylab = "Weight",
                                    ...) {
  weights <- if (!is.null(x$weights)) as.numeric(x$weights) else numeric()
  if (length(weights) == 0L) {
    stop("Cluster object does not contain any weights to plot.", call. = FALSE)
  }

  graphics::plot(seq_along(weights), weights, type = "h",
                 xlab = xlab, ylab = ylab, main = main, ...)
  graphics::points(seq_along(weights), weights, pch = 16)

  invisible(x)
}

#' @export
print.dirichletprocess_posterior_clusters <- function(x, ...) {
  posterior_print_clusters(x, "dirichletprocess_posterior_clusters")
}

#' @export
summary.dirichletprocess_posterior_clusters <- function(object, ...) {
  posterior_cluster_summary_frame(object, "posterior_clusters")
}

#' @export
plot.dirichletprocess_posterior_clusters <- function(x, y = NULL, ...) {
  posterior_plot_clusters(x, main = "Posterior cluster weights", ...)
}

#' @export
print.dirichletprocess_prior_clusters <- function(x, ...) {
  posterior_print_clusters(x, "dirichletprocess_prior_clusters")
}

#' @export
summary.dirichletprocess_prior_clusters <- function(object, ...) {
  posterior_cluster_summary_frame(object, "prior_clusters")
}

#' @export
plot.dirichletprocess_prior_clusters <- function(x, y = NULL, ...) {
  posterior_plot_clusters(x, main = "Prior cluster weights", ...)
}

#' @export
print.dirichletprocess_posterior_summary <- function(x, n = 6L, ...) {
  preview <- utils::head(posterior_plain_data_frame(x), n = n)

  cat("<dirichletprocess_posterior_summary>\n")
  cat("  rows: ", nrow(x), "; columns: ", paste(names(x), collapse = ", "), "\n", sep = "")
  cat("  kernel: ", attr(x, "kernel", exact = TRUE),
      "; level: ", attr(x, "level", exact = TRUE), "\n", sep = "")
  print(preview, row.names = FALSE, ...)

  invisible(x)
}

#' @export
summary.dirichletprocess_posterior_summary <- function(object, ...) {
  out <- summary.data.frame(posterior_plain_data_frame(object), ...)
  attr(out, "source_function") <- attr(object, "source_function", exact = TRUE)
  attr(out, "kernel") <- attr(object, "kernel", exact = TRUE)
  attr(out, "conjugacy") <- attr(object, "conjugacy", exact = TRUE)
  attr(out, "level") <- attr(object, "level", exact = TRUE)
  attr(out, "retained_samples") <- attr(object, "retained_samples", exact = TRUE)
  out
}

#' @export
plot.dirichletprocess_posterior_summary <- function(x, y = NULL,
                                                    xlab = NULL,
                                                    ylab = "Density",
                                                    main = NULL,
                                                    col = "black",
                                                    lwd = 2,
                                                    lty = 1,
                                                    ...) {
  if (!"Mean" %in% names(x)) {
    stop("Posterior summary object must contain a 'Mean' column to plot.",
         call. = FALSE)
  }
  if (!is.numeric(x$Mean)) {
    stop("Posterior summary object must contain a numeric 'Mean' column to plot.",
         call. = FALSE)
  }

  has_numeric_x <- "x" %in% names(x) && is.numeric(x$x)
  x_values <- if (has_numeric_x) {
    x$x
  } else {
    seq_len(nrow(x))
  }

  if (is.null(xlab)) {
    xlab <- if (has_numeric_x) "x" else "Evaluation index"
  }

  draw_order <- order(x_values)
  x_values <- x_values[draw_order]
  mean_values <- x$Mean[draw_order]

  has_interval <- all(c("Lower", "Upper") %in% names(x)) &&
    is.numeric(x$Lower) && is.numeric(x$Upper)
  if (has_interval) {
    lower_values <- x$Lower[draw_order]
    upper_values <- x$Upper[draw_order]
  }

  y_values <- if (has_interval) {
    c(mean_values, lower_values, upper_values)
  } else {
    mean_values
  }
  y_values <- y_values[is.finite(y_values)]
  if (length(y_values) == 0L) {
    stop("Posterior summary object has no finite values to plot.",
         call. = FALSE)
  }

  dots <- list(...)
  dots$type <- NULL
  plot_args <- c(
    list(x = x_values, y = mean_values, type = "n",
         xlab = xlab, ylab = ylab, main = main),
    dots
  )
  if (is.null(plot_args$ylim)) {
    plot_args$ylim <- range(y_values)
  }

  do.call(graphics::plot, plot_args)

  if (has_interval &&
      all(is.finite(x_values) & is.finite(lower_values) & is.finite(upper_values))) {
    graphics::polygon(
      c(x_values, rev(x_values)),
      c(lower_values, rev(upper_values)),
      col = grDevices::adjustcolor("grey70", alpha.f = 0.45),
      border = NA
    )
  }
  graphics::lines(x_values, mean_values, col = col, lwd = lwd, lty = lty)

  invisible(x)
}
