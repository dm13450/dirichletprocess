# Declare global variables for R CMD check
utils::globalVariables(c("x1", "x2", "Cluster", "y",
                         "Lower", "Mean", "Upper"))

plot_dirichletprocess <- function(x, ...) {
  if (inherits(x, "hierarchical")) {
    stop(
      "plot_dirichletprocess() is not defined for top-level hierarchical HDP objects in the rewritten R path. Plot a restaurant-specific DP instead, for example plot_dirichletprocess(x$indDP[[j]], ...).",
      call. = FALSE
    )
  }

  mdobj <- x$mixingDistribution
  UseMethod("plot_dirichletprocess", mdobj)
}

#' @export
plot_dirichletprocess.default <- function(x, ...){

  if (ncol(x$data) == 1){
    return(plot_dirichletprocess_univariate(x, ...))
  } else {
    return(plot_dirichletprocess_multivariate(x, ...))
  }

}

#' @export
plot_dirichletprocess.gaussian <- function(x, ...){
  plot_dirichletprocess_univariate(x, ...)
}

#' @export
plot_dirichletprocess.beta <- function(x, ...) {
  plot_dirichletprocess_univariate(x, ...)
}

#' @export
plot_dirichletprocess.weibull <- function(x, ...) {
  plot_dirichletprocess_univariate(x, ...)
}

#' @export
plot_dirichletprocess.mvnormal <- function(x, ...) {
  plot_dirichletprocess_multivariate(x)
}

ordinary_plot_summary_frame <- function(x, x_grid, ci_size) {
  storage <- dp_existing_sample_storage(x)

  if (length(storage$storedIterations) < 2L) {
    return(NULL)
  }

  tryCatch(
    PosteriorSummary(x, x_grid, burnin = 0, thinning = 1, level = 1 - ci_size),
    error = function(e) NULL
  )
}

#' @export
#' @rdname plot.dirichletprocess
plot_dirichletprocess_univariate <- function(x,
                                             likelihood  = FALSE,
                                             single      = TRUE,
                                             data_fill   = "black",
                                             data_method = "density",
                                             data_bw     = NULL,
                                             ci_size     = .05,
                                             xgrid_pts   = 100,
                                             quant_pts   = 100,
                                             xlim        = NA) {

  graph <- ggplot2::ggplot(data.frame(dt = x$data), ggplot2::aes(x = dt)) +
    ggplot2::theme(axis.title = ggplot2::element_blank())

  if (data_method == "density") {
    graph <- graph + ggplot2::geom_density(fill = data_fill,
                                           bw = ifelse(is.null(data_bw), "nrd0", data_bw))
  } else if (data_method == "hist" | data_method == "histogram") {
    graph <- graph + ggplot2::geom_histogram(ggplot2::aes(x = dt,
                                                          y = ggplot2::after_stat(density)),
                                             fill = data_fill,
                                             binwidth = data_bw)
  } else if (data_method != "none") {
    stop("Unknown `data_method`.")
  }

  if (is.na(xlim[1])) {
    x_grid <- pretty(x$data, n = xgrid_pts)
  } else {
    x_grid <- seq(xlim[1], xlim[2], length.out = xgrid_pts)
  }

  posterior_summary <- ordinary_plot_summary_frame(x, x_grid, ci_size)

  if (!is.null(posterior_summary)) {
    graph <- graph + ggplot2::geom_line(
      data = posterior_summary,
      ggplot2::aes(x = x, y = Lower, colour = "Posterior"),
      linetype = 2
    )
    graph <- graph + ggplot2::geom_line(
      data = posterior_summary,
      ggplot2::aes(x = x, y = Mean, colour = "Posterior")
    )
    graph <- graph + ggplot2::geom_line(
      data = posterior_summary,
      ggplot2::aes(x = x, y = Upper, colour = "Posterior"),
      linetype = 2
    )
  } else {
    message(
      "Credible intervals are unavailable because this object has no usable stored MCMC samples. Fit with storeSamples = TRUE to enable PosteriorSummary()-based intervals."
    )
    graph <- graph + ggplot2::geom_line(
      data = data.frame(x = x_grid, y = LikelihoodFunction(x)(x_grid)),
      ggplot2::aes(x = x, y = y, colour = "Posterior")
    )
  }

  if (likelihood) {
    graph <- graph + ggplot2::stat_function(fun = function(z) LikelihoodFunction(x)(z),
                                            n = xgrid_pts * 10,
                                            ggplot2::aes(colour = "Likelihood"))
  } else {
    graph <- graph + ggplot2::guides(colour="none")
  }

  return(graph)
}

#' @export
#' @rdname plot.dirichletprocess
plot_dirichletprocess_multivariate <- function(x) {

  plotFrame <- data.frame(x1=x$data[,1], x2=x$data[,2], Cluster=as.factor(x$clusterLabel))

  graph <- ggplot2::ggplot(plotFrame, ggplot2::aes(x=x1, y=x2, colour=Cluster)) +
    ggplot2::geom_point()
  return(graph)
}
