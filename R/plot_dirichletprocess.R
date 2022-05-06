
plot_dirichletprocess <- function(x, ...) {
  mdobj <- x$mixingDistribution
  UseMethod("plot_dirichletprocess", mdobj)
}

plot_dirichletprocess.default <- function(x, ...){

  if (ncol(x$data) == 1){
    return(plot_dirichletprocess_univariate(x, ...))
  } else {
    return(plot_dirichletprocess_multivariate(x, ...))
  }

}

plot_dirichletprocess.gaussian <- function(x, likelihood = FALSE, single = TRUE){
  plot_dirichletprocess_univariate(x, likelihood = FALSE, single = TRUE)
}

plot_dirichletprocess.beta <- function(x, likelihood = FALSE, single = TRUE) {
  plot_dirichletprocess_univariate(x, likelihood = FALSE, single = TRUE)
}

plot_dirichletprocess.weibull <- function(x, likelihood = FALSE, single = TRUE) {
  plot_dirichletprocess_univariate(x, likelihood = FALSE, single = TRUE)
}

plot_dirichletprocess.mvnormal <- function(x, ...) {
  plot_dirichletprocess_multivariate(x)
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

  graph <- ggplot2::ggplot(data.frame(dt = x$data), ggplot2::aes_(x = ~dt)) +
    ggplot2::theme(axis.title = ggplot2::element_blank())

  if (data_method == "density") {
    graph <- graph + ggplot2::geom_density(fill = data_fill,
                                           bw = ifelse(is.null(data_bw), "nrd0", data_bw))
  } else if (data_method == "hist" | data_method == "histogram") {
    graph <- graph + ggplot2::geom_histogram(ggplot2::aes_(x = ~dt,
                                                           y = ~..density..),
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

  if (single) {
    posteriorFit <- replicate(quant_pts, PosteriorFunction(x)(x_grid))
  } else {
    its <- length(x$alphaChain)
    inds <- round(seq(its/2, 2, length.out = quant_pts))
    posteriorFit <- sapply(inds, function(i) PosteriorFunction(x, i)(x_grid))
  }

  posteriorCI <- apply(posteriorFit, 1,
                       quantile, probs = c(ci_size/2, 0.5, 1 - ci_size/2),
                       na.rm = TRUE)

  graph <- graph + ggplot2::geom_line(data=data.frame(x=x_grid, y=posteriorCI[1,]), ggplot2::aes_(x=~x,y=~y, colour="Posterior"), linetype=2)
  graph <- graph + ggplot2::geom_line(data=data.frame(x=x_grid, y=posteriorCI[2,]), ggplot2::aes_(x=~x,y=~y, colour="Posterior"))
  graph <- graph + ggplot2::geom_line(data=data.frame(x=x_grid, y=posteriorCI[3,]), ggplot2::aes_(x=~x,y=~y, colour="Posterior"), linetype=2)

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

  graph <- ggplot2::ggplot(plotFrame, ggplot2::aes_(x=~x1, y=~x2, colour=~Cluster)) +
    ggplot2::geom_point()
  return(graph)
}
