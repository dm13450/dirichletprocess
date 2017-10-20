#' Plot the Dirichlet Process
#'
#' @param x Dirichlet Process Object to plot
#' @param likelihood Logical, indicating whether to plot the likelihood from the dpobj.
#' @param single Logical, indicating whether to draw the posterior from the last iteration or use the full cluster sequence.
#' @param y Consistency for generic.
#' @param ... Graphical arguments
#' @export
plot.dirichletprocess <- function(x, likelihood = FALSE, single = TRUE, y=NULL,...) {

  graph <- ggplot2::ggplot(data.frame(dt = x$data), ggplot2::aes(x = dt)) + ggplot2::geom_density(fill = "black") +
    ggplot2::theme(axis.title = ggplot2::element_blank())

  x_grid <- pretty(x$data, n=100)

  if (single){
    posteriorFit <- replicate(100, PosteriorFunction(x)(x_grid))
  } else {
    its <- length(x$alphaChain)
    inds <- round(seq(its/2, 2, length.out = 100))
    posteriorFit <- sapply(inds, function(i) PosteriorFunction(x, i)(x_grid))
  }

  posteriorCI <- apply(posteriorFit, 1, quantile, c(0.025, 0.5 ,0.975), na.rm=TRUE)

  graph <- graph + ggplot2::geom_line(data=data.frame(x=x_grid, y=posteriorCI[1,]), ggplot2::aes(x=x,y=y, colour="Posterior"), linetype=2)
  graph <- graph + ggplot2::geom_line(data=data.frame(x=x_grid, y=posteriorCI[2,]), ggplot2::aes(x=x,y=y, colour="Posterior"))
  graph <- graph + ggplot2::geom_line(data=data.frame(x=x_grid, y=posteriorCI[3,]), ggplot2::aes(x=x,y=y, colour="Posterior"), linetype=2)

  if (likelihood) {
    graph <- graph + ggplot2::stat_function(fun = function(z) LikelihoodFunction(x)(z),
                                            n = 1000, ggplot2::aes(colour = "Likelihood"))
  }
  else {
    graph <- graph + ggplot2::guides(colour=FALSE)
  }

  return(graph)
}
