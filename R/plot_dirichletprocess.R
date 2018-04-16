plot_dirichletprocess <- function(dpobj, likelihood = FALSE, single = TRUE) {
  mdobj <- dpobj$mixingDistribution
  UseMethod("plot_dirichletprocess", mdobj)
}

plot_dirichletprocess.default <- function(dpobj, likelihood = FALSE, single = TRUE){

  if(ncol(dpobj$data) == 1){
    return(plot_dirichletprocess_univariate(dpobj, likelihood = FALSE, single = TRUE))
  } else {
    return(plot_dirichletprocess_multivariate(dpobj))
  }

}

plot_dirichletprocess.gaussian <- function(dpobj, likelihood = FALSE, single = TRUE){
  plot_dirichletprocess_univariate(dpobj, likelihood = FALSE, single = TRUE)
}

plot_dirichletprocess.beta <- function(dpobj, likelihood = FALSE, single = TRUE) {
  plot_dirichletprocess_univariate(dpobj, likelihood = FALSE, single = TRUE)
}

plot_dirichletprocess.weibull <- function(dpobj, likelihood = FALSE, single = TRUE) {
  plot_dirichletprocess_univariate(dpobj, likelihood = FALSE, single = TRUE)
}

plot_dirichletprocess.mvnormal <- function(dpobj, likelihood = FALSE, single = TRUE) {
  plot_dirichletprocess_multivariate(dpobj, likelihood = FALSE, single = TRUE)
}

plot_dirichletprocess_univariate <- function(dpobj, likelihood = FALSE, single = TRUE) {

  graph <- ggplot2::ggplot(data.frame(dt = dpobj$data), ggplot2::aes_(x = ~dt)) +
    ggplot2::geom_density(fill = "black") +
    ggplot2::theme(axis.title = ggplot2::element_blank())

  x_grid <- pretty(dpobj$data, n=100)

  if (single){
    posteriorFit <- replicate(100, PosteriorFunction(dpobj)(x_grid))
  } else {
    its <- length(dpobj$alphaChain)
    inds <- round(seq(its/2, 2, length.out = 100))
    posteriorFit <- sapply(inds, function(i) PosteriorFunction(dpobj, i)(x_grid))
  }

  posteriorCI <- apply(posteriorFit, 1, quantile, c(0.025, 0.5 ,0.975), na.rm=TRUE)

  graph <- graph + ggplot2::geom_line(data=data.frame(x=x_grid, y=posteriorCI[1,]), ggplot2::aes_(x=~x,y=~y, colour="Posterior"), linetype=2)
  graph <- graph + ggplot2::geom_line(data=data.frame(x=x_grid, y=posteriorCI[2,]), ggplot2::aes_(x=~x,y=~y, colour="Posterior"))
  graph <- graph + ggplot2::geom_line(data=data.frame(x=x_grid, y=posteriorCI[3,]), ggplot2::aes_(x=~x,y=~y, colour="Posterior"), linetype=2)

  if (likelihood) {
    graph <- graph + ggplot2::stat_function(fun = function(z) LikelihoodFunction(dpobj)(z),
                                            n = 1000, ggplot2::aes(colour = "Likelihood"))
  }
  else {
    graph <- graph + ggplot2::guides(colour=FALSE)
  }

  return(graph)
}

plot_dirichletprocess_multivariate <- function(dpobj, likelihood = FALSE, single = TRUE) {

  plotFrame <- data.frame(x1=dpobj$data[,1], x2=dpobj$data[,2], Cluster=as.factor(dpobj$clusterLabel))

  graph <- ggplot2::ggplot(plotFrame, ggplot2::aes_(x=~x1, y=~x2, colour=~Cluster)) + ggplot2::geom_point()
  return(graph)
}
