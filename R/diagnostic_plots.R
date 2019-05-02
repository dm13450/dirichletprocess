
#' Diagnostic plots for dirichletprocess objects
#'
#' Plot several diagnostic plots for dirichletprocess objects. Because the
#' dimension of the dirichletprocess mixture is constantly changing, it is not
#' simple to create meaningful plots of the sampled parameters. Therefore, the
#' plots focus on the likelihood, alpha, and the number of clusters.
#'
#' @param dpobj A dirichletprocess object that was fit.
#' @param gg Logical; whether to create a ggplot or base R plot (if \code{gg =
#'   FALSE}). For \code{DiagnosticPlots}, this means that the plots will be
#'   given one-by-one, while base plots can be arranged in a grid.
#' @param prior_color For \code{AlphaPriorPosteriorPlot}, the color of the prior
#'   function.
#' @param post_color  For \code{AlphaPriorPosteriorPlot}, the color of the
#'   posterior histogram.
#'
#' @return If \code{gg = TRUE}, a ggplot2 object. Otherwise, nothing is returned
#'   and a base plot is plotted.
#' @export
#'
#' @examples
#' dp <- Fit(DirichletProcessGaussian(rnorm(10)), 100)
#' DiagnosticPlots(dp)
#'
DiagnosticPlots <- function(dpobj, gg = FALSE) {
  oldpar <- graphics::par()
  graphics::par(mfrow = c(2, 2))

  if ("alphaChain"  %in% names(dpobj)) AlphaTraceplot(dpobj, gg = gg)
  if ("alphaChain"  %in% names(dpobj)) AlphaPriorPosteriorPlot(dpobj, gg = gg)
  if ("labelsChain" %in% names(dpobj)) ClusterTraceplot(dpobj, gg = gg)
  if ("likelihoodChain" %in% names(dpobj)) LikelihoodTraceplot(dpobj, gg = gg)

  suppressWarnings(graphics::par(mfrow = oldpar$mfrow))

}



#' @export
#' @describeIn  DiagnosticPlots Trace plot of alpha.
AlphaTraceplot <- function(dpobj, gg = TRUE) {

  if (gg) {
    p <- ggplot2::ggplot(data.frame(Alpha = dpobj$alphaChain,
                               Index = seq_along(dpobj$alphaChain)),
                    ggplot2::aes_string("Index", "Alpha")) +
      ggplot2::geom_line() +
      ggplot2::ggtitle("Traceplot of alpha")
    return(p)
  } else {
    graphics::plot(dpobj$alphaChain, type = "l", ylab = "Alpha",
         main = "Traceplot of alpha")
  }
}


#' @export
#' @describeIn  DiagnosticPlots Plot of the prior and posterior of alpha.
AlphaPriorPosteriorPlot <- function(dpobj, prior_color = "#2c7fb8", post_color = "#d95f02", gg = TRUE) {

  dap <- dpobj$alphaPriorParameters
  its <- length(dpobj$alphaChain)

  prior_fun <- function(x) dgamma(x, dap[1], dap[2])

  if (gg) {

    p <- ggplot2::ggplot() +
      ggplot2::geom_histogram(data = data.frame(Alpha = dpobj$alphaChain),
                              mapping = ggplot2::aes_string("Alpha",
                                                            "..density..",
                                                            colour = "'Posterior'", fill = "'Posterior'"),
                              bins = min(its / 10, 100)) +
      ggplot2::stat_function(fun = prior_fun,
                             mapping = ggplot2::aes_string(colour = "'Prior'")) +
      ggplot2::ggtitle("Prior and posterior of alpha") +
      ggplot2::scale_colour_manual(labels = c("Posterior", "Prior"), values = c(prior_color, post_color), aesthetics = c("colour", "fill"), name = " ")
    return(p)
  } else {
    graphics::hist(dpobj$alphaChain, freq = FALSE, breaks = min(its / 10, 100),
         xlab = "Alpha", main = "Prior and posterior of alpha")

    thisdgam <- function(x) dgamma(x, dap[1], dap[2])
    graphics::curve(thisdgam, add = TRUE, col = "tomato")
  }
}


#' @export
#' @describeIn  DiagnosticPlots Trace plot of the number of clusters.
ClusterTraceplot <- function(dpobj, gg = TRUE) {

  n_clust <- sapply(dpobj$labelsChain, function(x) length(unique(x)))

  if (gg) {
    p <- ggplot2::ggplot(data.frame(nclust = n_clust,
                               Index = seq_along(n_clust)),
                    ggplot2::aes_string("Index", "nclust")) +
      ggplot2::geom_line() +
      ggplot2::ylab("Number of clusters") +
      ggplot2::ggtitle("Traceplot of the number of clusters")
    return(p)
  } else {
    graphics::plot(n_clust, type = "l", ylab = "Number of clusters",
         main = "Traceplot of the number of clusters")
  }
}



#' @export
#' @describeIn  DiagnosticPlots Trace plot of the likelihood of the data for
#'   each iteration.
LikelihoodTraceplot <- function(dpobj, gg = TRUE) {
  if (gg) {
    p <- ggplot2::ggplot(data.frame(Lik = dpobj$likelihoodChain,
                               Index = seq_along(dpobj$likelihoodChain)),
                    ggplot2::aes_string("Index", "Lik")) +
      ggplot2::geom_line() +
      ggplot2::ylab("Log-likelihood") +
      ggplot2::ggtitle("Traceplot of the log-likelihood")
    return(p)
  } else {
    graphics::plot(dpobj$likelihoodChain, type = "l", ylab = "Log-likelihood",
         main = "Traceplot of the log-likelihood")
  }
}
