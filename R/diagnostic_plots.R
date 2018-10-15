AlphaTraceplot <- function(dpobj, gg = TRUE) {

  if (gg) {
    ggplot2::ggplot(data.frame(Alpha = dpobj$alphaChain,
                               Index = seq_along(dpobj$alphaChain)),
                    ggplot2::aes_string("Index", "Alpha")) +
      ggplot2::geom_line() +
      ggplot2::ggtitle("Traceplot of alpha")
  } else {
    plot(dpobj$alphaChain, type = "l", ylab = "Alpha",
         main = "Traceplot of alpha")
  }
}

AlphaPriorPosteriorPlot <- function(dpobj, prior_color = "#2c7fb8", post_color = "#d95f02", gg = TRUE) {

  dap <- dpobj$alphaPriorParameters
  its <- length(dpobj$alphaChain)

  prior_fun <- function(x) dgamma(x, dap[1], dap[2])

  if (gg) {

    ggplot2::ggplot() +
      ggplot2::geom_histogram(data = data.frame(Alpha = dpobj$alphaChain),
                              mapping = ggplot2::aes_string("Alpha",
                                                            "..density..",
                                                            colour = "'Posterior'", fill = "'Posterior'"),
                              bins = min(its / 10, 100),
                              ) +
      ggplot2::stat_function(fun = prior_fun,
                             mapping = ggplot2::aes_string(colour = "'Prior'")) +
      ggplot2::ggtitle("Prior and posterior of alpha") +
      ggplot2::scale_colour_manual(labels = c("Posterior", "Prior"), values = c(prior_color, post_color), aesthetics = c("colour", "fill"), name = " ")
  } else {
    hist(dpobj$alphaChain, freq = FALSE, breaks = min(its / 10, 100),
         xlab = "Alpha", main = "Prior and posterior of alpha")

    curve(dgamma(x, dap[1], dap[2]), add = TRUE, col = "tomato")
  }
}

ClusterTraceplot <- function(dpobj, gg = TRUE) {

  n_clust <- sapply(dpobj$labelsChain, function(x) length(unique(x)))

  if (gg) {
    ggplot2::ggplot(data.frame(nclust = n_clust,
                               Index = seq_along(n_clust)),
                    ggplot2::aes_string("Index", "nclust")) +
      ggplot2::geom_line() +
      ggplot2::ylab("Number of clusters") +
      ggplot2::ggtitle("Traceplot of the number of clusters")
  } else {
    plot(n_clust, type = "l", ylab = "Number of clusters",
         main = "Traceplot of the number of clusters")
  }
}

LikelihoodTraceplot <- function(dpobj, gg = TRUE) {
  if (gg) {
    ggplot2::ggplot(data.frame(Lik = dpobj$likelihoodChain,
                               Index = seq_along(dpobj$likelihoodChain)),
                    ggplot2::aes_string("Index", "Lik")) +
      ggplot2::geom_line() +
      ggplot2::ylab("Log-likelihood") +
      ggplot2::ggtitle("Traceplot of the log-likelihood")
  } else {
    plot(dpobj$likelihoodChain, type = "l", ylab = "Log-likelihood",
         main = "Traceplot of the log-likelihood")
  }
}

DiagnosticPlots <- function(dpobj) {
  oldpar <- par()
  par(mfrow = c(2, 2))

  if ("alphaChain"  %in% names(dpobj)) AlphaTraceplot(dpobj, gg = FALSE)
  if ("alphaChain"  %in% names(dpobj)) AlphaPriorPosteriorPlot(dpobj, gg = FALSE)
  if ("labelsChain" %in% names(dpobj)) ClusterTraceplot(dpobj, gg = FALSE)
  if ("likelihoodChain" %in% names(dpobj)) LikelihoodTraceplot(dpobj, gg = FALSE)

  suppressWarnings(par(oldpar))

}

