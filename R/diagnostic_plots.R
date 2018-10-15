AlphaTraceplot <- function(dpobj) {
  plot(dpobj$alphaChain, type = "l", ylab = "Alpha",
       main = "Traceplot of alpha")
}

AlphaPriorPosteriorPlot <- function(dpobj) {
  its <- length(dpobj$alphaChain)
  hist(dpobj$alphaChain, freq = FALSE, breaks = min(its / 10, 100),
       xlab = "Alpha", main = "Prior and posterior of alpha")

  dap <- dpobj$alphaPriorParameters
  curve(dgamma(x, dap[1], dap[2]), add = TRUE, col = "tomato")
}

NClustPlot <- function(dpobj) {
  n_clust <- sapply(dpobj$labelsChain, function(x) length(unique(x)))
  plot(n_clust, type = "l", ylab = "Number of clusters",
       main = "Traceplot of the number of clusters")
}

LikelihoodTraceplot <- function(dpobj) {
  plot(dpobj$likelihoodChain, type = "l", ylab = "Likelihood",
       main = "Traceplot of the likelihood")
}

DiagnosticPlots <- function(dpobj) {
  oldpar <- par()
  par(mfrow = c(2, 2))

  if ("alphaChain"  %in% names(dpobj)) AlphaTraceplot(dpobj)
  if ("alphaChain"  %in% names(dpobj)) AlphaPriorPosteriorPlot(dpobj)
  if ("labelsChain" %in% names(dpobj)) NClustPlot(dpobj)
  if ("likelihoodChain" %in% names(dpobj)) LikelihoodTraceplot(dpobj)

  suppressWarnings(par(oldpar))

}

