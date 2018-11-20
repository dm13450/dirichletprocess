source("graph_theme.R")

require(ggplot2)
require(dirichletprocess)


oldfaithful_generate <- function(){
  #Figure 1a and 1b
  its <- 5000
  faithfulTransformed <- scale(faithful$waiting)
  dp <- DirichletProcessGaussian(faithfulTransformed)
  dp <- Fit(dp, its)
  plot(dp) -> oldfaithful_plot_dens
  plot(dp, data_method="hist") -> oldfaithful_plot_hist

  ggsave("../img/old_faithful_density_plot.pdf", oldfaithful_plot_dens, width=10, height=10, device="pdf", units = "cm")
  ggsave("../img/old_faithful_hist_plot.pdf", oldfaithful_plot_hist, width=10, height=10, device="pdf", units = "cm")
}

toybeta_generate <- function(){
  #Figure 2
  y <- c(rbeta(500, 1, 3), rbeta(500, 7, 3)) #generate sample data
  dp <- DirichletProcessBeta(y, 1)
  dp <- Fit(dp, 1000)
  #plot(dp) -> toy_beta

  postDraws <- replicate(1000, PosteriorFunction(dp)(ppoints(100)))
  posteriorMean <- rowMeans(postDraws)
  posteriorQuantiles <- apply(postDraws, 1, quantile, probs=c(0.025, 0.975))
  posteriorFrame <- data.frame(Mean=posteriorMean, t(posteriorQuantiles), x=ppoints(100))

  trueFrame <- data.frame(x=ppoints(100), y=0.5*dbeta(ppoints(100), 1, 3)+0.5*dbeta(ppoints(100), 7, 3))

  ggplot() +
    geom_ribbon(data=posteriorFrame, aes(x=x, ymin=X2.5., ymax=X97.5.), alpha=0.2, colour=NA, fill="red") +
    geom_line(data=posteriorFrame, aes(x=x, y=Mean), colour="red") +
    geom_line(data=trueFrame, aes(x=x, y=y)) -> toy_beta

  ggsave("../img/betaGraph.pdf", toy_beta, width=10, height=10, device="pdf", units = "cm", family="LM Roman 10")

}

faithfulmulti <- function(){
  #Figure 3
  faithfulTrans <- scale(faithful)
  dp <-  DirichletProcessMvnormal(faithfulTrans)
  dp <- Fit(dp, 1000)
  plot(dp) + guides(colour=FALSE) -> faithmulti
  ggsave("../img/faithful_multi_plot.pdf", faithmulti, width = 10, height = 10, device="pdf", units="cm")
}


rats_generate <- function(){
  #Figure 4a and 4b
  ggplot(rats, aes(x=y/N)) + geom_density(fill="black") -> ratsImperical

  numSamples = 2000
  thetaDirichlet <- matrix(nrow=numSamples, ncol=nrow(rats))

  dpobj <- DirichletProcessBeta(rats$y/rats$N,
                                maxY=1, g0Priors = c(2, 150),
                                mhStep=c(0.25, 0.25), hyperPriorParameters = c(1, 1/150))
  dpobj <- Fit(dpobj, 10)

  clusters <- dpobj$clusterParameters

  a <- clusters[[1]] * clusters[[2]]
  b <- (1 - clusters[[1]]) * clusters[[2]]

  for(i in seq_len(numSamples)){
    print(i)
    posteriorA <- a[dpobj$clusterLabels] + rats$y
    posteriorB <- b[dpobj$clusterLabels] + rats$N - rats$y
    thetaDirichlet[i, ] <- rbeta(nrow(rats), posteriorA, posteriorB)

    dpobj <- ChangeObservations(dpobj, thetaDirichlet[i, ])
    dpobj <- Fit(dpobj, 5, progressBar = F)
    clusters <- dpobj$clusterParameters

    a <- clusters[[1]] * clusters[[2]]
    b <- (1 - clusters[[1]]) * clusters[[2]]
  }

  ggplot(rats, aes(x=y/N)) + geom_density(fill="black") -> ratsImperical

  postDraws <- replicate(1000, PosteriorFunction(dpobj)(ppoints(1000)))

  posteriorMean <- rowMeans(postDraws)
  posteriorQuantiles <- apply(postDraws, 1, quantile, probs=c(0.05, 0.95))
  posteriorFrame <- data.frame(Mean=posteriorMean, t(posteriorQuantiles), x=ppoints(1000))

  ggplot() +
    geom_ribbon(data=posteriorFrame, aes(x=x, ymin=X5., ymax=X95.), alpha=0.5) +
    geom_line(data=posteriorFrame, aes(x=x, y=Mean)) +
    xlim(c(0, 0.35)) -> ratsDirichletPrior

  ggsave("../img/ratsDirichletPrior.pdf", ratsDirichletPrior, width=10, height=10, units="cm", device="pdf")
  ggsave("../img/ratsImpericalDistribution.pdf", ratsImperical, width=10, height=10, units="cm", device="pdf")
}

hierarchical_gen <- function(){
  #Figure 5
  mu <- c(0.25, 0.75, 0.4)
  tau <- c(5, 6, 10)
  a <- mu * tau
  b <- (1 - mu) * tau
  y1 <- c(rbeta(500, a[1], b[1]), rbeta(500, a[2], b[2]))
  y2 <- c(rbeta(500, a[1], b[1]), rbeta(500, a[3], b[3]))

  dpobjlist <- DirichletProcessHierarchicalBeta(list(y1, y2), maxY=1,
                                                hyperPriorParameters = c(1, 0.01), mhStepSize = c(0.1, 0.1),
                                                gammaPriors = c(2, 4), alphaPriors = c(2, 4))
  dpobjlist <- Fit(dpobjlist, 500, TRUE)

  postDraws <- lapply(dpobjlist$indDP, function(x) replicate(1000, PosteriorFunction(x)(ppoints(100))))

  postMeans <- lapply(postDraws, rowMeans)
  postQuantiles <- lapply(postDraws, function(x) apply(x, 1, quantile, probs=c(0.025, 0.975)))

  postFrame <- do.call(rbind, lapply(seq_along(postMeans), function(i) data.frame(Mean=postMeans[[i]],
                                                      t(postQuantiles[[i]]),
                                                      x=ppoints(100), ID=i)))

  trueFrame1 <- data.frame(y=0.5*dbeta(ppoints(100), a[1], b[1]) + 0.5*dbeta(ppoints(100), a[2], b[2]), x=ppoints(100), ID=1)
  trueFrame2 <- data.frame(y=0.5*dbeta(ppoints(100), a[1], b[1]) + 0.5*dbeta(ppoints(100), a[3], b[3]), x=ppoints(100), ID=2)
  trueFrame <- rbind(trueFrame1, trueFrame2)

  ggplot() +
    geom_ribbon(data=postFrame, aes(x=x, ymin=X2.5., ymax=X97.5.), alpha=0.2, fill="red", colour=NA) +
    geom_line(data=postFrame, aes(x=x, y=Mean), colour="red") +
    geom_line(data=trueFrame, aes(x=x, y=y)) +
    facet_grid(~ID) -> hierplot

  ggsave("../img/hierBetaGraph.pdf", hierplot, width=20, height=10, units="cm", device="pdf")

}

stickbreaking_gen <- function(){
  #Figure 6
  y <- cumsum(runif(1000))
  pdf <- function(x) sin(x/50)^2
  accept_prob <- pdf(y)
  pts <- sample(y, 500, prob=accept_prob)

  dp <- DirichletProcessBeta(sample(pts, 100), maxY = max(pts)*1.01,
                             alphaPrior = c(2, 0.01))
  dp <- Fit(dp, 100, TRUE)

  for(i in seq_len(2000)){
    lambdaHat <- PosteriorFunction(dp)
    newPts <- sample(pts, 150, prob=lambdaHat(pts))
    newPts[is.infinite(newPts)] <- 1
    newPts[is.na(newPts)] <- 0
    dp <- ChangeObservations(dp, newPts)
    dp <- Fit(dp, 2, progressBar = FALSE)
  }

  postDraws <- replicate(100, PosteriorFunction(dp)(seq(0, max(pts)*1.01, by=0.1)))

  posteriorMean <- rowMeans(postDraws)
  posteriorQuantiles <- apply(postDraws, 1, quantile, probs=c(0.05, 0.95))
  posteriorFrame <- data.frame(Mean=posteriorMean, t(posteriorQuantiles), x=seq(0, max(pts)*1.01, by=0.1))

  trueFrame <- data.frame(y=pdf(seq(0, max(pts)*1.01, by=0.1))/238, x=seq(0, max(pts)*1.01, by=0.1))

  ggplot() +
    geom_ribbon(data=posteriorFrame, aes(x=x, ymin=X5., ymax=X95.), alpha=0.2, fill="red") +
    geom_line(data=posteriorFrame, aes(x=x, y=Mean), colour="red") +
    geom_line(data=trueFrame, aes(x=x, y=y)) + theme_pub() + ylab("h(t)") + xlab("t") -> stickbreakingGraph

  ggsave("../img/poissonStickBreaking.pdf", stickbreakingGraph, width=10, height=10, units="cm", device="pdf")

}

clusterprediction_gen <- function(){
  # Figure 10
  faithfulTrans <- scale(faithful)
  trainIndex <- 1:(nrow(faithfulTrans)-5)

  dp <-  DirichletProcessMvnormal(faithfulTrans[trainIndex, ])
  dp <- Fit(dp, 1000)

  labelPred <- ClusterLabelPredict(dp, faithfulTrans[-trainIndex, ])

  faithfulPlot <- data.frame(faithful, Label=c(dp$clusterLabels, dp$componentIndexes), Test = rep(c(0.95, 1), c(length(trainIndex), 5)))

  faithfulTrainPlot <- data.frame(faithful[trainIndex, ], Label=dp$clusterLabels)
  faithfulTestPlot <- data.frame(faithful[-trainIndex, ], Label=labelPred$componentIndexes)

  faithful_pred_plot <- ggplot() +
    geom_point(data=faithfulTrainPlot, aes(x=eruptions, y=waiting, colour=as.factor(Label)), size=1) +
    geom_point(data=faithfulTestPlot, aes(x=eruptions, y=waiting, colour=as.factor(Label)), shape=17, size=5) +
    guides(colour=FALSE)

}


