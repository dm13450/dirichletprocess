pois_custom <- function(){
 #Figure 7

  Likelihood.poisson <- function(mdobj, x, theta){
    return(as.numeric(dpois(x, theta[[1]])))
  }

  PriorDraw.poisson <- function(mdobj, n){
    draws <- rgamma(n, mdobj$priorParameters[1], mdobj$priorParameters[2])
    theta <- list(array(draws, dim=c(1,1,n)))
    return(theta)
  }

  PosteriorDraw.poisson <- function(mdobj, x, n=1){
    priorParameters <- mdobj$priorParameters
    lambda <- rgamma(n, priorParameters[1] + sum(x), priorParameters[2] + nrow(x))
    return(list(array(lambda, dim=c(1,1,n))))
  }

  Predictive.poisson <- function(mdobj, x){
    priorParameters <- mdobj$priorParameters
    pred <- numeric(length(x))
    for(i in seq_along(x)){
      alphaPost <- priorParameters[1] + x[i]
      betaPost <- priorParameters[2] + 1
      pred[i] <- (priorParameters[2] ^  priorParameters[1]) / gamma(priorParameters[1])
      pred[i] <- pred[i] * gamma(alphaPost) / (betaPost^alphaPost)
      pred[i] <- pred[i] * (1 / prod(factorial(x[i])))
    }
    return(pred)
  }

  poisMd <- MixingDistribution(distribution="poisson", priorParameters = c(1, 1), conjugate="conjugate")

  y <- c(rpois(250, 3), rpois(250, 10)) #generate sample data
  dp <- DirichletProcessCreate(y, poisMd)
  dp <- Initialise(dp)
  dp <- Fit(dp, 2500)

  pf <- PosteriorFrame(dp, 0:20, 1000)

  trueFrame <- data.frame(x=0:20,
                          y= 0.5*dpois(0:20, 3) + 0.5*dpois(0:20, 10))

  ggplot() +
    geom_ribbon(data=pf, aes(x=x, ymin=X5., ymax=X95.), colour=NA, fill="red", alpha=0.2) +
    geom_line(data=pf, aes(x=x, y=Mean), colour="red") +
    geom_line(data=trueFrame, aes(x=x, y=y)) + theme_pub() -> poissonPlot

  ggsave("../img/poisson_mixture_plot.pdf", poissonPlot, width=10, height=10, units="cm", device="pdf")

}

gamma_custom <- function(){

  Likelihood.gamma <- function(mdobj, x, theta){
    return(as.numeric(dgamma(x, theta[[1]], theta[[2]])))
  }

  PriorDraw.gamma <- function(mdobj, n=1){
    theta <- list()
    theta[[1]] = array(rexp(n, mdobj$priorParameters[1]), dim=c(1,1, n))
    theta[[2]] = array(rexp(n, mdobj$priorParameters[2]), dim=c(1,1, n))
    return(theta)
  }

  PriorDensity.gamma <- function(mdobj, theta){
    priorParameters <- mdobj$priorParameters
    thetaDensity <- dexp(theta[[1]], priorParameters[1])
    thetaDensity <- thetaDensity * dexp(theta[[2]], priorParameters[2])
    return(as.numeric(thetaDensity))
  }

  MhParameterProposal.gamma <- function(mdobj, oldParams){
    mhStepSize <- mdobj$mhStepSize
    newParams <- oldParams
    newParams[[1]] <- abs(oldParams[[1]] + mhStepSize[1]*rnorm(1))
    newParams[[2]] <- abs(oldParams[[2]] + mhStepSize[2]*rnorm(1))
    return(newParams)
  }

  gammaMd <- MixingDistribution(distribution = "gamma",
                                priorParameters = c(0.1, 0.1),
                                conjugate = "nonconjugate",
                                mhStepSize = c(0.1, 0.1))

  y <- c(rgamma(500, 2, 4), rgamma(500, 6, 3)) #generate sample data
  dp <- DirichletProcessCreate(y, gammaMd)
  dp <- Initialise(dp)
  dp <- Fit(dp, 1000)
  plot(dp)


  pf <- PosteriorFrame(dp, ppoints(100)*6, 1000)

  trueFrame <- data.frame(x=ppoints(100)*6,
                          y= 0.5*dgamma(ppoints(100)*6, 2, 4) + 0.5*dgamma(ppoints(100)*6, 6, 3))

  ggplot() +
    geom_ribbon(data=pf, aes(x=x, ymin=X5., ymax=X95.), colour=NA, fill="red", alpha=0.2) +
    geom_line(data=pf, aes(x=x, y=Mean), colour="red") +
    geom_line(data=trueFrame, aes(x=x, y=y)) + theme_pub() -> gammaPlot


  ggsave("../img/gamma_mixture_plot.pdf", gammaPlot, width=10, height=10, units="cm", device="pdf")

}




