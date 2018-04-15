require(microbenchmark)

dpobj <- DirichletProcessBeta(rbeta(15000, 2, 3), 1)

newData <- matrix(rbeta(15000, 2, 3), ncol=1)

alpha <- dpobj$alpha

# clusterLabels <- dpobj$clusterLabels
clusterParams <- dpobj$clusterParameters
numLabels <- dpobj$numberClusters
mdobj <- dpobj$mixingDistribution

pointsPerCluster <- dpobj$pointsPerCluster

m = 3
i = 1

aux <- PriorDraw(mdobj, m)

dataVal <- newData[i, , drop = FALSE]

f1 <- function(){
weights <- numeric(numLabels + 1)
weights[1:numLabels] <- pointsPerCluster * Likelihood(mdobj, dataVal, clusterParams)
weights[(numLabels + 1):(numLabels + m)] <- (alpha/m) * Likelihood(mdobj,
                                                                   dataVal, aux)
return(weights)
}


f2 <- function(){
  weights<- c(pointsPerCluster * Likelihood(mdobj, dataVal, clusterParams),
              (alpha/m) * Likelihood(mdobj,dataVal, aux))
  return(weights)
}


mb <- microbenchmark(f1(), f2(), times=1e6)



