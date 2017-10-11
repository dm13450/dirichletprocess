
update_concentration <- function(oldParam, n, nParams, priorParameters){

  x <- rbeta(1, oldParam + 1, n)

  pi1 <- priorParameters[1] + nParams - 1
  pi2 <- n * (priorParameters[2] - log(x))
  pi1 <- pi1/(pi1 + pi2)

  if (runif(1) < pi1) {
    g1 <- rgamma(1, priorParameters[1] + nParams, priorParameters[2] - log(x))
    new_alpha <- g1
  } else {
    g2 <- rgamma(1, priorParameters[1] + nParams - 1, priorParameters[2] - log(x))
    new_alpha <- g2
  }

  new_alpha
  return(new_alpha)
}

UpdateGamma <- function(dpobjlist){

  globalLabels <- lapply(seq_along(dpobjlist$indDP), function(x) match(dpobjlist$indDP[[x]]$clusterParameters[[1]],
                                                                 dpobjlist$globalParameters[[1]]))
  globalParamTable <- data.frame(table(GlobalParam=unlist(globalLabels)))
  globalParamTable$GlobalParam <- as.numeric(levels(globalParamTable$GlobalParam))

  numParams <- nrow(globalParamTable)
  numTables <- sum(globalParamTable$Freq)

  newGamma <- update_concentration(dpobjlist$gamma, numTables, numParams, c(2,4))

  for(i in seq_along(dpobjlist)){
    dpobjlist$gamma <- newGamma
  }
  return(dpobjlist)
}
