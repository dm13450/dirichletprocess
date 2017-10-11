
UpdateG0 <- function(dpobjlist){

  globalParams <- dpobjlist$globalParameters

  globalLabels <- lapply(seq_along(dpobjlist$indDP), function(x) match(dpobjlist$indDP[[x]]$clusterParameters[[1]],
                                                                 globalParams[[1]]))

  globalParamTable <- data.frame(table(GlobalParam=unlist(globalLabels)))
  globalParamTable$GlobalParam <- as.numeric(levels(globalParamTable$GlobalParam))

  globalParams <- dpobjlist$globalParameters

  numTables <- nrow(globalParamTable)
  dirichlet_draws <- gtools::rdirichlet(1, c(globalParamTable$Freq, dpobjlist$gamma))
  numBreaks <- ceiling(dpobjlist$gamma + numTables) * 20 + 5

  sticks <- StickBreaking(dpobjlist$gamma + numTables, numBreaks)
  sticks <- sticks * dirichlet_draws[numTables + 1]
  sticks <- c(dirichlet_draws[-(numTables + 1)], sticks)

  priorDraws <- PriorDraw.beta(dpobjlist$indDP[[1]]$mixingDistribution, numBreaks)
  postParams <- list()

  postParams[[1]] <- array(c(globalParams[[1]][,,globalParamTable$GlobalParam], priorDraws[[1]]), dim=c(1,1,numBreaks+numTables))
  postParams[[2]] <- array(c(globalParams[[2]][,,globalParamTable$GlobalParam], priorDraws[[2]]), dim=c(1,1,numBreaks+numTables))

  for(i in seq_along(dpobjlist$indDP)){
    newGJ <- draw_gj(dpobjlist$indDP[[i]]$mixingDistribution$alpha, sticks)
    newGJ[is.na(newGJ)] <- 0
    dpobjlist$indDP[[i]]$mixingDistribution$pi_k <- newGJ
    dpobjlist$indDP[[i]]$mixingDistribution$theta_k <- postParams
  }

  dpobjlist$globalParameters <- postParams
  dpobjlist$globalStick <- sticks

  return(dpobjlist)
}


