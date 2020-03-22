
UpdateG0 <- function(dpobjlist){

  globalParams <- dpobjlist$globalParameters

  globalLabels <- lapply(seq_along(dpobjlist$indDP),
                         function(x) match(dpobjlist$indDP[[x]]$clusterParameters[[1]],
                                           globalParams[[1]]))

  for (i in seq_along(globalLabels)){
    globalLabels[[i]] <- unique(globalLabels[[i]])
    if (length(globalLabels[[i]]) > 1) {
      globalLabels[[i]] <- true_cluster_labels(globalLabels[[i]], dpobjlist)
    }
  }
  
  globalParamTable <- data.frame(table(GlobalParam=unlist(globalLabels)))
  globalParamTable$GlobalParam <- as.numeric(levels(globalParamTable$GlobalParam))

  #globalParams <- dpobjlist$globalParameters

  numTables <- nrow(globalParamTable)
  dirichlet_draws <- gtools::rdirichlet(1, c(globalParamTable$Freq, dpobjlist$gamma))
  numBreaks <- ceiling(dpobjlist$gamma + numTables) * 20 + 5

  sticks <- StickBreaking(dpobjlist$gamma + numTables, numBreaks)
  sticks <- sticks * dirichlet_draws[numTables + 1]
  sticks <- c(dirichlet_draws[-(numTables + 1)], sticks)

  if (class(dpobjlist$indDP[[1]]$mixingDistribution)[[2]] == "beta") {
    priorDraws <- PriorDraw.beta(dpobjlist$indDP[[1]]$mixingDistribution, numBreaks)
  } else if (class(dpobjlist$indDP[[1]]$mixingDistribution)[[2]] == "mvnormal2") {
    priorDraws <- PriorDraw.mvnormal2(dpobjlist$indDP[[1]]$mixingDistribution, numBreaks)
  } else {
    stop(paste("The hierarchical fit is not implemented for ",
               class(dpobjlist$indDP[[1]]$mixingDistribution)[[2]],
               " mixing distribution object"))
  }
  postParams <- list()

  for (i in seq_along(priorDraws)) {
    postParams[[i]] <- array(c(globalParams[[i]][,,globalParamTable$GlobalParam], priorDraws[[i]]), dim=c(dim(priorDraws[[i]])[1:2],numBreaks+numTables))
  }
  
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


