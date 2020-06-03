#' Create a Hierarchical Dirichlet Mixture of
#' semi-conjugate Multivariate Normal Distributions
#'
#' @param dataList List of data for each separate Dirichlet mixture object
#' @param g0Priors Prior Parameters for the top level base distribution.
#' @param gammaPriors Prior parameters for the top level concentration parameter.
#' @param alphaPriors Prior parameters for the individual parameters.
#' @param numSticks Truncation level for the Stick Breaking formulation.
#' @param numInitialClusters Number of clusters to initialise with.
#' @param mhDraws Number of Metropolis-Hastings samples to perform for each cluster update.
#' @return dpobjlist A Hierarchical Dirichlet Process object that can be fitted, plotted etc.
#' @export
DirichletProcessHierarchicalMvnormal2 <- function(dataList,
                                                  g0Priors,
                                                  gammaPriors = c(2,4), alphaPriors = c(2, 4),
                                                  numSticks = 50,
                                                  numInitialClusters = 1,
                                                  mhDraws=250) {


  # for(i in dataList) {
  #   if(!is.matrix(i)){
  #     i <- matrix(i, ncol=length(i))
  #   }
  # }
  #
  if(missing(g0Priors)){
    g0Priors <- list(nu0 = 2,
                     phi0 = diag(ncol(dataList[[1]])),
                     mu0 = numeric(ncol(dataList[[1]])),
                     sigma0 = diag(ncol(dataList[[1]])))
  }


  mdobj_list <- HierarchicalMvnormal2Create(n=length(dataList), priorParameters=g0Priors,
                                            gammaPrior=gammaPriors,
                                            alphaPrior = alphaPriors, num_sticks=numSticks)

  dpobjlist <- list()
  dpobjlist$indDP <- lapply(seq_along(dataList),
                            function(x) DirichletProcessCreate(dataList[[x]], mdobj_list[[x]], alphaPriors, mhDraws))

  dpobjlist$indDP <- lapply(dpobjlist$indDP, Initialise, posterior=FALSE)

  for(i in seq_along(dpobjlist$indDP)){
    dpobjlist$indDP[[i]]$alpha <- dpobjlist$indDP[[i]]$mixingDistribution$alpha
  }

  dpobjlist$globalParameters <- mdobj_list[[1]]$theta_k
  dpobjlist$globalStick <- mdobj_list[[1]]$beta_k
  dpobjlist$gamma <- mdobj_list[[1]]$gamma
  dpobjlist$gammaPriors <- gammaPriors
  class(dpobjlist) <- c("list", "dirichletprocess", "hierarchical")

  return(dpobjlist)
}
