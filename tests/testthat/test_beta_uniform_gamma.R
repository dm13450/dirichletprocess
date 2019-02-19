context("Beta Mixing Distribution Tests")

test_mdobj <- BetaMixtureCreate(c(1,1), mhStepSize = c(0.1, 0.1), 1)

test_that("Beta Mixture Create", {

  expect_is(test_mdobj, c("list", "beta", "nonconjugate"))
})


test_that("Beta Mixture Likelihood Numerical", {

  maxT <- 10

  test_mdobj <- BetaMixtureCreate(maxT=maxT)

  mu <- 5
  tau <- 4

  a <- (mu*tau)/maxT
  b <- (1-mu/maxT)*tau

  thetaTest <- list(array(mu, dim=c(1,1,length(mu))), array(tau, dim=c(1,1,length(tau))))

  xTest <- seq(0, maxT, by=0.1)

  mdLikeTest <- Likelihood(test_mdobj, xTest, thetaTest)
  trueLike <- 1/maxT * dbeta(xTest/maxT, a, b)

  expect_equal(mdLikeTest, trueLike)

})

test_that("Beta Mixture Likelihood", {

  test_params_single <- list(mu=array(0.5, dim=c(1,1,1)), nu=array(1, dim=c(1,1,1)))

  test_parameters_2 <- list(mu=array(c(0.5, 0.75), dim=c(1,1,2)), nu=array(c(1,2), dim=c(1,1,2)))

  expect_equal(Likelihood(test_mdobj, 0.5, test_params_single), dbeta(0.5, 0.5,0.5))
  expect_equal(length(Likelihood(test_mdobj, 0.5, test_parameters_2)), 2)
  expect_equal(Likelihood(test_mdobj, 0.5, test_parameters_2), dbeta(0.5, c(0.5, 1.5), c(0.5, 0.5)))
})

test_that("Beta Prior Density", {
  test_params_single <- list(mu=array(0.5, dim=c(1,1,1)), nu=array(1, dim=c(1,1,1)))

  expect_is(PriorDensity(test_mdobj, test_params_single), "numeric")
  expect_equal(PriorDensity(test_mdobj, test_params_single), dunif(0.5, 0, 1)*dgamma(1,1,1))
})

test_that("Beta Prior Draw", {
  expect_is(PriorDraw(test_mdobj, 10), "list")
  expect_equal(length(PriorDraw(test_mdobj, 10)), c(2))
  expect_equal(dim(PriorDraw(test_mdobj, 10)$mu), c(1,1,10))
})

test_that("Beta Posterior Draw", {

  post_draws <- PosteriorDraw(test_mdobj, x=matrix(rbeta(10, 1,1),ncol=1), n=10)

  expect_is(post_draws, "list")
  expect_length(post_draws, 2)
  expect_equal(dim(post_draws[[1]]), c(1,1,10))
  expect_equal(dim(post_draws[[2]]), c(1,1,10))

})

test_that("Beta Posterior Draw with Start Position", {

  start_pos_test <- list(mu=array(1, dim=c(1,1,1)), nu=array(1, dim=c(1,1,1)))

  postDraws <- PosteriorDraw(test_mdobj, rbeta(10, 1,1), 10, start_pos = start_pos_test)

  expect_is(postDraws, "list")
  expect_length(postDraws, 2)
  expect_equal(c(postDraws[[1]])[1], c(start_pos_test[[1]])[1])
  expect_equal(c(postDraws[[2]])[1], c(start_pos_test[[2]])[1])

})


test_that("Beta MH Parameter Proposal", {
  test_params_single <- list(mu=array(0.5, dim=c(1,1,1)), nu=array(1, dim=c(1,1,1)))

  test_param_prop <- MhParameterProposal(test_mdobj, test_params_single)

  expect_equal(length(test_param_prop), 2)
  expect_equal(length(test_param_prop$mu), 1)
  expect_equal(length(test_param_prop$nu), 1)

})

test_that("Beta Prior Parameters Update", {

  pts <- rbeta(10, 2,2)
  beta_md_obj <- BetaMixtureCreate(c(2,8), c(1, 1), 1, hyperPriorParameters=c(1, 0.125))

  cluster_params_test <- list(mu=array(c(0.5, 0.75), dim=c(1,1,2)), nu=array(c(1,2), dim=c(1,1,2)))

  beta_md_obj <- PriorParametersUpdate(beta_md_obj, cluster_params_test)

  expect_is(beta_md_obj$priorParameters, "matrix")
  expect_equal(dim(beta_md_obj$priorParameters), c(1,2))
})

test_that("Beta Dirichlet Process Create", {

  pts <- rbeta(10, 2,2)
  beta_dpobj <- DirichletProcessBeta(pts, 1, verbose = FALSE)

  expect_is(beta_dpobj, c("list", "dirichletprocess", "beta", "nonconjugate"))
  expect_equal(beta_dpobj$data, as.matrix(pts))
})

test_that("Beta Dirichlet Process Initialise", {

  pts <- rbeta(10, 2,2)
  beta_md_obj <- BetaMixtureCreate(c(2,8), c(1, 1), 1, hyperPriorParameters=c(1, 0.125))
  beta_dpobj <- DirichletProcessCreate(pts, beta_md_obj, c(2,4))
  beta_dpobj <- Initialise(beta_dpobj, verbose=FALSE)

  expect_is(beta_dpobj, c("list", "dirichletprocess", "beta", "nonconjugate"))
  expect_equal(beta_dpobj$data, as.matrix(pts))

  expect_length(beta_dpobj$clusterParameters, 2)
  expect_length(beta_dpobj$clusterParameters[[1]], 1)
  expect_length(beta_dpobj$clusterParameters[[2]], 1)

  expect_equal(beta_dpobj$clusterLabels, rep(1, 10))

})

test_that("Beta Component Update", {

  pts <- rbeta(10, 2,2)
  beta_dpobj <- DirichletProcessBeta(pts, 1, verbose = FALSE)
  beta_dpobj <- ClusterComponentUpdate(beta_dpobj)

  expect_length(beta_dpobj$clusterParameters, 2)
  expect_length(beta_dpobj$clusterLabels, 10)
  expect_equal(sum(beta_dpobj$pointsPerCluster), 10)
})

test_that("Beta Component Parameter Update 1 Cluster", {
  pts <- rbeta(10, 2,2)
  beta_md_obj <- BetaMixtureCreate(c(2,8), c(1, 1), 1, hyperPriorParameters=c(1, 0.125))
  beta_dpobj <- DirichletProcessCreate(pts, beta_md_obj, c(2,4))
  beta_dpobj <- Initialise(beta_dpobj, verbose=FALSE)
  beta_dpobj <- ClusterParameterUpdate(beta_dpobj)

  expect_length(beta_dpobj$clusterParameters, 2)
  expect_length(beta_dpobj$clusterLabels, 10)
  expect_equal(sum(beta_dpobj$pointsPerCluster), 10)
})

test_that("Beta Component Parameter Update 2 Clusters", {
  pts <- rbeta(10, 2,2)
  beta_md_obj <- BetaMixtureCreate(c(2,8), c(1, 1), 1, hyperPriorParameters=c(1, 0.125))
  beta_dpobj <- DirichletProcessCreate(pts, beta_md_obj, c(2,4))
  beta_dpobj <- Initialise(beta_dpobj, verbose=FALSE)

  beta_dpobj$clusterLabels <- c(1, rep.int(2, 9))
  beta_dpobj$numberClusters <- 2
  beta_dpobj$pointsPerCluster <- c(1,9)
  beta_dpobj$clusterParameters <- PriorDraw(beta_dpobj$mixingDistribution, 2)

  beta_dpobj <- ClusterParameterUpdate(beta_dpobj)

  expect_length(beta_dpobj$clusterParameters, 2)
  expect_length(beta_dpobj$clusterParameters[[1]], 2)
  expect_length(beta_dpobj$clusterParameters[[2]], 2)
  expect_length(beta_dpobj$clusterLabels, 10)
  expect_equal(sum(beta_dpobj$pointsPerCluster), 10)
})


test_that("Beta Cluster Label Change D", {
  pts <- rbeta(10, 2,2)
  beta_md_obj <- BetaMixtureCreate(c(2,8), c(1, 1), 1, hyperPriorParameters=c(1, 0.125))
  beta_dpobj <- DirichletProcessCreate(pts, beta_md_obj, c(2,4))
  beta_dpobj <- Initialise(beta_dpobj, verbose=FALSE)

  clusterLabels <- beta_dpobj$clusterLabels
  pointsPerCluster <- beta_dpobj$pointsPerCluster

  currentLabel <- clusterLabels[1]
  pointsPerCluster[currentLabel] <- pointsPerCluster[currentLabel] - 1

  beta_dpobj$pointsPerCluster <- pointsPerCluster

  aux <- PriorDraw(beta_md_obj, 3)

  beta_dpobj = ClusterLabelChange(beta_dpobj, 1, 2, currentLabel, aux)

  expect_equal(beta_dpobj$numberClusters, 2)
  expect_equal(sum(beta_dpobj$pointsPerCluster), 10)
  expect_length(beta_dpobj$clusterParameters[[1]], 2)
  expect_length(beta_dpobj$clusterParameters[[2]], 2)
})

test_that("Beta Cluster Label Change C", {
  pts <- rbeta(10, 2,2)
  beta_md_obj <- BetaMixtureCreate(c(2,8), c(1, 1), 1, hyperPriorParameters=c(1, 0.125))
  beta_dpobj <- DirichletProcessCreate(pts, beta_md_obj, c(2,4))
  beta_dpobj <- Initialise(beta_dpobj, verbose=FALSE)

  beta_dpobj$clusterLabels <- c(1, rep.int(2, 9))
  beta_dpobj$numberClusters <- 2
  beta_dpobj$pointsPerCluster <- c(1,9)
  beta_dpobj$clusterParameters <- PriorDraw(beta_dpobj$mixingDistribution, 2)
  beta_dpobj$m <- 3

  clusterLabels <- beta_dpobj$clusterLabels
  pointsPerCluster <- beta_dpobj$pointsPerCluster

  currentLabel <- clusterLabels[1]
  pointsPerCluster[currentLabel] <- pointsPerCluster[currentLabel] - 1

  beta_dpobj$pointsPerCluster <- pointsPerCluster

  aux <- PriorDraw(beta_md_obj, 3)

  beta_dpobj = ClusterLabelChange(beta_dpobj, 1, 3, currentLabel, aux)

  expect_equal(beta_dpobj$numberClusters, 2)
  expect_equal(sum(beta_dpobj$pointsPerCluster), 10)
  expect_length(beta_dpobj$clusterParameters[[1]], 2)
  expect_length(beta_dpobj$clusterParameters[[2]], 2)
})

test_that("Beta Cluster Label Change B", {
  pts <- rbeta(10, 2,2)
  beta_md_obj <- BetaMixtureCreate(c(2,8), c(1, 1), 1, hyperPriorParameters=c(1, 0.125))
  beta_dpobj <- DirichletProcessCreate(pts, beta_md_obj, c(2,4))
  beta_dpobj <- Initialise(beta_dpobj, verbose=FALSE)

  beta_dpobj$clusterLabels <- c(1, rep.int(2, 9))
  beta_dpobj$numberClusters <- 2
  beta_dpobj$pointsPerCluster <- c(1,9)
  beta_dpobj$clusterParameters <- PriorDraw(beta_dpobj$mixingDistribution, 2)
  beta_dpobj$m <- 3

  clusterLabels <- beta_dpobj$clusterLabels
  pointsPerCluster <- beta_dpobj$pointsPerCluster

  currentLabel <- clusterLabels[1]
  pointsPerCluster[currentLabel] <- pointsPerCluster[currentLabel] - 1

  beta_dpobj$pointsPerCluster <- pointsPerCluster

  aux <- PriorDraw(beta_md_obj, 3)

  beta_dpobj = ClusterLabelChange(beta_dpobj, 1, 2, currentLabel, aux)

  expect_equal(beta_dpobj$numberClusters, 1)
  expect_equal(sum(beta_dpobj$pointsPerCluster), 10)
  expect_length(beta_dpobj$clusterParameters[[1]], 1)
  expect_length(beta_dpobj$clusterParameters[[2]], 1)
})

test_that("Beta DP Posterior Clusters", {

  pts <- rbeta(10, 2,2)
  beta_dpobj <- DirichletProcessBeta(pts, 1)
  beta_dpobj <- Fit(beta_dpobj, 5, FALSE, FALSE)

  post_cl_test <- PosteriorClusters(beta_dpobj)

  expect_is(post_cl_test, "list")
  expect_is(post_cl_test[[1]], "numeric")

  expect_length(post_cl_test, 2)
  expect_length(post_cl_test[[2]], 2)

})

test_that("Beta DP Posterior Function", {

  pts <- rbeta(10, 2,2)
  beta_dpobj <- DirichletProcessBeta(pts, 1)
  beta_dpobj <- Fit(beta_dpobj, 5, FALSE, FALSE)

  post_func <- PosteriorFunction(beta_dpobj)

  expect_is(post_func, "function")

})

test_that("Beta Penelised Likelihodd", {
  maxT <- 1
  mu <- 0.5
  tau <- 4

  a <- (mu * tau)/maxT
  b <- (1 - mu/maxT) * tau

  pts <- rbeta(100, a, b)
  beta_md_obj <- BetaMixtureCreate(c(2,8), c(1, 1), 1, hyperPriorParameters=c(1, 0.125))

  testParams <- PenalisedLikelihood(beta_md_obj, pts)

  expect_is(testParams, "list")
  expect_length(testParams, 2)

  expect_is(testParams[[1]], "array")
  expect_is(testParams[[2]], "array")

  expect_length(testParams[[1]], 1)
  expect_length(testParams[[2]], 1)

})

test_that("Dirichlet Process Likelihood", {

  pts <- rbeta(10, 2,2)
  beta_dpobj <- DirichletProcessBeta(pts, 1, verbose = FALSE)
  beta_dpobj <- Fit(beta_dpobj, 5, FALSE, FALSE)

  lik <- LikelihoodDP(beta_dpobj)


})



