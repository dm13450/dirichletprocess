context("Multivariate Normal Tests")


test_that("Multivariate Mixture Object Create", {

  mdobj <- MvnormalCreate(c(1,1))

  expect_is(mdobj, c("list", "MixingDistribution", "mvnormal", "conjugate"))

})

test_that("Multivariate Normal Likelihood", {
  priorParameters <- list(mu0=c(0,0), Lambda=diag(2), kappa0=1, nu=2)
  mdobj <- MvnormalCreate(priorParameters)
  test_theta <- list(mu=array(c(0,0), c(1,2,1)), sig=array(diag(2), c(2,2,1)))
  lik_test <- Likelihood(mdobj, c(0,0), test_theta)
  expect_equal(lik_test, 1/sqrt(4*pi^2))

  test_theta_multi <- list(mu=array(c(0,0), c(1,2,2)), sig=array(diag(2), c(2,2,2)))
  lik_test_multi <- Likelihood(mdobj, c(0,0), test_theta_multi)

  expect_equal(lik_test_multi, rep.int(1/sqrt(4*pi^2), 2))

})

test_that("Multivariate Normal Prior Draw", {

  priorParameters <- list(mu0=c(0,0), Lambda=diag(2), kappa0=1, nu=2)

  mdobj <- MvnormalCreate(priorParameters)

  PriorDraw_test_single <- PriorDraw(mdobj, 1)

  expect_is(PriorDraw_test_single, "list")

  PriorDraw_test_multiple <- PriorDraw(mdobj, 10)

  expect_is(PriorDraw_test_multiple, "list")
  expect_equal(dim(PriorDraw_test_multiple$mu), c(1,2,10))
  expect_equal(dim(PriorDraw_test_multiple$sig), c(2,2,10))
})

test_that("Multivariate Normal Posterior Parameters", {

  test_data <- mvtnorm::rmvnorm(10, c(0,0), diag(2))

  priorParameters <- list(mu0=c(0,0), Lambda=diag(2), kappa0=1, nu=2)

  mdobj <- MvnormalCreate(priorParameters)

  post_params_test <- PosteriorParameters(mdobj, test_data)

  expect_is(post_params_test, "list")
  expect_equal(length(post_params_test), 4)
})

test_that("Multivariate Normal Posterior Parameters 1 Data Point", {

  test_data <- mvtnorm::rmvnorm(10, c(0,0), diag(2))

  priorParameters <- list(mu0=c(0,0), Lambda=diag(2), kappa0=2, nu=2)
  mdobj <- MvnormalCreate(priorParameters)

  post_params_test2 <- PosteriorParameters(mdobj, test_data[1, ])
  expect_is(post_params_test2, "list")
  expect_equal(length(post_params_test2), 4)
})

test_that("Multivariate Normal Posterior Draw", {

  test_data <- mvtnorm::rmvnorm(10, c(0,0), diag(2))
  priorParameters <- list(mu0=c(0,0), Lambda=diag(2), kappa0=1, nu=2)
  mdobj <- MvnormalCreate(priorParameters)

  post_draws_single <- PosteriorDraw(mdobj, test_data, 1)

  expect_is(post_draws_single, "list")
  expect_equal(length(post_draws_single), 2)
  expect_equal(dim(post_draws_single$mu), c(1,2,1))
  expect_equal(dim(post_draws_single$sig), c(2,2,1))

  post_draws_multi <- PosteriorDraw(mdobj, test_data[1,], 10)

  expect_equal(length(post_draws_multi), 2)
  expect_equal(dim(post_draws_multi$mu), c(1,2,10))
  expect_equal(dim(post_draws_multi$sig), c(2,2,10))
  expect_is(post_draws_multi, "list")
})

test_that("Multivariate Normal Predictive", {

  test_data <- mvtnorm::rmvnorm(10, c(0,0), diag(2))
  priorParameters <- list(mu0=c(0,0), Lambda=diag(2), kappa0=1, nu=2)
  mdobj <- MvnormalCreate(priorParameters)

  pred_test <- Predictive(mdobj, test_data)

  expect_length(pred_test, 10)

})



test_that("Multivariate Normal Dirichlet Create and Initialise", {

  test_data <- mvtnorm::rmvnorm(10, c(0,0), diag(2))
  priorParameters <- list(mu0=c(0,0), Lambda=diag(2), kappa0=1, nu=2)
  mdobj <- MvnormalCreate(priorParameters)

  dpobj <- DirichletProcessCreate(test_data, mdobj)
  dpobj <- Initialise(dpobj)

  expect_is(dpobj, c("list", "dirichletprocess", "mvnormal", "conjugate"))

  expect_equal(length(dpobj$clusterParameters), 2)
  expect_equal(dim(dpobj$clusterParameters$mu), c(1,2,1))
  expect_equal(dim(dpobj$clusterParameters$sig), c(2,2,1))
})

test_that("Multivariate Normal Componenet Update", {

  test_data <- mvtnorm::rmvnorm(10, c(0,0), diag(2))
  priorParameters <- list(mu0=c(0,0), Lambda=diag(2), kappa0=1, nu=2)
  mdobj <- MvnormalCreate(priorParameters)

  dpobj <- DirichletProcessCreate(test_data, mdobj)
  dpobj <- Initialise(dpobj)

  dpobj <- ClusterComponentUpdate(dpobj)

  expect_equal(dpobj$n, 10)
  expect_equal(sum(dpobj$pointsPerCluster), 10)
  expect_equal(dpobj$data, test_data)
})


test_that("Multivariate Normal Cluster Label Change",{

  test_data <- mvtnorm::rmvnorm(10, c(0,0), diag(2))
  priorParameters <- list(mu0=c(0,0), Lambda=diag(2), kappa0=1, nu=2)
  mdobj <- MvnormalCreate(priorParameters)

  dpobj <- DirichletProcessCreate(test_data, mdobj)
  dpobj <- Initialise(dpobj)

  dpobj <- ClusterLabelChange(dpobj, 1, 11, 1)

})

test_that("Multivariate Normal Cluster Parameter Update", {

  test_data <- mvtnorm::rmvnorm(10, c(0,0), diag(2))
  priorParameters <- list(mu0=c(0,0), Lambda=diag(2), kappa0=1, nu=2)
  mdobj <- MvnormalCreate(priorParameters)

  dpobj <- DirichletProcessCreate(test_data, mdobj)
  dpobj <- Initialise(dpobj)

  dpobj <- ClusterParameterUpdate(dpobj)

  expect_equal(dim(dpobj$clusterParameters$mu), c(1,2,1))
  expect_equal(dim(dpobj$clusterParameters$sig), c(2,2,1))

})

test_that("Multivariate Normal Fit", {
  test_data <- mvtnorm::rmvnorm(10, c(0,0), diag(2))
  priorParameters <- list(mu0=c(0,0), Lambda=diag(2), kappa0=1, nu=2)
  mdobj <- MvnormalCreate(priorParameters)

  dpobj <- DirichletProcessCreate(test_data, mdobj)
  dpobj <- Initialise(dpobj)
  dpobj <- Fit(dpobj, 10, FALSE, FALSE)

  expect_equal(dpobj$n, 10)
  expect_equal(sum(dpobj$pointsPerCluster), 10)
  expect_equal(dpobj$data, test_data)

})

test_that("Multivariate Normal Cluster Predict", {

  test_data <- as.matrix(mvtnorm::rmvnorm(1, c(0,0), diag(2)))
  dp <- DirichletProcessMvnormal(test_data)
  dp <- Fit(dp, 10, progressBar=FALSE)

  pred <- ClusterLabelPredict(dp, mvtnorm::rmvnorm(1, c(0,0), diag(2)))

  expect_length(pred$componentIndexes, 1)

})

