context("Weibull Uniform Gamma Tests")

test_mdobj <- WeibullMixtureCreate(c(1,1,1), c(1,1))

test_that("Weibull Creation", {

  expect_is(test_mdobj, c("list", "weibull", "nonconjugate"))

})

test_that("Weibull Likelihood", {

  weibull_test_params_1 <- list(array(1, dim=c(1,1,1)), array(1, dim=c(1,1,1)))

  expect_equal(Likelihood(test_mdobj, 0,weibull_test_params_1), dweibull(0, 1, 1))

  weibull_test_params_2 <- list(array(c(1,2), dim=c(1,1,2)), array(c(1,2), dim=c(1,1,2)))

  expect_equal(Likelihood(test_mdobj, 0, weibull_test_params_2), dweibull(0, c(1,2), c(1,2)))

})

test_that("Weibull Likelihood: Negative x", {

  weibull_test_params_1 <- list(array(1, dim=c(1,1,1)), array(1, dim=c(1,1,1)))
  weibull_test_params_2 <- list(array(.1, dim=c(1,1,1)), array(.1, dim=c(1,1,1)))
  testEval <- sapply(seq(-10, -0.1, by=0.1), function(x) Likelihood(test_mdobj, x, weibull_test_params_1))
  testEval2 <- sapply(seq(-10, -0.1, by=0.1), function(x) Likelihood(test_mdobj, x, weibull_test_params_2))

  expect_true(all(testEval==0))
  expect_true(all(testEval2==0))
})


test_that("Weibull Prior Density", {

  expect_equal(PriorDensity(test_mdobj, matrix(c(1,1),ncol=2)), dunif(1, 0,1))

})

test_that("Weibull Prior Draw", {

  expect_is(PriorDraw(test_mdobj, 1), "list")
  expect_equal(dim(PriorDraw(test_mdobj, 10)[[1]]), c(1,1,10))
  expect_equal(dim(PriorDraw(test_mdobj, 10)[[2]]), c(1,1,10))

})

test_that("Weibull Posterior Draw", {

  expect_is(PosteriorDraw(test_mdobj, rweibull(10, 1,1), 1), "list")

  expect_equal(dim(PosteriorDraw(test_mdobj, rweibull(10, 1,1), 1)[[1]]), c(1, 1,1))
  expect_equal(dim(PosteriorDraw(test_mdobj, rweibull(10, 1,1), 1)[[2]]), c(1, 1,1))
  expect_equal(dim(PosteriorDraw(test_mdobj, rweibull(10, 1,1), 10)[[1]]), c(1, 1,10))
  expect_equal(dim(PosteriorDraw(test_mdobj, rweibull(10, 1,1), 10)[[2]]), c(1, 1,10))

})

test_that("Prior Parameter Update", {

  test_clusterParameters <- list(array(c(runif(10)), dim=c(1,1,10)), array(c(rgamma(10, 1,1)), dim=c(1,1,10)))

  test_mdobj <- PriorParametersUpdate(test_mdobj, test_clusterParameters)

  expect_equal(dim(test_mdobj$priorParameters), c(1,3))

})

test_that("Cluster Parameter Update", {

  # test_data <- c(rlnorm(100, 0.4,0.25), rlnorm(100, 1.4, 0.6))
  #
  # dpobj <- dirichlet_process_weibull(test_data, c(10, 2, 0.01), mh_step_size=c(0.1, 0.1))
  # dpobj <- ClusterComponentUpdate(dpobj)
  #
  # dpobj <- ClusterParameterUpdate(dpobj)


})

test_that("Parameter Proposal", {

  old_param <- PriorDraw(test_mdobj, 1)

  new_param <- MhParameterProposal(test_mdobj, old_param)
})



