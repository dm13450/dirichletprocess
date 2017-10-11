context("Metropolis Hastings Tests")

test_that("Metropolis Hastings Full Sample Weibull", {

  test_data <- rweibull(100, 1, 1)

  test_MixingDistribution <- WeibullMixtureCreate(c(1,1,1), 1)

  test_mh <- MetropolisHastings(test_MixingDistribution, test_data,  PriorDraw(test_MixingDistribution), 20)

  expect_equal(length(test_mh), 2)
  expect_is(test_mh$accept_ratio, "numeric")
  expect_equal(length(test_mh$parameter_samples), 2)
  expect_equal(length(test_mh$parameter_samples[[1]]), 20)
  expect_equal(length(test_mh$parameter_samples[[2]]), 20)

})

test_that("Metropolis Hastings Full Sample Beta", {


  test_data <- rbeta(10, 2,2)

  test_mdobj <- BetaMixtureCreate(mhStepSize = 0.1, maxT = 1)
  test_start_pos <- PriorDraw(test_mdobj)

  test_mh <- MetropolisHastings(test_mdobj, test_data, test_start_pos, 20)

  expect_equal(length(test_mh), 2)
  expect_is(test_mh$accept_ratio, "numeric")
  expect_equal(length(test_mh$parameter_samples), 2)
  expect_equal(length(test_mh$parameter_samples[[1]]), 20)
  expect_equal(length(test_mh$parameter_samples[[2]]), 20)
})

