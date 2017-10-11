context("Normal Mixing Distribution Functions")

data_test = rnorm(10)
priorParameters_test = matrix(c(1,1,1,1), ncol=4)

normal_object_test = MixingDistribution("normal", priorParameters_test, "conjugate")

test_that("Normal Class", {

  expect_is(normal_object_test, c("list", "normal", "conjugate"))

})

test_that("Normal Likelihood", {

  cluster_params_test = list(array(c(1,1), dim=c(1,1,2)), array(c(1,2), dim=c(1,1,2)))

  expect_equal(Likelihood(normal_object_test, 0, list(0,1)), dnorm(0))
  expect_equal(Likelihood(normal_object_test, 0, cluster_params_test), dnorm(0, c(1,1), c(1,2)))
})

test_that("Normal Prior Draw", {

  expect_length((PriorDraw(normal_object_test, 1)), 2)
  expect_equal(dim(PriorDraw(normal_object_test, 10)[[1]]), c(1,1,10))
  expect_equal(dim(PriorDraw(normal_object_test, 10)[[2]]), c(1,1,10))
})

test_that("Normal Posterior Draw", {

  expect_length((PosteriorDraw(normal_object_test, data_test, 1)), 2)
  expect_equal(dim(PosteriorDraw(normal_object_test, data_test, 10)[[1]]), c(1,1,10))
  expect_equal(dim(PosteriorDraw(normal_object_test, data_test, 10)[[2]]), c(1,1,10))

})

test_that("Normal Predictive", {


  pred_array <- Predictive(normal_object_test, matrix(data_test, ncol=1))



  expect_equal(length(Predictive(normal_object_test, data_test)), length(data_test))
})


