context("Utilities")


test_that("Weighted Function Generator", {

  test_func <- function(x, theta) dnorm(x, theta[[1]], theta[[2]])

  test_params <- list(array(0, dim=c(1,1,1)), array(1, dim=c(1,1,1)))

  test_weighted_func <- weighted_function_generator(test_func, 1, test_params)

  expect_is(test_weighted_func, "function")
  expect_equal(test_weighted_func(0), dnorm(0))
  expect_equivalent(test_weighted_func(as.matrix(0)), dnorm(0))
  expect_equivalent(test_weighted_func(as.data.frame(0)), dnorm(0))
})



test_that("Burn", {
  dp <- Fit(DirichletProcessGaussian(rnorm(10)), 4)

  burned_dp <- Burn(dp, 2)

  expect_s3_class(burned_dp, "dirichletprocess")
})
