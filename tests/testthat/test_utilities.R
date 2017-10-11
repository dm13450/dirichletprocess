context("Utilities")


test_that("Weighted Function Generator", {

  test_func <- function(x, theta) dnorm(x, theta[[1]], theta[[2]])

  test_params <- list(array(0, dim=c(1,1,1)), array(1, dim=c(1,1,1)))

  test_weighted_func <- weighted_function_generator(test_func, 1, test_params)

  expect_is(test_weighted_func, "function")
  expect_equal(test_weighted_func(0), dnorm(0))
})
