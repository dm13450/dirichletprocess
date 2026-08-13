# tests/testthat/test-cpp-consistency.R

library(testthat)
library(dirichletprocess)

# Functions are now defined in helper-testing.R

# Basic test to ensure framework is working
test_that("Consistency validation framework works", {
  test_data <- rnorm(50)
  results <- validate_r_cpp_consistency("normal", test_data, iterations = 10, n_runs = 2)

  expect_type(results, "list")
  expect_true("alpha_mean_diff" %in% names(results))
  expect_true("speedup_factor" %in% names(results))
  expect_true(results$speedup_factor > 0)
})
