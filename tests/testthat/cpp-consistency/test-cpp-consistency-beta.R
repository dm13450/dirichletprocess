# tests/testthat/test-cpp-consistency-beta.R
#
# R/C++ Consistency Tests for Beta Distribution
# 
# This file tests statistical equivalence between R and C++ implementations
# for the Beta distribution with comprehensive validation.

# Development vs Production testing configuration
DEV_MODE <- Sys.getenv("DP_DEV_TESTING", "TRUE") == "TRUE"
BASE_ITERATIONS <- if (DEV_MODE) 50 else 200

test_that("Beta distribution R/C++ consistency", {
  set.seed(123)
  beta_size <- if (DEV_MODE) 25 else 50
  test_data <- c(rbeta(beta_size, 2, 5), rbeta(beta_size, 5, 2))

  results <- validate_r_cpp_consistency("beta", test_data, iterations = BASE_ITERATIONS)

  expect_lt(results$alpha_mean_diff, ALPHA_TOLERANCE)
  expect_lt(results$cluster_count_diff, CLUSTER_TOLERANCE)
  expect_gt(results$likelihood_correlation, LIKELIHOOD_CORR_MIN)
})