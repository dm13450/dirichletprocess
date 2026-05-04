# tests/testthat/test-cpp-consistency-weibull.R
#
# R/C++ Consistency Tests for Weibull Distribution
# 
# This file tests statistical equivalence between R and C++ implementations
# for the Weibull distribution with comprehensive validation.

# Development vs Production testing configuration
DEV_MODE <- Sys.getenv("DP_DEV_TESTING", "TRUE") == "TRUE"
BASE_ITERATIONS <- if (DEV_MODE) 50 else 200
BASE_SAMPLE_SIZE <- if (DEV_MODE) 50 else 100

test_that("Weibull distribution R/C++ consistency", {
  set.seed(123)
  test_data <- rweibull(BASE_SAMPLE_SIZE, shape = c(0.5, 1.5, 3), scale = 1)

  results <- validate_r_cpp_consistency("weibull", test_data, iterations = BASE_ITERATIONS)

  expect_lt(results$alpha_mean_diff, ALPHA_TOLERANCE)
  expect_lt(results$cluster_count_diff, CLUSTER_TOLERANCE)
  expect_gt(results$likelihood_correlation, LIKELIHOOD_CORR_MIN)
})