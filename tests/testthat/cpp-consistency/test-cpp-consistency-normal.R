# tests/testthat/test-cpp-consistency-normal.R
#
# R/C++ Consistency Tests for Normal Distribution
# 
# This file tests statistical equivalence between R and C++ implementations
# for the Normal (Gaussian) distribution with comprehensive validation.
#
# PERFORMANCE OPTIMIZATION:
# - DEV_MODE (default): Fast testing for development (50 iterations, smaller samples)
# - PRODUCTION_MODE: Full validation testing (200 iterations, larger samples)
#
# Usage:
# - Development: Sys.setenv(DP_DEV_TESTING = "TRUE") (default)
# - Production:  Sys.setenv(DP_DEV_TESTING = "FALSE")

# Development vs Production testing configuration
DEV_MODE <- Sys.getenv("DP_DEV_TESTING", "TRUE") == "TRUE"
BASE_ITERATIONS <- if (DEV_MODE) 50 else 200
BASE_SAMPLE_SIZE <- if (DEV_MODE) 50 else 100

test_that("Normal distribution R/C++ consistency", {
  # Generate test data
  set.seed(123)
  test_data <- rnorm(BASE_SAMPLE_SIZE, mean = c(-2, 0, 2), sd = 1)

  # Run consistency tests
  results <- validate_r_cpp_consistency("normal", test_data, iterations = BASE_ITERATIONS)

  # Assertions
  expect_lt(results$alpha_mean_diff, ALPHA_TOLERANCE)
  expect_lt(results$cluster_count_diff, CLUSTER_TOLERANCE)
  expect_gt(results$likelihood_correlation, LIKELIHOOD_CORR_MIN)
  expect_lt(results$param_max_diff, PARAM_TOLERANCE)
})

# Test different sample sizes for Normal distribution
test_that("Normal consistency holds for different sample sizes", {
  sample_sizes <- if (DEV_MODE) c(25, 50) else c(50, 100, 500)
  dev_iterations <- if (DEV_MODE) 30 else 100
  dev_runs <- if (DEV_MODE) 2 else 3

  for (n in sample_sizes) {
    test_data <- generate_test_data("normal", n)
    results <- validate_r_cpp_consistency("normal", test_data, iterations = dev_iterations, n_runs = dev_runs)

    expect_lt(results$alpha_mean_diff, ALPHA_TOLERANCE,
              label = paste("Failed for sample size", n))
    expect_lt(results$cluster_count_diff, CLUSTER_TOLERANCE,
              label = paste("Failed for sample size", n))
  }
})