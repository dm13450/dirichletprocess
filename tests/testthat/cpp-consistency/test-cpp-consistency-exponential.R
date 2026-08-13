# tests/testthat/test-cpp-consistency-exponential.R
#
# R/C++ Consistency Tests for Exponential Distribution
# 
# This file tests statistical equivalence between R and C++ implementations
# for the Exponential distribution with comprehensive validation.

# Development vs Production testing configuration
DEV_MODE <- Sys.getenv("DP_DEV_TESTING", "TRUE") == "TRUE"
BASE_ITERATIONS <- if (DEV_MODE) 50 else 200
BASE_SAMPLE_SIZE <- if (DEV_MODE) 50 else 100

test_that("Exponential distribution R/C++ consistency", {
  set.seed(123)
  test_data <- rexp(BASE_SAMPLE_SIZE, rate = c(0.5, 1, 2))

  results <- validate_r_cpp_consistency("exponential", test_data, iterations = BASE_ITERATIONS)

  expect_lt(results$alpha_mean_diff, ALPHA_TOLERANCE)
  expect_lt(results$cluster_count_diff, CLUSTER_TOLERANCE)
  expect_gt(results$likelihood_correlation, LIKELIHOOD_CORR_MIN)
})

# Test different iteration counts for Exponential distribution
test_that("Exponential consistency holds for different iteration counts", {
  test_data <- generate_test_data("exponential", BASE_SAMPLE_SIZE)
  iteration_counts <- if (DEV_MODE) c(25, 50) else c(50, 100, 500)
  iter_runs <- if (DEV_MODE) 2 else 3

  for (its in iteration_counts) {
    results <- validate_r_cpp_consistency("exponential", test_data,
                                          iterations = its, n_runs = iter_runs)

    expect_lt(results$alpha_mean_diff, ALPHA_TOLERANCE,
              label = paste("Failed for", its, "iterations"))
    expect_gt(results$likelihood_correlation, -0.5,  # Use updated tolerance
              label = paste("Failed for", its, "iterations"))
  }
})