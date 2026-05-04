# tests/testthat/test-cpp-consistency-mvnormal2.R
#
# R/C++ Consistency Tests for MVNormal2 Distribution
# 
# This file tests statistical equivalence between R and C++ implementations
# for the MVNormal2 (semi-conjugate) distribution with comprehensive validation.

# Development vs Production testing configuration
DEV_MODE <- Sys.getenv("DP_DEV_TESTING", "TRUE") == "TRUE"
BASE_ITERATIONS <- if (DEV_MODE) 50 else 200

test_that("MVNormal2 distribution R/C++ consistency", {
  set.seed(123)
  mu1 <- c(-2, -2)
  mu2 <- c(2, 2)
  sigma <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
  mvn2_size <- if (DEV_MODE) 25 else 50
  test_data <- rbind(
    mvtnorm::rmvnorm(mvn2_size, mu1, sigma),
    mvtnorm::rmvnorm(mvn2_size, mu2, sigma)
  )

  results <- validate_r_cpp_consistency("mvnormal2", test_data, iterations = BASE_ITERATIONS)

  expect_lt(results$alpha_mean_diff, ALPHA_TOLERANCE)
  expect_lt(results$cluster_count_diff, CLUSTER_TOLERANCE)
  expect_gt(results$likelihood_correlation, LIKELIHOOD_CORR_MIN)
})