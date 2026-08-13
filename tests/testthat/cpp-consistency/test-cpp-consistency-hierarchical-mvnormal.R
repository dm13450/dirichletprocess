# tests/testthat/test-cpp-consistency-hierarchical-mvnormal.R
#
# R/C++ Consistency Tests for Hierarchical MVNormal Distribution
# 
# This file tests statistical equivalence between R and C++ implementations
# for the Hierarchical MVNormal distribution with comprehensive validation.

# Development vs Production testing configuration
DEV_MODE <- Sys.getenv("DP_DEV_TESTING", "TRUE") == "TRUE"
BASE_ITERATIONS <- if (DEV_MODE) 50 else 200

test_that("Hierarchical MVNormal distribution R/C++ consistency", {
  skip_if_not_installed("mvtnorm")
  
  set.seed(123)
  
  # Create hierarchical test data - multiple groups with different means
  group_size <- if (DEV_MODE) 25 else 50
  
  # Generate 2D multivariate normal data for different groups
  group1_data <- mvtnorm::rmvnorm(group_size, mean = c(2, 3), sigma = diag(c(1, 1.5)))
  group2_data <- mvtnorm::rmvnorm(group_size, mean = c(-1, 2), sigma = diag(c(1.2, 0.8)))
  group3_data <- mvtnorm::rmvnorm(group_size, mean = c(1, -1), sigma = diag(c(0.9, 1.1)))
  
  hierarchical_data <- list(group1_data, group2_data, group3_data)
  
  # Set up default priors for MVNormal-Wishart
  prior_params <- list(
    mu0 = c(0, 0),
    kappa0 = 0.01,
    nu = 4,  # ncol + 2
    Lambda = diag(2)
  )
  
  results <- validate_r_cpp_consistency("hierarchical_mvnormal", hierarchical_data, 
                                        iterations = BASE_ITERATIONS)

  expect_lt(results$alpha_mean_diff, ALPHA_TOLERANCE)
  expect_lt(results$cluster_count_diff, CLUSTER_TOLERANCE)
  expect_gt(results$likelihood_correlation, LIKELIHOOD_CORR_MIN)
})

test_that("Hierarchical MVNormal manual MCMC interface", {
  skip_if_not(using_cpp(), "C++ not available")
  skip_if_not_installed("mvtnorm")
  
  set.seed(456)
  
  # Create smaller test data for manual MCMC
  group_size <- if (DEV_MODE) 15 else 30
  group1_data <- mvtnorm::rmvnorm(group_size, mean = c(1, 1), sigma = diag(c(0.5, 0.5)))
  group2_data <- mvtnorm::rmvnorm(group_size, mean = c(-1, -1), sigma = diag(c(0.6, 0.4)))
  hierarchical_data <- list(group1_data, group2_data)
  
  # Set up priors
  prior_params <- list(
    mu0 = c(0, 0),
    kappa0 = 0.01,
    nu = 4,
    Lambda = diag(2)
  )
  
  # Test HierarchicalDirichletProcessMVNormal constructor
  hdp <- HierarchicalDirichletProcessMVNormal(hierarchical_data, 
                                              prior_params = prior_params)
  
  # Test basic fitting
  hdp <- Fit(hdp, iterations = if (DEV_MODE) 25 else 50)
  
  # Verify results
  expect_s3_class(hdp, c("hdp_mvnormal", "hdp", "dirichletprocess"))
  expect_true("samples" %in% names(hdp))
  expect_true("final_state" %in% names(hdp))
})

test_that("Hierarchical MVNormal different dimensions", {
  skip_if_not(using_cpp(), "C++ not available")
  skip_if_not_installed("mvtnorm")
  
  set.seed(789)
  
  # Test with 3D data
  group_size <- if (DEV_MODE) 20 else 30
  group1_data <- mvtnorm::rmvnorm(group_size, mean = c(1, 2, 3), 
                                  sigma = diag(c(0.5, 0.6, 0.7)))
  group2_data <- mvtnorm::rmvnorm(group_size, mean = c(-1, -2, 1), 
                                  sigma = diag(c(0.8, 0.4, 0.9)))
  hierarchical_data <- list(group1_data, group2_data)
  
  # Set up 3D priors
  prior_params <- list(
    mu0 = c(0, 0, 0),
    kappa0 = 0.01,
    nu = 5,  # ncol + 2
    Lambda = diag(3)
  )
  
  expect_no_error({
    hdp_3d <- HierarchicalDirichletProcessMVNormal(hierarchical_data, 
                                                   prior_params = prior_params)
    hdp_3d <- Fit(hdp_3d, iterations = if (DEV_MODE) 20 else 40)
  })
  
  expect_s3_class(hdp_3d, c("hdp_mvnormal", "hdp", "dirichletprocess"))
})

test_that("Hierarchical MVNormal edge cases", {
  skip_if_not(using_cpp(), "C++ not available")
  skip_if_not_installed("mvtnorm")
  
  set.seed(101112)
  
  # Test with very small groups
  small_data <- list(
    mvtnorm::rmvnorm(3, mean = c(0, 0), sigma = diag(2)),
    mvtnorm::rmvnorm(2, mean = c(1, 1), sigma = diag(2))
  )
  
  prior_params <- list(
    mu0 = c(0, 0),
    kappa0 = 0.01,
    nu = 4,
    Lambda = diag(2)
  )
  
  expect_no_error({
    hdp_small <- HierarchicalDirichletProcessMVNormal(small_data, 
                                                      prior_params = prior_params)
    hdp_small <- Fit(hdp_small, iterations = if (DEV_MODE) 10 else 25)
  })
  
  # Test with single group (edge case)  
  single_group <- list(mvtnorm::rmvnorm(if (DEV_MODE) 10 else 20, 
                                        mean = c(0, 0), sigma = diag(2)))
  
  expect_no_error({
    hdp_single <- HierarchicalDirichletProcessMVNormal(single_group, 
                                                       prior_params = prior_params)
    hdp_single <- Fit(hdp_single, iterations = if (DEV_MODE) 10 else 25)
  })
})