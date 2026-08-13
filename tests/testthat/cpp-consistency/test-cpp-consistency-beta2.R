# tests/testthat/test-cpp-consistency-beta2.R
#
# R/C++ Consistency Tests for Beta2 Distribution
# 
# This file tests statistical equivalence between R and C++ implementations
# for the Beta2 distribution (Beta with Uniform-Pareto base measure).

# Development vs Production testing configuration
DEV_MODE <- Sys.getenv("DP_DEV_TESTING", "TRUE") == "TRUE"
BASE_ITERATIONS <- if (DEV_MODE) 50 else 200

test_that("Beta2 distribution R/C++ consistency", {
  set.seed(123)
  
  beta2_size <- if (DEV_MODE) 25 else 50
  # Generate beta data bounded on (0, 1)
  test_data <- c(rbeta(beta2_size, 3, 2), rbeta(beta2_size, 1, 4))
  
  # Beta2 requires maxY parameter (upper bound)
  maxY <- 1.0
  
  results <- validate_r_cpp_consistency("beta2", test_data, 
                                        iterations = BASE_ITERATIONS)

  expect_lt(results$alpha_mean_diff, ALPHA_TOLERANCE)
  expect_lt(results$cluster_count_diff, CLUSTER_TOLERANCE)
  expect_gt(results$likelihood_correlation, LIKELIHOOD_CORR_MIN)
})

test_that("Beta2 manual MCMC interface", {
  skip_if_not(using_cpp(), "C++ not available")
  
  set.seed(456)
  
  # Create smaller test data for manual MCMC
  test_size <- if (DEV_MODE) 15 else 30
  test_data <- c(rbeta(test_size, 2, 3), rbeta(test_size, 4, 1))
  maxY <- 1.0
  
  # Test Beta2 constructor with Pareto scale prior
  dp <- DirichletProcessBeta2(test_data, maxY = maxY, g0Priors = 2)
  
  # Enable C++ samplers
  set_use_cpp(TRUE)
  enable_cpp_samplers()
  
  # Test manual MCMC steps
  manual_iterations <- if (DEV_MODE) 25 else 50
  for (i in 1:manual_iterations) {
    dp <- ClusterComponentUpdate(dp)
    dp <- ClusterParameterUpdate(dp)
    dp <- UpdateAlpha(dp)
  }
  
  # Verify results
  expect_s3_class(dp, c("dirichletprocess"))
  expect_true(dp$numberClusters >= 1)
  expect_true(all(dp$clusterLabels %in% 1:dp$numberClusters))
  
  # Check that parameters are within correct bounds
  # mu should be in (0, maxY), nu should be positive
  mu_params <- unlist(dp$clusterParameters$mu)
  nu_params <- unlist(dp$clusterParameters$nu)
  expect_true(all(mu_params > 0 & mu_params < maxY))
  expect_true(all(nu_params > 0))
})

test_that("Beta2 with different maxY values", {
  skip_if_not(using_cpp(), "C++ not available")
  
  set.seed(789)
  
  # Test with different upper bounds
  maxY_values <- if (DEV_MODE) c(0.5, 1.0) else c(0.5, 1.0, 2.0)
  
  for (maxY in maxY_values) {
    test_size <- if (DEV_MODE) 15 else 25
    # Generate data bounded by maxY
    test_data <- runif(test_size, 0, maxY * 0.8)  # Keep data well within bounds
    
    expect_no_error({
      dp_maxY <- DirichletProcessBeta2(test_data, maxY = maxY, g0Priors = 2)
      dp_maxY <- Fit(dp_maxY, its = if (DEV_MODE) 20 else 40)
    })
    
    # Verify parameters respect bounds
    # mu should be in (0, maxY), nu should be positive
    mu_params <- unlist(dp_maxY$clusterParameters$mu)
    nu_params <- unlist(dp_maxY$clusterParameters$nu)
    expect_true(all(mu_params > 0 & mu_params < maxY))
    expect_true(all(nu_params > 0))
  }
})

test_that("Beta2 Pareto scale prior effects", {
  skip_if_not(using_cpp(), "C++ not available")
  
  set.seed(987)
  
  # Test with different Pareto scale priors
  pareto_priors <- if (DEV_MODE) c(1, 3) else c(1, 2, 3, 5)
  test_size <- if (DEV_MODE) 20 else 30
  test_data <- rbeta(test_size, 2, 2)
  maxY <- 1.0
  
  for (g0_prior in pareto_priors) {
    expect_no_error({
      dp_pareto <- DirichletProcessBeta2(test_data, maxY = maxY, 
                                         g0Priors = g0_prior)
      dp_pareto <- Fit(dp_pareto, its = if (DEV_MODE) 20 else 40)
    })
    
    # Verify basic properties
    expect_s3_class(dp_pareto, "dirichletprocess")
    expect_true(dp_pareto$numberClusters >= 1)
  }
})

test_that("Beta2 edge cases", {
  skip_if_not(using_cpp(), "C++ not available")
  
  set.seed(101112)
  
  # Test with very small dataset
  small_data <- rbeta(3, 1, 1)
  maxY <- 1.0
  
  expect_no_error({
    dp_small <- DirichletProcessBeta2(small_data, maxY = maxY, g0Priors = 2)
    dp_small <- Fit(dp_small, its = if (DEV_MODE) 10 else 25)
  })
  
  # Test with uniform data (edge case for Beta)
  uniform_data <- rep(0.5, if (DEV_MODE) 5 else 10)
  
  expect_no_error({
    dp_uniform <- DirichletProcessBeta2(uniform_data, maxY = maxY, g0Priors = 2)
    dp_uniform <- Fit(dp_uniform, its = if (DEV_MODE) 10 else 25)
  })
  
  # Test with data near boundaries
  boundary_data <- c(rep(0.01, 3), rep(0.99, 3))
  
  expect_no_error({
    dp_boundary <- DirichletProcessBeta2(boundary_data, maxY = maxY, g0Priors = 2)
    dp_boundary <- Fit(dp_boundary, its = if (DEV_MODE) 10 else 25)
  })
})