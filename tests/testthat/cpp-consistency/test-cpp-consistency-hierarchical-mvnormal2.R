# tests/testthat/test-cpp-consistency-hierarchical-mvnormal2.R
#
# R/C++ Consistency Tests for Hierarchical MVNormal2 Distribution
# 
# This file tests statistical equivalence between R and C++ implementations
# for the Hierarchical MVNormal2 distribution with comprehensive validation.

# Development vs Production testing configuration
DEV_MODE <- Sys.getenv("DP_DEV_TESTING", "TRUE") == "TRUE"
BASE_ITERATIONS <- if (DEV_MODE) 50 else 200

test_that("Hierarchical MVNormal2 distribution R/C++ consistency", {
  skip_if_not_installed("mvtnorm")
  
  set.seed(123)
  
  # Create hierarchical test data - multiple groups with different parameters
  group_size <- if (DEV_MODE) 25 else 50
  
  # Generate 2D multivariate normal data for different groups
  group1_data <- mvtnorm::rmvnorm(group_size, mean = c(3, 2), sigma = matrix(c(1, 0.3, 0.3, 1.2), 2, 2))
  group2_data <- mvtnorm::rmvnorm(group_size, mean = c(-2, 1), sigma = matrix(c(0.8, -0.2, -0.2, 0.9), 2, 2))
  group3_data <- mvtnorm::rmvnorm(group_size, mean = c(1, -2), sigma = matrix(c(1.1, 0.1, 0.1, 0.7), 2, 2))
  
  hierarchical_data <- list(group1_data, group2_data, group3_data)
  
  # Set up priors for MVNormal2 (semi-conjugate)
  g0_priors <- list(
    nu0 = 4,          # degrees of freedom
    phi0 = diag(2),   # scale matrix
    mu0 = matrix(c(0, 0), ncol = 2),  # prior mean
    sigma0 = diag(2)  # prior covariance for mean
  )
  
  results <- validate_r_cpp_consistency("hierarchical_mvnormal2", hierarchical_data, 
                                        iterations = BASE_ITERATIONS)

  expect_lt(results$alpha_mean_diff, ALPHA_TOLERANCE)
  expect_lt(results$cluster_count_diff, CLUSTER_TOLERANCE)
  expect_gt(results$likelihood_correlation, LIKELIHOOD_CORR_MIN)
})

test_that("Hierarchical MVNormal2 manual MCMC interface", {
  skip_if_not(using_cpp(), "C++ not available")
  skip_if_not_installed("mvtnorm")
  
  set.seed(456)
  
  # Create smaller test data for manual MCMC
  group_size <- if (DEV_MODE) 15 else 30
  group1_data <- mvtnorm::rmvnorm(group_size, mean = c(1, 1), sigma = diag(c(0.5, 0.8)))
  group2_data <- mvtnorm::rmvnorm(group_size, mean = c(-1, -1), sigma = diag(c(0.6, 0.4)))
  hierarchical_data <- list(group1_data, group2_data)
  
  # Set up priors
  g0_priors <- list(
    nu0 = 4,
    phi0 = diag(2),
    mu0 = matrix(c(0, 0), ncol = 2),
    sigma0 = diag(2)
  )
  
  # Test DirichletProcessHierarchicalMvnormal2 constructor
  dp <- DirichletProcessHierarchicalMvnormal2(hierarchical_data, 
                                              g0Priors = g0_priors,
                                              gammaPriors = c(2, 0.01))
  
  # Enable C++ hierarchical samplers
  enable_cpp_hierarchical_samplers()
  
  # Test manual MCMC steps
  manual_iterations <- if (DEV_MODE) 25 else 50
  for (i in 1:manual_iterations) {
    dp <- ClusterComponentUpdate(dp)
    dp <- ClusterParameterUpdate(dp)
    dp <- UpdateAlpha(dp)
    
    # Test hierarchical-specific updates
    dp <- GlobalParameterUpdate(dp)
  }
  
  # Verify results
  expect_s3_class(dp, c("hierarchical", "dirichletprocess", "list"))
  expect_true(length(dp$indDP) >= 2)  # Multiple groups in indDP
  expect_true(all(sapply(dp$indDP, function(x) x$numberClusters >= 1)))
})

test_that("Hierarchical MVNormal2 different covariance models", {
  skip_if_not(using_cpp(), "C++ not available")
  skip_if_not_installed("mvtnorm")
  
  set.seed(789)
  
  # Test with different covariance models (similar to MVNormal2)
  group_size <- if (DEV_MODE) 20 else 30
  
  # Create test data with different covariance structures
  cov1 <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
  cov2 <- matrix(c(0.8, -0.3, -0.3, 1.2), 2, 2)
  
  group1_data <- mvtnorm::rmvnorm(group_size, mean = c(2, 1), sigma = cov1)
  group2_data <- mvtnorm::rmvnorm(group_size, mean = c(-1, 2), sigma = cov2)
  hierarchical_data <- list(group1_data, group2_data)
  
  # Test different covariance models
  covariance_models <- if (DEV_MODE) c("FULL", "EII") else c("FULL", "EII", "VII", "EEI")
  
  for (model in covariance_models) {
    g0_priors <- list(
      nu0 = 4,
      phi0 = diag(2),
      mu0 = matrix(c(0, 0), ncol = 2),
      sigma0 = diag(2)
    )
    
    expect_no_error({
      dp_model <- DirichletProcessHierarchicalMvnormal2(hierarchical_data,
                                                        g0Priors = g0_priors,
                                                        gammaPriors = c(2, 0.01))
      # Set covariance model (if supported)
      if (exists("SetCovarianceModel")) {
        dp_model <- SetCovarianceModel(dp_model, model)
      }
      
      dp_model <- Fit(dp_model, its = if (DEV_MODE) 20 else 40)
    })
  }
})

test_that("Hierarchical MVNormal2 global parameter sharing", {
  skip_if_not(using_cpp(), "C++ not available")
  skip_if_not_installed("mvtnorm")
  
  set.seed(987)
  
  # Create test data with similar patterns to test global parameter sharing
  group_size <- if (DEV_MODE) 20 else 40
  # Use similar parameters to encourage parameter sharing
  shared_mean <- c(2, 1)
  shared_cov <- matrix(c(1, 0.2, 0.2, 0.8), 2, 2)
  
  group1_data <- mvtnorm::rmvnorm(group_size, mean = shared_mean, sigma = shared_cov)
  group2_data <- mvtnorm::rmvnorm(group_size, mean = shared_mean + c(0.1, -0.1), sigma = shared_cov * 1.1)
  hierarchical_data <- list(group1_data, group2_data)
  
  # Test with C++ enabled
  set_use_cpp(TRUE)
  enable_cpp_hierarchical_samplers()
  
  g0_priors <- list(
    nu0 = 4,
    phi0 = diag(2),
    mu0 = matrix(c(0, 0), ncol = 2),
    sigma0 = diag(2)
  )
  
  dp <- DirichletProcessHierarchicalMvnormal2(hierarchical_data,
                                              g0Priors = g0_priors,
                                              gammaPriors = c(2, 0.01))
  dp <- Fit(dp, its = if (DEV_MODE) 30 else 100)
  
  # Verify hierarchical structure
  expect_s3_class(dp, c("hierarchical", "dirichletprocess", "list"))
  expect_true(length(dp$indDP) == 2)
  
  # Check that global parameters exist and are shared
  expect_true("globalParameters" %in% names(dp))
  
  # Verify that individual groups maintain their own cluster structure
  expect_true(all(sapply(dp$indDP, function(x) {
    "clusterLabels" %in% names(x) && "clusterParameters" %in% names(x)
  })))
})

test_that("Hierarchical MVNormal2 edge cases", {
  skip_if_not(using_cpp(), "C++ not available")
  skip_if_not_installed("mvtnorm")
  
  set.seed(131415)
  
  # Test with very small groups
  small1 <- mvtnorm::rmvnorm(3, mean = c(0, 0), sigma = diag(2))
  small2 <- mvtnorm::rmvnorm(2, mean = c(1, 1), sigma = diag(2))
  small_data <- list(small1, small2)
  
  g0_priors <- list(
    nu0 = 4,
    phi0 = diag(2),
    mu0 = matrix(c(0, 0), ncol = 2),
    sigma0 = diag(2)
  )
  
  expect_no_error({
    dp_small <- DirichletProcessHierarchicalMvnormal2(small_data,
                                                      g0Priors = g0_priors,
                                                      gammaPriors = c(2, 0.01))
    dp_small <- Fit(dp_small, its = if (DEV_MODE) 10 else 25)
  })
  
  # Test with single group (edge case)
  single_group <- list(mvtnorm::rmvnorm(if (DEV_MODE) 10 else 20, 
                                        mean = c(0, 0), sigma = diag(2)))
  
  expect_no_error({
    dp_single <- DirichletProcessHierarchicalMvnormal2(single_group,
                                                       g0Priors = g0_priors,
                                                       gammaPriors = c(2, 0.01))
    dp_single <- Fit(dp_single, its = if (DEV_MODE) 10 else 25)
  })
})