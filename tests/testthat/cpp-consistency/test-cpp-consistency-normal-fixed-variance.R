# tests/testthat/test-cpp-consistency-normal-fixed-variance.R
#
# R/C++ Consistency Tests for Normal Fixed Variance Distribution
# 
# This file tests statistical equivalence between R and C++ implementations
# for the Normal distribution with fixed variance.

# Development vs Production testing configuration
DEV_MODE <- Sys.getenv("DP_DEV_TESTING", "TRUE") == "TRUE"
BASE_ITERATIONS <- if (DEV_MODE) 50 else 200

test_that("Normal Fixed Variance distribution R/C++ consistency", {
  set.seed(123)
  
  normal_size <- if (DEV_MODE) 25 else 50
  # Generate normal data with known variance
  fixed_sigma <- 1.5
  test_data <- c(rnorm(normal_size, mean = -2, sd = fixed_sigma),
                 rnorm(normal_size, mean = 2, sd = fixed_sigma))
  
  results <- validate_r_cpp_consistency("normal_fixed_variance", test_data, 
                                        iterations = BASE_ITERATIONS)

  expect_lt(results$alpha_mean_diff, ALPHA_TOLERANCE)
  expect_lt(results$cluster_count_diff, CLUSTER_TOLERANCE)
  expect_gt(results$likelihood_correlation, LIKELIHOOD_CORR_MIN)
})

test_that("Normal Fixed Variance manual MCMC interface", {
  skip_if_not(using_cpp(), "C++ not available")
  
  set.seed(456)
  
  # Create smaller test data for manual MCMC
  test_size <- if (DEV_MODE) 15 else 30
  fixed_sigma <- 1.0
  test_data <- c(rnorm(test_size, mean = -1, sd = fixed_sigma),
                 rnorm(test_size, mean = 1, sd = fixed_sigma))
  
  # Test GaussianFixedVariance constructor
  dp <- DirichletProcessGaussianFixedVariance(test_data, sigma = fixed_sigma)
  
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
  
  # Check that mixing distribution maintains fixed variance
  expect_true(inherits(dp$mixingDistribution, "normalFixedVariance"))
  expect_equal(dp$mixingDistribution$sigma, fixed_sigma)
})

test_that("Normal Fixed Variance with different sigma values", {
  skip_if_not(using_cpp(), "C++ not available")
  
  set.seed(789)
  
  # Test with different fixed variances
  sigma_values <- if (DEV_MODE) c(0.5, 1.0) else c(0.5, 1.0, 2.0, 5.0)
  
  for (sigma in sigma_values) {
    test_size <- if (DEV_MODE) 15 else 25
    # Generate data with the specified variance
    test_data <- c(rnorm(test_size, mean = -1, sd = sigma),
                   rnorm(test_size, mean = 1, sd = sigma))
    
    expect_error({
      dp_sigma <- DirichletProcessGaussianFixedVariance(test_data, sigma = sigma)
      dp_sigma <- Fit(dp_sigma, its = if (DEV_MODE) 20 else 40)
    }, regexp = NA, info = paste("sigma =", sigma))
    
    # Verify fixed variance is maintained
    expect_equal(dp_sigma$mixingDistribution$sigma, sigma,
                info = paste("Fixed variance maintained for sigma =", sigma))
    
    # Verify clustering occurred
    expect_true(dp_sigma$numberClusters >= 1,
                info = paste("Clustering occurred for sigma =", sigma))
  }
})

test_that("Normal Fixed Variance with different priors", {
  skip_if_not(using_cpp(), "C++ not available")
  
  set.seed(987)
  
  # Test with different prior parameters for the mean
  g0_priors_list <- if (DEV_MODE) {
    list(c(0, 1), c(0, 4))
  } else {
    list(c(0, 1), c(0, 4), c(2, 1), c(-1, 2))
  }
  
  test_size <- if (DEV_MODE) 20 else 30
  fixed_sigma <- 1.0
  test_data <- rnorm(test_size, mean = 0, sd = fixed_sigma)
  
  for (g0_prior in g0_priors_list) {
    expect_error({
      dp_prior <- DirichletProcessGaussianFixedVariance(test_data, 
                                                        sigma = fixed_sigma,
                                                        g0Priors = g0_prior)
      dp_prior <- Fit(dp_prior, its = if (DEV_MODE) 20 else 40)
    }, regexp = NA, info = paste("g0Priors =", paste(g0_prior, collapse = ", ")))
    
    # Verify basic properties
    expect_s3_class(dp_prior, "dirichletprocess")
    expect_true(dp_prior$numberClusters >= 1)
    expect_equal(dp_prior$mixingDistribution$sigma, fixed_sigma)
  }
})

test_that("Normal Fixed Variance conjugacy properties", {
  skip_if_not(using_cpp(), "C++ not available")
  
  set.seed(131415)
  
  # Test conjugate properties with known mean structure
  test_size <- if (DEV_MODE) 20 else 40
  fixed_sigma <- 1.0
  true_means <- c(-2, 0, 2)
  
  # Generate data from known clusters
  cluster_size <- test_size %/% length(true_means)
  test_data <- c()
  for (mean_val in true_means) {
    test_data <- c(test_data, rnorm(cluster_size, mean = mean_val, sd = fixed_sigma))
  }
  
  # Test with informative prior
  dp <- DirichletProcessGaussianFixedVariance(test_data, 
                                              sigma = fixed_sigma,
                                              g0Priors = c(0, 0.1))  # Tight prior around 0
  dp <- Fit(dp, its = if (DEV_MODE) 30 else 100)
  
  # Verify conjugate update properties
  expect_true(dp$numberClusters >= 2)  # Should find multiple clusters
  expect_true(inherits(dp$mixingDistribution, "normalFixedVariance"))
  
  # Check cluster means are reasonable
  if (dp$numberClusters > 0 && length(dp$clusterParameters) > 0) {
    # Handle different possible structures of clusterParameters
    if (is.list(dp$clusterParameters) && length(dp$clusterParameters) == 1 && is.array(dp$clusterParameters[[1]])) {
      # List containing a 3D array: extract means from third dimension of the array
      param_array <- dp$clusterParameters[[1]]
      if (length(dim(param_array)) == 3) {
        cluster_means <- param_array[1, 1, 1:dp$numberClusters]
      } else {
        cluster_means <- as.numeric(param_array[1:dp$numberClusters])
      }
    } else if (is.array(dp$clusterParameters) && length(dim(dp$clusterParameters)) == 3) {
      # 3D array structure: extract means from third dimension
      cluster_means <- dp$clusterParameters[1, 1, 1:dp$numberClusters]
    } else if (is.list(dp$clusterParameters)) {
      # List structure: extract first element from each list item
      cluster_means <- sapply(dp$clusterParameters, function(x) x[1])
    } else {
      # Fallback: try to extract as vector
      cluster_means <- as.numeric(dp$clusterParameters[1:dp$numberClusters])
    }
    
    expect_true(length(cluster_means) == dp$numberClusters)
    expect_true(all(is.finite(cluster_means)))
  }
})

test_that("Normal Fixed Variance edge cases", {
  skip_if_not(using_cpp(), "C++ not available")
  
  set.seed(161718)
  
  # Test with very small dataset
  small_data <- rnorm(3, mean = 0, sd = 1)
  fixed_sigma <- 1.0
  
  expect_no_error({
    dp_small <- DirichletProcessGaussianFixedVariance(small_data, sigma = fixed_sigma)
    dp_small <- Fit(dp_small, its = if (DEV_MODE) 10 else 25)
  })
  
  # Test with constant data (challenging for clustering)
  constant_data <- rep(2.5, if (DEV_MODE) 5 else 10)
  
  expect_no_error({
    dp_constant <- DirichletProcessGaussianFixedVariance(constant_data, sigma = fixed_sigma)
    dp_constant <- Fit(dp_constant, its = if (DEV_MODE) 10 else 25)
  })
  
  # Test with very small variance
  small_sigma <- 0.01
  precise_data <- c(rnorm(5, mean = 0, sd = small_sigma),
                    rnorm(5, mean = 1, sd = small_sigma))
  
  expect_no_error({
    dp_precise <- DirichletProcessGaussianFixedVariance(precise_data, sigma = small_sigma)
    dp_precise <- Fit(dp_precise, its = if (DEV_MODE) 10 else 25)
  })
  
  # Verify small variance is maintained
  expect_equal(dp_precise$mixingDistribution$sigma, small_sigma)
})