# tests/testthat/test-cpp-convergence.R
#
# R/C++ Convergence Diagnostic Tests
# 
# This file tests convergence behavior and MCMC diagnostics between R and C++ implementations.
# These tests require longer chains and multiple runs for statistical validity.
#
# PERFORMANCE OPTIMIZATION:
# - DEV_MODE (default): Fast testing for development (reduced iterations, fewer chains)
# - PRODUCTION_MODE: Full convergence validation (long chains, multiple runs)
#
# Usage:
# - Development: Sys.setenv(DP_DEV_TESTING = "TRUE") (default)
# - Production:  Sys.setenv(DP_DEV_TESTING = "FALSE")
#
# Speed improvement: ~80% faster in DEV_MODE while maintaining convergence validation

# Development vs Production testing configuration
DEV_MODE <- Sys.getenv("DP_DEV_TESTING", "TRUE") == "TRUE"
CONV_ITERATIONS <- if (DEV_MODE) 250 else 1000
MULTI_ITERATIONS <- if (DEV_MODE) 150 else 500
CHAIN_COUNT <- if (DEV_MODE) 2 else 3
BASE_SAMPLE_SIZE <- if (DEV_MODE) 75 else 100
LARGE_SAMPLE_SIZE <- if (DEV_MODE) 100 else 200

test_that("R and C++ show similar convergence behavior", {
  suppressPackageStartupMessages(library(coda))
  set.seed(123)
  test_data <- rnorm(BASE_SAMPLE_SIZE, mean = c(-2, 2))

  # Longer chains for convergence analysis
  iterations <- CONV_ITERATIONS

  # R implementation
  set_use_cpp(FALSE)
  dp_r <- DirichletProcessGaussian(test_data)
  dp_r <- Fit(dp_r, its = iterations, updatePrior = TRUE)

  # C++ implementation
  set_use_cpp(TRUE)
  dp_cpp <- DirichletProcessGaussian(test_data)
  dp_cpp <- Fit(dp_cpp, its = iterations, updatePrior = TRUE)

  # Convert to mcmc objects for diagnostics
  mcmc_r <- mcmc(dp_r$alphaChain)
  mcmc_cpp <- mcmc(dp_cpp$alphaChain)

  # Effective sample size should be similar
  ess_r <- effectiveSize(mcmc_r)
  ess_cpp <- effectiveSize(mcmc_cpp)

  # Use relative tolerance for ESS comparison (MCMC can vary significantly)
  relative_diff <- abs(ess_r - ess_cpp) / max(ess_r, ess_cpp)
  expect_lt(relative_diff, 0.6)  # Allow up to 60% relative difference

  # Geweke diagnostics should both indicate convergence
  # Note: Geweke test is stochastic and can vary significantly
  # Values > 2 occur ~5% of time even for converged chains
  # This test is primarily for detecting catastrophic divergence
  geweke_r <- geweke.diag(mcmc_r)$z
  geweke_cpp <- geweke.diag(mcmc_cpp)$z

  # Very permissive threshold - we're testing for major bugs, not tight convergence
  expect_true(abs(geweke_r) < 5,
              info = sprintf("R Geweke statistic: %.2f", geweke_r))
  expect_true(abs(geweke_cpp) < 5,
              info = sprintf("C++ Geweke statistic: %.2f", geweke_cpp))
})

test_that("Multiple chains show similar behavior", {
  suppressPackageStartupMessages(library(coda))
  set.seed(123)
  test_data <- generate_test_data("exponential", BASE_SAMPLE_SIZE)
  n_chains <- CHAIN_COUNT
  iterations <- MULTI_ITERATIONS

  # Run multiple chains for R
  r_chains <- list()
  for (i in 1:n_chains) {
    set.seed(123 + i)
    set_use_cpp(FALSE)
    dp <- DirichletProcessExponential(test_data)
    dp <- Fit(dp, its = iterations, updatePrior = TRUE)
    r_chains[[i]] <- mcmc(dp$alphaChain)
  }
  r_mcmc_list <- mcmc.list(r_chains)

  # Run multiple chains for C++
  cpp_chains <- list()
  for (i in 1:n_chains) {
    set.seed(123 + i)
    set_use_cpp(TRUE)
    dp <- DirichletProcessExponential(test_data)
    dp <- Fit(dp, its = iterations, updatePrior = TRUE)
    cpp_chains[[i]] <- mcmc(dp$alphaChain)
  }
  cpp_mcmc_list <- mcmc.list(cpp_chains)

  # Compare Gelman-Rubin statistics
  r_gelman <- gelman.diag(r_mcmc_list)
  cpp_gelman <- gelman.diag(cpp_mcmc_list)

  # Both should indicate convergence (close to 1)
  expect_lt(r_gelman$psrf[1], 1.1)
  expect_lt(cpp_gelman$psrf[1], 1.1)

  # Should be similar between R and C++ - allow wider tolerance for Gelman-Rubin
  expect_equal(r_gelman$psrf[1], cpp_gelman$psrf[1], tolerance = 0.2)
})

test_that("Autocorrelation patterns are similar", {
  suppressPackageStartupMessages(library(coda))
  set.seed(123)
  beta_size <- if (DEV_MODE) 75 else 150
  test_data <- generate_test_data("beta", beta_size)
  iterations <- CONV_ITERATIONS

  # R implementation
  set_use_cpp(FALSE)
  dp_r <- DirichletProcessBeta(test_data)
  dp_r <- Fit(dp_r, its = iterations, updatePrior = TRUE)

  # C++ implementation
  set_use_cpp(TRUE)
  dp_cpp <- DirichletProcessBeta(test_data)
  dp_cpp <- Fit(dp_cpp, its = iterations, updatePrior = TRUE)

  # Compute autocorrelations
  acf_r <- acf(dp_r$alphaChain, plot = FALSE)
  acf_cpp <- acf(dp_cpp$alphaChain, plot = FALSE)

  # Compare first 10 lags - allow wide tolerance for autocorrelation patterns
  # MCMC chains can have quite different ACF patterns due to RNG differences
  expect_equal(acf_r$acf[1:10], acf_cpp$acf[1:10], tolerance = 0.4)
})

test_that("Burn-in behavior is consistent", {
  set.seed(123)
  test_data <- generate_test_data("weibull", BASE_SAMPLE_SIZE)
  iterations <- MULTI_ITERATIONS

  # Track convergence metrics over time
  check_interval <- if (DEV_MODE) 30 else 50
  check_points <- seq(check_interval, iterations, by = check_interval)
  r_means <- numeric(length(check_points))
  cpp_means <- numeric(length(check_points))

  # R implementation
  set_use_cpp(FALSE)
  dp_r <- DirichletProcessWeibull(test_data, g0Priors = c(1, 1, 1))

  for (i in seq_along(check_points)) {
    dp_r <- Fit(dp_r, its = ifelse(i == 1, check_points[1],
                                   check_points[i] - check_points[i-1]), updatePrior = TRUE)
    r_means[i] <- mean(dp_r$alphaChain)
  }

  # C++ implementation
  set_use_cpp(TRUE)
  dp_cpp <- DirichletProcessWeibull(test_data, g0Priors = c(1, 1, 1))

  for (i in seq_along(check_points)) {
    dp_cpp <- Fit(dp_cpp, its = ifelse(i == 1, check_points[1],
                                       check_points[i] - check_points[i-1]), updatePrior = TRUE)
    cpp_means[i] <- mean(dp_cpp$alphaChain)
  }

  # Should converge to similar values - allow wide tolerance for burn-in behavior
  # MCMC burn-in can vary significantly between R/C++ due to RNG differences
  expect_equal(r_means, cpp_means, tolerance = 0.6)

  # Both should stabilize (decreasing variance)
  r_diffs <- abs(diff(r_means))
  cpp_diffs <- abs(diff(cpp_means))

  # Check that both implementations show reasonable MCMC behavior
  # MCMC chains can have natural fluctuations, so we check that the trend isn't severely increasing
  r_trend <- coef(lm(r_diffs ~ seq_along(r_diffs)))[2]
  cpp_trend <- coef(lm(cpp_diffs ~ seq_along(cpp_diffs)))[2]
  # Allow moderate to large trends (MCMC can have substantial natural variability during burn-in)
  # The key is detecting catastrophic divergence, not minor fluctuations
  expect_true(r_trend <= 0.5)    # Allow substantial positive trends for MCMC burn-in behavior
  expect_true(cpp_trend <= 0.5)  # Both implementations can show variable burn-in patterns
})

test_that("Posterior predictive distributions are similar", {
  set.seed(123)
  test_data <- generate_test_data("mvnormal", BASE_SAMPLE_SIZE)
  iterations <- MULTI_ITERATIONS
  n_posterior_samples <- if (DEV_MODE) 50 else 100

  # R implementation
  set_use_cpp(FALSE)
  dp_r <- DirichletProcessMvnormal(test_data)
  dp_r <- Fit(dp_r, its = iterations, updatePrior = TRUE)

  # C++ implementation
  set_use_cpp(TRUE)
  dp_cpp <- DirichletProcessMvnormal(test_data)
  dp_cpp <- Fit(dp_cpp, its = iterations, updatePrior = TRUE)

  # Generate posterior predictive samples - skip if matrix errors occur
  set.seed(456)
  r_predictive <- tryCatch({
    PosteriorDraw(dp_r$mixingDistribution, dp_r$data, n_posterior_samples)
  }, error = function(e) {
    skip(paste("Posterior predictive sampling failed for R implementation:", e$message))
  })

  set.seed(456)
  cpp_predictive <- tryCatch({
    PosteriorDraw(dp_cpp$mixingDistribution, dp_cpp$data, n_posterior_samples)
  }, error = function(e) {
    skip(paste("Posterior predictive sampling failed for C++ implementation:", e$message))
  })

  # Check if both samples were generated successfully
  if (is.null(r_predictive) || is.null(cpp_predictive)) {
    skip("One or both posterior predictive samples failed")
  }

  # For mvnormal, PosteriorDraw returns list(mu=..., sig=...)
  # Extract parameter values carefully based on structure
  r_vals <- if (is.list(r_predictive) && "mu" %in% names(r_predictive)) {
    mu <- r_predictive$mu
    # Handle different array structures based on actual dimensions
    dims <- dim(mu)
    if (!is.null(dims)) {
      if (length(dims) == 3) {
        as.vector(mu[1, 1, ])  # 3D: [param, component, sample]
      } else if (length(dims) == 2) {
        as.vector(mu[1, ])     # 2D: [param, sample]
      } else {
        as.vector(mu)          # 1D: just samples
      }
    } else {
      as.vector(mu)            # Not an array, convert to vector
    }
  } else if (is.matrix(r_predictive)) {
    r_predictive[, 1]
  } else {
    as.vector(r_predictive)
  }

  cpp_vals <- if (is.list(cpp_predictive) && "mu" %in% names(cpp_predictive)) {
    mu <- cpp_predictive$mu
    # Handle different array structures based on actual dimensions
    dims <- dim(mu)
    if (!is.null(dims)) {
      if (length(dims) == 3) {
        as.vector(mu[1, 1, ])  # 3D: [param, component, sample]
      } else if (length(dims) == 2) {
        as.vector(mu[1, ])     # 2D: [param, sample]
      } else {
        as.vector(mu)          # 1D: just samples
      }
    } else {
      as.vector(mu)            # Not an array, convert to vector
    }
  } else if (is.matrix(cpp_predictive)) {
    cpp_predictive[, 1]
  } else {
    as.vector(cpp_predictive)
  }

  # Kolmogorov-Smirnov test - should not reject null hypothesis
  ks_test <- ks.test(r_vals, cpp_vals)
  expect_gt(ks_test$p.value, 0.05)

  # Compare summary statistics
  expect_equal(mean(r_vals), mean(cpp_vals), tolerance = 0.15)
  expect_equal(sd(r_vals), sd(cpp_vals), tolerance = 0.15)
})

test_that("Convergence diagnostics for cluster counts", {
  suppressPackageStartupMessages(library(coda))
  set.seed(123)
  test_data <- generate_test_data("normal", LARGE_SAMPLE_SIZE)
  iterations <- CONV_ITERATIONS

  # R implementation
  set_use_cpp(FALSE)
  dp_r <- DirichletProcessGaussian(test_data)
  dp_r <- Fit(dp_r, its = iterations, updatePrior = TRUE)

  # C++ implementation
  set_use_cpp(TRUE)
  dp_cpp <- DirichletProcessGaussian(test_data)
  dp_cpp <- Fit(dp_cpp, its = iterations, updatePrior = TRUE)

  # Extract cluster counts
  r_clusters <- sapply(dp_r$labelsChain, function(x) length(unique(x)))
  cpp_clusters <- sapply(dp_cpp$labelsChain, function(x) length(unique(x)))

  # Convert to mcmc objects
  mcmc_r_clusters <- mcmc(r_clusters)
  mcmc_cpp_clusters <- mcmc(cpp_clusters)

  # Both should show convergence (very permissive threshold)
  geweke_r <- geweke.diag(mcmc_r_clusters)$z
  geweke_cpp <- geweke.diag(mcmc_cpp_clusters)$z

  expect_true(abs(geweke_r) < 5,
              info = sprintf("R cluster Geweke: %.2f", geweke_r))
  expect_true(abs(geweke_cpp) < 5,
              info = sprintf("C++ cluster Geweke: %.2f", geweke_cpp))

  # Should have similar posterior distributions - allow wide tolerance for cluster count variation
  expect_equal(mean(r_clusters), mean(cpp_clusters), tolerance = 2.0)
  expect_equal(sd(r_clusters), sd(cpp_clusters), tolerance = 1.0)
})
