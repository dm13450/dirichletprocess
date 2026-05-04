# tests/integration/stress_tests.R

# Load required library
library(dirichletprocess)

# Development/Production Mode Configuration
# Set DP_DEV_TESTING=TRUE for development mode (faster, smaller tests)
# Set DP_DEV_TESTING=FALSE for production mode (full validation)
is_dev_mode <- function() {
  dev_env <- Sys.getenv("DP_DEV_TESTING", unset = "TRUE")
  return(tolower(dev_env) %in% c("true", "1", "yes"))
}

get_stress_params <- function() {
  if (is_dev_mode()) {
    list(
      # Basic stress tests
      large_dataset_size = 5000,        # vs 50000 in production
      large_dataset_its = 5,            # vs 10 in production
      many_clusters_groups = 10,        # vs 50 in production
      many_clusters_per_group = 10,     # vs 20 in production
      many_clusters_its = 20,           # vs 100 in production
      high_dim_dimensions = 5,          # vs 20 in production
      high_dim_size = 200,              # vs 1000 in production
      high_dim_its = 5,                 # vs 10 in production
      extreme_params_its = 20,          # vs 50 in production
      
      # Extended stress tests
      long_run_its = 1000,              # vs 10000 in production
      long_run_size = 100,              # vs 500 in production
      dynamic_clusters_groups = 5,      # vs 10 in production
      dynamic_clusters_per_group = 20,  # vs 50 in production
      dynamic_clusters_its = 100,       # vs 500 in production
      
      # Robustness tests
      repeated_fitting_runs = 3,        # vs 10 in production
      repeated_fitting_its = 20,        # vs 100 in production
      bad_init_recovery_its = 50,       # vs 200 in production
      outlier_handling_its = 50,        # vs 200 in production
      
      # Performance tests
      max_sizes = c(1000, 5000),        # vs c(10000, 25000, 50000, 75000, 100000)
      max_dimensions = c(5, 10),        # vs c(10, 25, 50, 75, 100)
      perf_test_its = 3                 # vs 5 in production
    )
  } else {
    list(
      # Basic stress tests
      large_dataset_size = 50000,
      large_dataset_its = 10,
      many_clusters_groups = 50,
      many_clusters_per_group = 20,
      many_clusters_its = 100,
      high_dim_dimensions = 20,
      high_dim_size = 1000,
      high_dim_its = 10,
      extreme_params_its = 50,
      
      # Extended stress tests
      long_run_its = 10000,
      long_run_size = 500,
      dynamic_clusters_groups = 10,
      dynamic_clusters_per_group = 50,
      dynamic_clusters_its = 500,
      
      # Robustness tests
      repeated_fitting_runs = 10,
      repeated_fitting_its = 100,
      bad_init_recovery_its = 200,
      outlier_handling_its = 200,
      
      # Performance tests
      max_sizes = c(10000, 25000, 50000, 75000, 100000),
      max_dimensions = c(10, 25, 50, 75, 100),
      perf_test_its = 5
    )
  }
}

# Load helper functions if available
if (file.exists("tests/testthat/helper-testing.R")) {
  source("tests/testthat/helper-testing.R")
} else {
  # Basic fallback functions
  generate_test_data <- function(distribution, n = 100) {
    switch(distribution,
      "normal" = rnorm(n),
      "exponential" = rexp(n),
      "beta" = rbeta(n, 2, 2),
      "weibull" = rweibull(n, 2),
      "mvnormal" = matrix(rnorm(n * 3), ncol = 3),
      rnorm(n)
    )
  }
  
  create_dp_object <- function(distribution, data) {
    switch(distribution,
      "normal" = DirichletProcessGaussian(data),
      "exponential" = DirichletProcessExponential(data),
      "beta" = DirichletProcessBeta(data),
      "weibull" = DirichletProcessWeibull(data),
      "mvnormal" = DirichletProcessMvnormal(data),
      DirichletProcessGaussian(data)
    )
  }
}

run_stress_tests <- function() {
  params <- get_stress_params()
  mode_info <- if (is_dev_mode()) "DEV" else "PROD"
  
  cat("\n=== BASIC STRESS TESTS [", mode_info, " MODE] ===\n")
  stress_results <- list()

  # 1. Large dataset test
  cat("\n1. Testing large dataset (n=", params$large_dataset_size, ")...\n")
  cat("   MCMC iterations:", params$large_dataset_its, "\n")
  large_data <- rnorm(params$large_dataset_size)

  tryCatch({
    dp <- DirichletProcessGaussian(large_data)
    dp <- Fit(dp, its = params$large_dataset_its)
    stress_results$large_dataset <- "PASSED"
    cat("   ✅ Large dataset test PASSED\n")
  }, error = function(e) {
    stress_results$large_dataset <- paste("FAILED:", e$message)
    cat("   ❌ Large dataset test FAILED:", e$message, "\n")
  })

  # 2. Many clusters test
  cat("\n2. Testing many clusters scenario...\n")
  cat("   Groups:", params$many_clusters_groups, "| Per group:", params$many_clusters_per_group, "| MCMC its:", params$many_clusters_its, "\n")
  many_clusters_data <- c()
  for (i in 1:params$many_clusters_groups) {
    many_clusters_data <- c(many_clusters_data, rnorm(params$many_clusters_per_group, mean = i * 5))
  }

  tryCatch({
    dp <- DirichletProcessGaussian(many_clusters_data)
    dp <- Fit(dp, its = params$many_clusters_its)
    stress_results$many_clusters <- paste("PASSED - Found", dp$numberClusters, "clusters")
    cat("   ✅ Many clusters test PASSED - Found", dp$numberClusters, "clusters\n")
  }, error = function(e) {
    stress_results$many_clusters <- paste("FAILED:", e$message)
    cat("   ❌ Many clusters test FAILED:", e$message, "\n")
  })

  # 3. High dimension test (MVNormal)
  cat("\n3. Testing high dimensions (d=", params$high_dim_dimensions, ")...\n")
  cat("   Sample size:", params$high_dim_size, "| MCMC its:", params$high_dim_its, "\n")
  high_dim_data <- matrix(rnorm(params$high_dim_size * params$high_dim_dimensions), ncol = params$high_dim_dimensions)

  tryCatch({
    dp <- DirichletProcessMvnormal(high_dim_data)
    dp <- Fit(dp, its = params$high_dim_its)
    stress_results$high_dimension <- "PASSED"
    cat("   ✅ High dimension test PASSED\n")
  }, error = function(e) {
    stress_results$high_dimension <- paste("FAILED:", e$message)
    cat("   ❌ High dimension test FAILED:", e$message, "\n")
  })

  # 4. Extreme parameter values
  cat("\n4. Testing extreme parameters...\n")
  cat("   MCMC iterations:", params$extreme_params_its, "\n")
  extreme_data <- c(rnorm(100, sd = 0.01), rnorm(100, sd = 1000))

  tryCatch({
    dp <- DirichletProcessGaussian(extreme_data)
    dp <- Fit(dp, its = params$extreme_params_its)
    stress_results$extreme_params <- "PASSED"
    cat("   ✅ Extreme parameters test PASSED\n")
  }, error = function(e) {
    stress_results$extreme_params <- paste("FAILED:", e$message)
    cat("   ❌ Extreme parameters test FAILED:", e$message, "\n")
  })

  # 5. Concurrent execution test
  cat("\n5. Testing concurrent execution...\n")
  if (requireNamespace("parallel", quietly = TRUE)) {
    cl <- parallel::makeCluster(4)

    tryCatch({
      parallel::clusterEvalQ(cl, {
        library(dirichletprocess)
        set_use_cpp(TRUE)
      })

      results <- parallel::parLapply(cl, 1:4, function(i) {
        dp <- DirichletProcessGaussian(rnorm(1000))
        dp <- Fit(dp, its = 100)
        return(dp$numberClusters)
      })

      stress_results$concurrent <- "PASSED"
    }, error = function(e) {
      stress_results$concurrent <- paste("FAILED:", e$message)
    }, finally = {
      parallel::stopCluster(cl)
    })
  }

  return(stress_results)
}

# Extended stress tests
run_extended_stress_tests <- function() {
  params <- get_stress_params()
  mode_info <- if (is_dev_mode()) "DEV" else "PROD"
  
  cat("\n=== EXTENDED STRESS TESTS [", mode_info, " MODE] ===\n")

  results <- list()

  # 1. Imbalanced clusters
  cat("\n1. Testing highly imbalanced clusters...\n")
  imbalanced_data <- c(rnorm(1000, mean = 0),
                       rnorm(10, mean = 10),
                       rnorm(5, mean = -10))

  results$imbalanced <- tryCatch({
    dp <- DirichletProcessGaussian(imbalanced_data)
    dp <- Fit(dp, its = 100)
    list(
      status = "PASSED",
      clusters_found = dp$numberClusters,
      cluster_sizes = table(dp$clusterLabels)
    )
  }, error = function(e) {
    list(status = "FAILED", error = e$message)
  })

  # 2. Numerical precision stress
  cat("\n2. Testing numerical precision limits...\n")

  # Very small scale
  small_scale_data <- rnorm(100) * 1e-15
  results$small_scale <- tryCatch({
    dp <- DirichletProcessGaussian(small_scale_data)
    dp <- Fit(dp, its = 50)
    "PASSED"
  }, error = function(e) {
    paste("FAILED:", e$message)
  })

  # Very large scale
  large_scale_data <- rnorm(100) * 1e15
  results$large_scale <- tryCatch({
    dp <- DirichletProcessGaussian(large_scale_data)
    dp <- Fit(dp, its = 50)
    "PASSED"
  }, error = function(e) {
    paste("FAILED:", e$message)
  })

  # 3. Long running test
  cat("\n3. Testing long running MCMC (", params$long_run_its, " iterations)...\n")
  long_run_data <- generate_test_data("normal", params$long_run_size)

  results$long_run <- tryCatch({
    start_time <- Sys.time()
    dp <- DirichletProcessGaussian(long_run_data)
    dp <- Fit(dp, its = params$long_run_its)
    end_time <- Sys.time()

    list(
      status = "PASSED",
      runtime = as.numeric(end_time - start_time, units = "secs"),
      final_clusters = dp$numberClusters,
      alpha_mean = mean(dp$alphaChain)
    )
  }, error = function(e) {
    list(status = "FAILED", error = e$message)
  })

  # 4. Rapid cluster changes
  cat("\n4. Testing rapid cluster changes...\n")
  # Data that encourages cluster splitting/merging
  dynamic_data <- c()
  for (i in 1:params$dynamic_clusters_groups) {
    dynamic_data <- c(dynamic_data, rnorm(params$dynamic_clusters_per_group, mean = sin(i) * 10))
  }

  results$dynamic_clusters <- tryCatch({
    dp <- DirichletProcessGaussian(dynamic_data)
    dp <- Fit(dp, its = params$dynamic_clusters_its)

    # Analyze cluster stability
    cluster_counts <- sapply(dp$labelsChain, function(x) length(unique(x)))

    list(
      status = "PASSED",
      min_clusters = min(cluster_counts),
      max_clusters = max(cluster_counts),
      cluster_variance = var(cluster_counts)
    )
  }, error = function(e) {
    list(status = "FAILED", error = e$message)
  })

  # 5. Mixed distribution types stress test
  cat("\n5. Testing all distributions with edge cases...\n")

  edge_case_data <- list(
    normal = c(rnorm(50, -100), rnorm(50, 100)),
    exponential = c(rexp(50, 0.01), rexp(50, 100)),
    beta = c(rbeta(50, 0.1, 0.1), rbeta(50, 10, 10)),
    weibull = c(rweibull(50, 0.1), rweibull(50, 10)),
    mvnormal = rbind(
      mvtnorm::rmvnorm(50, rep(-10, 5), diag(5)),
      mvtnorm::rmvnorm(50, rep(10, 5), diag(5))
    )
  )

  for (dist in names(edge_case_data)) {
    cat("    Testing", dist, "...")

    results[[paste0(dist, "_edge")]] <- tryCatch({
      dp <- create_dp_object(dist, edge_case_data[[dist]])
      dp <- Fit(dp, its = 100)
      "PASSED"
    }, error = function(e) {
      paste("FAILED:", e$message)
    })

    cat(" ", results[[paste0(dist, "_edge")]], "\n")
  }

  return(results)
}

# Robustness tests
test_robustness <- function() {
  params <- get_stress_params()
  mode_info <- if (is_dev_mode()) "DEV" else "PROD"
  
  cat("\n=== ROBUSTNESS TESTS [", mode_info, " MODE] ===\n")

  results <- list()

  # 1. Repeated fitting stability
  cat("\n1. Testing repeated fitting stability...\n")
  test_data <- generate_test_data("normal", 200)

  cluster_results <- numeric(params$repeated_fitting_runs)
  alpha_results <- numeric(params$repeated_fitting_runs)

  for (i in 1:params$repeated_fitting_runs) {
    set.seed(123)  # Same seed each time
    dp <- DirichletProcessGaussian(test_data)
    dp <- Fit(dp, its = params$repeated_fitting_its)

    cluster_results[i] <- dp$numberClusters
    alpha_results[i] <- dp$alpha
  }

  results$repeated_fitting <- list(
    cluster_variance = var(cluster_results),
    alpha_variance = var(alpha_results),
    all_identical = length(unique(cluster_results)) == 1
  )

  # 2. Recovery from bad initialization
  cat("\n2. Testing recovery from bad initialization...\n")
  test_data <- generate_test_data("exponential", 300)
  dp <- DirichletProcessExponential(test_data)

  # Force bad initialization
  dp$clusterLabels <- rep(1, length(test_data))  # All in one cluster
  dp$numberClusters <- 1
  dp$alpha <- 0.001  # Very low alpha

  results$bad_init_recovery <- tryCatch({
    dp <- Fit(dp, its = params$bad_init_recovery_its)
    list(
      status = "PASSED",
      final_clusters = dp$numberClusters,
      recovered = dp$numberClusters > 1
    )
  }, error = function(e) {
    list(status = "FAILED", error = e$message)
  })

  # 3. Handling of outliers
  cat("\n3. Testing outlier handling...\n")
  # Normal data with outliers
  outlier_data <- c(rnorm(200),
                    runif(10, min = -50, max = -40),  # Left outliers
                    runif(10, min = 40, max = 50))     # Right outliers

  results$outlier_handling <- tryCatch({
    dp <- DirichletProcessGaussian(outlier_data)
    dp <- Fit(dp, its = params$outlier_handling_its)

    # Check if outliers are in separate clusters
    outlier_indices <- c(201:220)
    outlier_clusters <- dp$clusterLabels[outlier_indices]
    main_clusters <- dp$clusterLabels[1:200]

    list(
      status = "PASSED",
      total_clusters = dp$numberClusters,
      outliers_separated = length(intersect(unique(outlier_clusters),
                                            unique(main_clusters))) == 0
    )
  }, error = function(e) {
    list(status = "FAILED", error = e$message)
  })

  return(results)
}

# Performance under extreme conditions
test_extreme_performance <- function() {
  params <- get_stress_params()
  mode_info <- if (is_dev_mode()) "DEV" else "PROD"
  
  cat("\n=== EXTREME PERFORMANCE TESTS [", mode_info, " MODE] ===\n")

  results <- list()

  # 1. Maximum practical dataset size
  cat("\n1. Finding maximum practical dataset size...\n")

  max_size <- 0

  for (size in params$max_sizes) {
    cat("  Trying n =", size, "...")

    result <- tryCatch({
      test_data <- rnorm(size)
      start_time <- Sys.time()

      dp <- DirichletProcessGaussian(test_data)
      dp <- Fit(dp, its = params$perf_test_its)

      end_time <- Sys.time()
      runtime <- as.numeric(end_time - start_time, units = "secs")

      if (runtime < 60) {  # Less than 1 minute
        max_size <- size
        cat(" SUCCESS (", round(runtime, 1), "s)\n")
        TRUE
      } else {
        cat(" Too slow (", round(runtime, 1), "s)\n")
        FALSE
      }
    }, error = function(e) {
      cat(" FAILED\n")
      FALSE
    })

    if (!result) break
  }

  results$max_dataset_size <- max_size

  # 2. Maximum dimensions for multivariate
  cat("\n2. Finding maximum dimensions for MVNormal...\n")

  max_dim <- 0

  for (d in params$max_dimensions) {
    cat("  Trying d =", d, "...")

    result <- tryCatch({
      test_data <- matrix(rnorm(1000 * d), ncol = d)

      dp <- DirichletProcessMvnormal(test_data)
      dp <- Fit(dp, its = 10)

      max_dim <- d
      cat(" SUCCESS\n")
      TRUE
    }, error = function(e) {
      cat(" FAILED\n")
      FALSE
    })

    if (!result) break
  }

  results$max_dimensions <- max_dim

  return(results)
}

# Run comprehensive stress test suite
run_comprehensive_stress_tests <- function() {
  all_results <- list()

  cat("\n========================================\n")
  cat("  COMPREHENSIVE STRESS TEST SUITE\n")
  cat("========================================\n")

  # Basic stress tests
  all_results$basic <- run_stress_tests()

  # Extended stress tests
  all_results$extended <- run_extended_stress_tests()

  # Robustness tests
  all_results$robustness <- test_robustness()

  # Extreme performance tests
  all_results$extreme <- test_extreme_performance()

  # Generate stress test report
  generate_stress_report(all_results)

  return(all_results)
}

generate_stress_report <- function(results) {
  cat("\n\n=== STRESS TEST REPORT ===\n")
  cat("Generated:", format(Sys.time()), "\n\n")

  # Basic stress tests
  cat("Basic Stress Tests:\n")
  for (test in names(results$basic)) {
    cat("  ", test, ":", results$basic[[test]], "\n")
  }

  # Extended tests summary
  cat("\nExtended Tests:\n")
  if (!is.null(results$extended$long_run$runtime)) {
    cat("  Long run (10k iterations):",
        round(results$extended$long_run$runtime, 1), "seconds\n")
  }

  # Robustness summary
  cat("\nRobustness:\n")
  if (!is.null(results$robustness$repeated_fitting)) {
    cat("  Repeated fitting stable:",
        results$robustness$repeated_fitting$all_identical, "\n")
  }

  # Extreme performance
  cat("\nExtreme Performance:\n")
  if (!is.null(results$extreme$max_dataset_size)) {
    cat("  Max dataset size:", results$extreme$max_dataset_size, "\n")
  }
  if (!is.null(results$extreme$max_dimensions)) {
    cat("  Max dimensions:", results$extreme$max_dimensions, "\n")
  }

  # Save detailed results
  saveRDS(results, "stress_test_results.rds")
  cat("\nDetailed results saved to: stress_test_results.rds\n")
}
