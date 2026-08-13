# tests/integration/memory_tests.R

# Load required library
library(dirichletprocess)

# Development/Production Mode Configuration
# Set DP_DEV_TESTING=TRUE for development mode (faster, smaller tests)
# Set DP_DEV_TESTING=FALSE for production mode (full validation)
is_dev_mode <- function() {
  dev_env <- Sys.getenv("DP_DEV_TESTING", unset = "TRUE")
  return(tolower(dev_env) %in% c("true", "1", "yes"))
}

get_test_params <- function() {
  if (is_dev_mode()) {
    list(
      memory_iterations = 5,        # vs 10 in production
      memory_mcmc_its = 50,         # vs 100 in production
      profile_sample_size = 200,    # vs 500 in production
      profile_mcmc_its = 25,        # vs 50 in production
      manual_mcmc_its = 500,        # vs 1000 in production
      compare_sample_size = 1000,   # vs 2000 in production
      compare_mcmc_its = 50,        # vs 100 in production
      large_dataset_sizes = c(1000, 2000, 5000),  # vs c(1000, 5000, 10000, 20000)
      large_dataset_its = 10        # vs 20 in production
    )
  } else {
    list(
      memory_iterations = 10,
      memory_mcmc_its = 100,
      profile_sample_size = 500,
      profile_mcmc_its = 50,
      manual_mcmc_its = 1000,
      compare_sample_size = 2000,
      compare_mcmc_its = 100,
      large_dataset_sizes = c(1000, 5000, 10000, 20000),
      large_dataset_its = 20
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

test_memory_stability <- function() {
  params <- get_test_params()
  mode_info <- if (is_dev_mode()) "DEV" else "PROD"
  
  cat("\nTesting memory stability [", mode_info, " MODE]...\n")
  cat("  Iterations:", params$memory_iterations, "| MCMC its:", params$memory_mcmc_its, "\n")

  # Run extended MCMC to check for leaks
  test_data <- rnorm(1000)

  # Get baseline memory
  gc()
  baseline_mem <- gc()[2, 2]  # Max memory used

  # Run many iterations
  for (i in 1:params$memory_iterations) {
    dp <- DirichletProcessGaussian(test_data)
    dp <- Fit(dp, its = params$memory_mcmc_its)
    rm(dp)
    gc()
  }

  # Check final memory
  final_mem <- gc()[2, 2]
  memory_increase <- final_mem - baseline_mem

  cat("Memory increase:", round(memory_increase, 2), "MB\n")

  # Should be minimal increase (< 10MB)
  if (memory_increase < 10) {
    cat("✅ PASS: Memory stable\n")
  } else {
    cat("❌ FAIL: Memory increase too high\n")
  }
  
  return(memory_increase)
}

# Detailed memory profiling
profile_memory_by_distribution <- function() {
  if (!requireNamespace("profmem", quietly = TRUE)) {
    warning("profmem package not installed")
    return(NULL)
  }

  library(profmem)
  params <- get_test_params()
  mode_info <- if (is_dev_mode()) "DEV" else "PROD"
  
  cat("\nProfiling memory by distribution [", mode_info, " MODE]...\n")
  cat("  Sample size:", params$profile_sample_size, "| MCMC its:", params$profile_mcmc_its, "\n")

  distributions <- c("normal", "exponential", "beta", "weibull", "mvnormal")
  memory_results <- list()

  for (dist in distributions) {
    cat("\nProfiling", dist, "distribution...\n")

    test_data <- generate_test_data(dist, n = params$profile_sample_size)

    # Profile object creation
    creation_prof <- profmem({
      dp <- create_dp_object(dist, test_data)
    })

    dp <- create_dp_object(dist, test_data)

    # Profile fitting
    fitting_prof <- profmem({
      dp <- Fit(dp, its = params$profile_mcmc_its)
    })

    # Profile prediction
    prediction_prof <- profmem({
      samples <- PosteriorDraw(dp, 100)
    })

    memory_results[[dist]] <- list(
      creation = total(creation_prof),
      fitting = total(fitting_prof),
      prediction = total(prediction_prof),
      total = total(creation_prof) + total(fitting_prof) + total(prediction_prof)
    )
  }

  # Create memory comparison
  mem_df <- do.call(rbind, lapply(names(memory_results), function(dist) {
    data.frame(
      distribution = dist,
      creation_mb = memory_results[[dist]]$creation / 1e6,
      fitting_mb = memory_results[[dist]]$fitting / 1e6,
      prediction_mb = memory_results[[dist]]$prediction / 1e6,
      total_mb = memory_results[[dist]]$total / 1e6
    )
  }))

  print(mem_df)

  return(memory_results)
}

# Test for memory leaks in manual MCMC
test_manual_mcmc_memory_leaks <- function() {
  params <- get_test_params()
  mode_info <- if (is_dev_mode()) "DEV" else "PROD"
  
  cat("\nTesting manual MCMC for memory leaks [", mode_info, " MODE]...\n")
  cat("  Iterations:", params$manual_mcmc_its, "\n")

  test_data <- generate_test_data("normal", 1000)
  dp <- DirichletProcessGaussian(test_data)
  
  # Check if CppMCMCRunner is available
  if (!using_cpp() || !exists("CppMCMCRunner")) {
    cat("⚠️  CppMCMCRunner not available, skipping manual MCMC test\n")
    return(list(status = "SKIPPED", reason = "CppMCMCRunner not available"))
  }
  
  runner <- CppMCMCRunner$new(dp)

  # Baseline memory
  gc()
  baseline <- gc()[2, 2]

  # Run many iterations
  check_interval <- max(100, params$manual_mcmc_its %/% 5)
  for (i in 1:params$manual_mcmc_its) {
    runner$step_assignments()
    runner$step_parameters()
    runner$step_concentration()

    if (i %% check_interval == 0) {
      gc()
      current_mem <- gc()[2, 2]
      cat("  Iteration", i, "- Memory:", round(current_mem, 2), "MB\n")
    }
  }

  # Final memory check
  gc()
  final_mem <- gc()[2, 2]
  memory_growth <- final_mem - baseline

  cat("Total memory growth:", round(memory_growth, 2), "MB\n")

  # Should have minimal growth
  if (memory_growth < 5) {
    cat("✅ PASS: Manual MCMC memory stable\n")
  } else {
    cat("❌ FAIL: Manual MCMC memory growth too high\n")
  }
  
  return(memory_growth)
}

# Compare R vs C++ memory usage
compare_r_cpp_memory <- function() {
  params <- get_test_params()
  mode_info <- if (is_dev_mode()) "DEV" else "PROD"
  
  cat("\nComparing R vs C++ memory usage [", mode_info, " MODE]...\n")
  cat("  Sample size:", params$compare_sample_size, "| MCMC its:", params$compare_mcmc_its, "\n")

  distributions <- c("normal", "exponential", "mvnormal")
  comparison_results <- list()

  for (dist in distributions) {
    cat("\n", dist, "distribution:\n")

    test_data <- generate_test_data(dist, n = params$compare_sample_size)

    # R implementation memory
    gc()
    r_baseline <- gc()[2, 2]

    set_use_cpp(FALSE)
    dp_r <- create_dp_object(dist, test_data)
    dp_r <- Fit(dp_r, its = params$compare_mcmc_its)

    gc()
    r_peak <- gc()[2, 2]
    r_usage <- r_peak - r_baseline

    rm(dp_r)
    gc()

    # C++ implementation memory
    gc()
    cpp_baseline <- gc()[2, 2]

    set_use_cpp(TRUE)
    dp_cpp <- create_dp_object(dist, test_data)
    dp_cpp <- Fit(dp_cpp, its = params$compare_mcmc_its)

    gc()
    cpp_peak <- gc()[2, 2]
    cpp_usage <- cpp_peak - cpp_baseline

    comparison_results[[dist]] <- list(
      r_memory_mb = r_usage,
      cpp_memory_mb = cpp_usage,
      reduction_percent = (r_usage - cpp_usage) / r_usage * 100
    )

    cat("  R memory:", round(r_usage, 2), "MB\n")
    cat("  C++ memory:", round(cpp_usage, 2), "MB\n")
    cat("  Reduction:", round(comparison_results[[dist]]$reduction_percent, 1), "%\n")
  }

  return(comparison_results)
}

# Test memory usage with large datasets
test_large_dataset_memory <- function() {
  params <- get_test_params()
  mode_info <- if (is_dev_mode()) "DEV" else "PROD"
  
  cat("\nTesting memory usage with large datasets [", mode_info, " MODE]...\n")
  cat("  Sample sizes:", paste(params$large_dataset_sizes, collapse = ", "), "| MCMC its:", params$large_dataset_its, "\n")

  memory_usage <- list()

  for (n in params$large_dataset_sizes) {
    cat("\nSample size:", n, "\n")

    # Generate data
    test_data <- rnorm(n)

    # Memory before
    gc()
    mem_before <- gc()[2, 2]

    # Fit model
    dp <- DirichletProcessGaussian(test_data)
    dp <- Fit(dp, its = params$large_dataset_its)

    # Memory after
    gc()
    mem_after <- gc()[2, 2]

    memory_usage[[as.character(n)]] <- list(
      sample_size = n,
      memory_used = mem_after - mem_before,
      memory_per_obs = (mem_after - mem_before) / n * 1000  # KB per observation
    )

    cat("  Memory used:", round(memory_usage[[as.character(n)]]$memory_used, 2), "MB\n")
    cat("  Memory per observation:",
        round(memory_usage[[as.character(n)]]$memory_per_obs, 2), "KB\n")

    rm(dp, test_data)
    gc()
  }

  # Check if memory scales linearly
  sizes <- sapply(memory_usage, `[[`, "sample_size")
  mems <- sapply(memory_usage, `[[`, "memory_used")

  # Fit linear model
  mem_model <- lm(mems ~ sizes)
  r_squared <- summary(mem_model)$r.squared

  cat("\nMemory scaling R-squared:", round(r_squared, 3), "\n")
  cat("Memory scaling is", ifelse(r_squared > 0.95, "linear ✅", "non-linear ⚠️"), "\n")

  return(memory_usage)
}

# Run all memory tests
run_all_memory_tests <- function() {
  results <- list()

  results$stability <- tryCatch(
    test_memory_stability(),
    error = function(e) list(error = e$message)
  )

  results$distribution_profiles <- profile_memory_by_distribution()

  results$manual_mcmc_leaks <- tryCatch(
    test_manual_mcmc_memory_leaks(),
    error = function(e) list(error = e$message)
  )

  results$r_cpp_comparison <- compare_r_cpp_memory()

  results$large_datasets <- test_large_dataset_memory()

  # Generate memory report
  generate_memory_report(results)

  return(results)
}

generate_memory_report <- function(results) {
  cat("\n\n=== MEMORY TEST REPORT ===\n")
  cat("Generated:", format(Sys.time()), "\n\n")

  # R vs C++ comparison
  if (!is.null(results$r_cpp_comparison)) {
    cat("Memory Efficiency (R vs C++):\n")
    for (dist in names(results$r_cpp_comparison)) {
      res <- results$r_cpp_comparison[[dist]]
      cat("  ", dist, ": ",
          round(res$reduction_percent, 1), "% reduction\n")
    }
  }

  # Large dataset scaling
  if (!is.null(results$large_datasets)) {
    cat("\nLarge Dataset Memory Scaling:\n")
    for (n in names(results$large_datasets)) {
      res <- results$large_datasets[[n]]
      cat("  n =", res$sample_size, ":",
          round(res$memory_per_obs, 2), "KB per observation\n")
    }
  }

  saveRDS(results, "memory_test_results.rds")
  cat("\nDetailed results saved to: memory_test_results.rds\n")
}
