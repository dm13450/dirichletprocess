# tests/run_tests.R
# Convenient test runner for the Dirichlet Process C++ implementation

#' Run specific test suites for the dirichletprocess package
#'
#' @param suite Character string specifying which test suite to run
#' @param verbose Logical, whether to print detailed output
#' @param save_results Logical, whether to save results to file
#' @return Test results
run_dp_tests <- function(suite = "quick", verbose = TRUE, save_results = TRUE) {

  # Ensure we're in the package root
  if (!file.exists("DESCRIPTION")) {
    stop("Please run this script from the package root directory")
  }

  # Load required library
  library(dirichletprocess)

  # Load required functions
  source("tests/testthat/helper-testing.R")

  # Create results directory
  if (save_results) {
    dir.create("test_results", showWarnings = FALSE)
    results_file <- file.path("test_results",
                              paste0(suite, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))
  }

  cat("\n================================================\n")
  cat("  Dirichlet Process C++ Test Runner\n")
  cat("================================================\n")
  cat("Test suite:", suite, "\n")
  cat("Start time:", format(Sys.time()), "\n\n")

  start_time <- Sys.time()

  results <- switch(suite,
                    # Quick tests (5-10 minutes)
                    "quick" = run_quick_tests(verbose),

                    # Consistency tests only (10-20 minutes)
                    "consistency" = run_consistency_suite(verbose),

                    # Performance tests only (30-60 minutes)
                    "performance" = run_performance_suite(verbose),

                    # Integration tests (20-30 minutes)
                    "integration" = run_integration_suite(verbose),

                    # Memory tests (15-20 minutes)
                    "memory" = run_memory_suite(verbose),

                    # Stress tests (30-45 minutes)
                    "stress" = run_stress_suite(verbose),

                    # Manual MCMC tests (10-15 minutes)
                    "manual" = run_manual_mcmc_suite(verbose),

                    # Full validation (3-6 hours)
                    "full" = run_full_validation(verbose),

                    # Default
                    stop("Unknown test suite. Choose from: quick, consistency, performance, ",
                         "integration, memory, stress, manual, full")
  )

  end_time <- Sys.time()
  runtime <- difftime(end_time, start_time, units = "mins")

  cat("\n\nTest suite completed in", round(runtime, 2), "minutes\n")

  # Save results
  if (save_results) {
    saveRDS(results, results_file)
    cat("Results saved to:", results_file, "\n")
  }

  # Print summary
  print_test_summary(results, suite)

  invisible(results)
}

# Quick test suite
run_quick_tests <- function(verbose = TRUE) {
  if (verbose) cat("Running quick test suite...\n\n")

  results <- list()

  # 1. Basic functionality
  if (verbose) cat("1. Testing basic functionality...\n")
  results$basic <- tryCatch({
    source("tests/testthat/test-cpp-consistency.R")
    test_that("Consistency validation framework works", {
      test_data <- rnorm(50)
      res <- validate_r_cpp_consistency("normal", test_data, iterations = 10, n_runs = 2)
      expect_type(res, "list")
    })
    "PASSED"
  }, error = function(e) e$message)

  # 2. Quick consistency check
  if (verbose) cat("2. Quick consistency check (Normal only)...\n")
  results$consistency <- tryCatch({
    test_data <- generate_test_data("normal", 100)
    validate_r_cpp_consistency("normal", test_data, iterations = 50, n_runs = 3)
  }, error = function(e) list(error = e$message))

  # 3. Quick performance
  if (verbose) cat("3. Quick performance benchmark...\n")
  results$performance <- tryCatch({
    source("benchmark/comprehensive_performance_tests.R")
    quick_benchmark("normal", n = 500, its = 50)
  }, error = function(e) list(error = e$message))

  # 4. Manual MCMC basic test
  if (verbose) cat("4. Testing manual MCMC interface...\n")
  results$manual_mcmc <- tryCatch({
    dp <- DirichletProcessGaussian(rnorm(100))
    runner <- CppMCMCRunner$new(dp)
    for (i in 1:10) {
      runner$step_assignments()
      runner$step_parameters()
      runner$step_concentration()
    }
    "PASSED"
  }, error = function(e) e$message)

  # 5. Edge case sampling
  if (verbose) cat("5. Testing edge cases...\n")
  results$edge_cases <- tryCatch({
    # Single point
    dp1 <- DirichletProcessGaussian(1.5)
    dp1 <- Fit(dp1, its = 5)

    # Extreme values
    dp2 <- DirichletProcessGaussian(c(rnorm(50), 1e6, -1e6))
    dp2 <- Fit(dp2, its = 5)

    "PASSED"
  }, error = function(e) e$message)

  return(results)
}

# Consistency test suite
run_consistency_suite <- function(verbose = TRUE) {
  if (verbose) cat("Running consistency test suite...\n\n")

  source("tests/testthat/test-cpp-consistency.R")
  source("tests/testthat/test-cpp-consistency-distributions.R")

  results <- list()
  distributions <- c("normal", "exponential", "beta", "weibull", "mvnormal", "mvnormal2")

  for (dist in distributions) {
    if (verbose) cat("Testing", dist, "distribution...\n")

    test_data <- generate_test_data(dist, n = 200)
    results[[dist]] <- validate_r_cpp_consistency(dist, test_data,
                                                  iterations = 200, n_runs = 5)
  }

  # Run formal tests
  if (verbose) cat("\nRunning formal consistency tests...\n")
  results$formal_tests <- testthat::test_file("tests/testthat/test-cpp-consistency-distributions.R")

  return(results)
}

# Performance test suite
run_performance_suite <- function(verbose = TRUE) {
  if (verbose) cat("Running performance test suite...\n\n")

  source("benchmark/comprehensive_performance_tests.R")
  source("benchmark/manual_mcmc_performance.R")

  results <- list()

  # 1. Quick benchmarks for all distributions
  if (verbose) cat("1. Quick benchmarks...\n")
  distributions <- c("normal", "exponential", "beta", "weibull", "mvnormal")

  for (dist in distributions) {
    if (verbose) cat("  ", dist, "...")
    results$quick[[dist]] <- quick_benchmark(dist, n = 1000, its = 100)
    if (verbose) cat(" done\n")
  }

  # 2. Scaling analysis
  if (verbose) cat("\n2. Scaling analysis...\n")
  results$scaling <- test_scaling_behavior()

  # 3. Manual MCMC performance
  if (verbose) cat("\n3. Manual MCMC performance...\n")
  results$manual_mcmc <- benchmark_manual_mcmc()
  results$manual_vs_fit <- benchmark_manual_vs_fit()

  # 4. Memory profiling
  if (verbose) cat("\n4. Memory profiling...\n")
  results$memory <- profile_memory_usage()

  return(results)
}

# Integration test suite
run_integration_suite <- function(verbose = TRUE) {
  if (verbose) cat("Running integration test suite...\n\n")

  source("inst/integration/package_checks.R")

  results <- list()

  # 1. C++ availability check
  if (verbose) cat("1. Checking C++ availability...\n")
  results$cpp_availability <- check_cpp_availability()

  # 2. Compilation check
  if (verbose) cat("\n2. Checking C++ compilation...\n")
  results$compilation <- check_cpp_compilation()

  # 3. Namespace check
  if (verbose) cat("\n3. Checking NAMESPACE...\n")
  results$namespace <- check_namespace()

  # 4. Dependencies
  if (verbose) cat("\n4. Checking dependencies...\n")
  results$dependencies <- check_dependencies()

  # 5. Run examples
  if (verbose) cat("\n5. Running examples...\n")
  results$examples <- tryCatch({
    devtools::run_examples()
    "PASSED"
  }, error = function(e) e$message)

  return(results)
}

# Memory test suite
run_memory_suite <- function(verbose = TRUE) {
  if (verbose) cat("Running memory test suite...\n\n")

  source("inst/integration/memory_tests.R")

  results <- list()

  # 1. Memory stability
  if (verbose) cat("1. Testing memory stability...\n")
  results$stability <- tryCatch(
    test_memory_stability(),
    error = function(e) list(error = e$message)
  )

  # 2. R vs C++ comparison
  if (verbose) cat("\n2. Comparing R vs C++ memory usage...\n")
  results$comparison <- compare_r_cpp_memory()

  # 3. Large dataset memory
  if (verbose) cat("\n3. Testing large dataset memory usage...\n")
  results$large_datasets <- test_large_dataset_memory()

  # 4. Manual MCMC memory
  if (verbose) cat("\n4. Testing manual MCMC memory leaks...\n")
  results$manual_mcmc <- tryCatch(
    test_manual_mcmc_memory_leaks(),
    error = function(e) list(error = e$message)
  )

  return(results)
}

# Stress test suite
run_stress_suite <- function(verbose = TRUE) {
  if (verbose) cat("Running stress test suite...\n\n")

  source("inst/integration/stress_tests.R")

  # Run comprehensive stress tests
  results <- run_comprehensive_stress_tests()

  return(results)
}

# Manual MCMC test suite
run_manual_mcmc_suite <- function(verbose = TRUE) {
  if (verbose) cat("Running manual MCMC test suite...\n\n")

  results <- list()

  # Run manual MCMC tests
  results$tests <- testthat::test_file("tests/testthat/test-cpp-manual-mcmc.R")

  # Performance benchmarks
  source("benchmark/manual_mcmc_performance.R")
  results$performance <- benchmark_manual_mcmc()
  results$vs_fit <- benchmark_manual_vs_fit()
  results$advanced <- benchmark_advanced_features()

  return(results)
}

# Full validation suite
run_full_validation <- function(verbose = TRUE) {
  if (verbose) cat("Running FULL validation suite (this will take 3-6 hours)...\n\n")

  source("inst/validation/run_all_validations.R")
  results <- run_complete_validation()

  return(results)
}

# Print test summary
print_test_summary <- function(results, suite) {
  cat("\n================================================\n")
  cat("  Test Summary -", suite, "\n")
  cat("================================================\n")

  # Count passes and failures
  count_results <- function(res) {
    if (is.null(res)) return(list(pass = 0, fail = 0))

    passes <- failures <- 0

    if (is.character(res) && res == "PASSED") {
      passes <- 1
    } else if (is.character(res) && res != "PASSED") {
      failures <- 1
    } else if (is.list(res)) {
      for (item in res) {
        if (!is.null(item$error) ||
            (is.character(item) && item != "PASSED")) {
          failures <- failures + 1
        } else {
          passes <- passes + 1
        }
      }
    }

    list(pass = passes, fail = failures)
  }

  total_pass <- 0
  total_fail <- 0

  for (component in names(results)) {
    counts <- count_results(results[[component]])
    total_pass <- total_pass + counts$pass
    total_fail <- total_fail + counts$fail

    status <- if (counts$fail > 0) "❌ FAIL" else "✅ PASS"
    cat(sprintf("%-20s %s (%d passed, %d failed)\n",
                paste0(component, ":"), status, counts$pass, counts$fail))
  }

  cat("\n")
  cat("Total: ", total_pass, " passed, ", total_fail, " failed\n", sep = "")

  overall_status <- if (total_fail == 0) {
    "✅ ALL TESTS PASSED"
  } else {
    "❌ SOME TESTS FAILED"
  }

  cat("\nOverall Status: ", overall_status, "\n", sep = "")
}

# Interactive menu
if (interactive()) {
  cat("Dirichlet Process C++ Test Runner\n")
  cat("=================================\n\n")
  cat("Available test suites:\n")
  cat("1. quick       - Quick tests (5-10 minutes)\n")
  cat("2. consistency - R/C++ consistency tests (10-20 minutes)\n")
  cat("3. performance - Performance benchmarks (30-60 minutes)\n")
  cat("4. integration - Integration tests (20-30 minutes)\n")
  cat("5. memory      - Memory tests (15-20 minutes)\n")
  cat("6. stress      - Stress tests (30-45 minutes)\n")
  cat("7. manual      - Manual MCMC tests (10-15 minutes)\n")
  cat("8. full        - Full validation (3-6 hours)\n")
  cat("\nRun with: run_dp_tests('suite_name')\n")
  cat("Example: run_dp_tests('quick')\n")
}
