# inst/validation/run_all_validations.R

# Load required libraries
library(dirichletprocess)
library(testthat)

# Source helper files only (test files will be run via test_file)
source("tests/testthat/helper-testing.R")

run_complete_validation <- function(output_dir = "validation_results") {
  dir.create(output_dir, showWarnings = FALSE)

  cat("\n========================================\n")
  cat("DIRICHLET PROCESS C++ VALIDATION SUITE\n")
  cat("========================================\n")

  validation_results <- list()
  validation_results$start_time <- Sys.time()

  # Phase 1: testthat Tests
  cat("\n--- PHASE 1: TESTTHAT TESTS ---\n")

  # Get all test files in tests/testthat/
  test_files <- list.files("tests/testthat", pattern = "^test.*\\.R$", full.names = TRUE)
  cat("Found", length(test_files), "test files\n")
  
  for (test_file in test_files) {
    test_name <- basename(test_file)
    cat("\n Running", test_name, "...\n")
    
    validation_results[[gsub("test-?|\\.R$", "", test_name)]] <- tryCatch({
      testthat::test_file(test_file)
    }, error = function(e) {
      cat("ERROR in", test_name, ":", e$message, "\n")
      list(error = e$message, file = test_file)
    })
  }

  # Phase 2: Integration Tests
  cat("\n--- PHASE 2: INTEGRATION TESTS ---\n")

  # Check if integration directory exists
  if (dir.exists("tests/integration")) {
    integration_files <- list.files("tests/integration", pattern = "\\.R$", full.names = TRUE)
    cat("Found", length(integration_files), "integration test files\n")
    
    for (integration_file in integration_files) {
      test_name <- basename(integration_file)
      cat("\n Running integration test:", test_name, "...\n")
      
      validation_results[[paste0("integration_", gsub("\\.R$", "", test_name))]] <- tryCatch({
        source(integration_file)
        list(status = "completed", file = integration_file)
      }, error = function(e) {
        cat("ERROR in", test_name, ":", e$message, "\n")
        list(error = e$message, file = integration_file)
      })
    }
  } else {
    cat("No integration tests directory found (tests/integration)\n")
    validation_results$integration_tests <- list(status = "no_directory")
  }

  # End time
  validation_results$end_time <- Sys.time()
  validation_results$total_runtime <- difftime(
    validation_results$end_time,
    validation_results$start_time,
    units = "hours"
  )

  # Generate reports
  cat("\n--- GENERATING REPORTS ---\n")

  # Save raw results
  saveRDS(validation_results, file.path(output_dir, "validation_results.rds"))

  # Generate validation report
  generate_validation_report(validation_results, output_dir)

  # Print summary
  print_validation_summary(validation_results)

  return(validation_results)
}

generate_validation_report <- function(results, output_dir) {
  # Create markdown report
  report_file <- file.path(output_dir, "validation_report.md")

  # Separate testthat tests from integration tests
  testthat_results <- results[!grepl("^(integration_|start_time|end_time|total_runtime)$", names(results))]
  integration_results <- results[grepl("^integration_", names(results))]
  
  report_content <- sprintf("
# Dirichlet Process Testing Validation Report

**Generated**: %s
**Total Runtime**: %.2f hours

## Executive Summary

### Overall Status: %s

## Phase 1: testthat Tests (%d test files)

%s

## Phase 2: Integration Tests (%d integration files)

%s

## Test Summary

%s

## Recommendations

%s
",
                            format(results$end_time, "%Y-%m-%d %H:%M:%S"),
                            as.numeric(results$total_runtime),
                            determine_overall_status(results),
                            length(testthat_results),
                            format_all_test_results(testthat_results),
                            length(integration_results),
                            format_integration_results(integration_results),
                            generate_test_summary(results),
                            generate_recommendations(results)
  )

  writeLines(report_content, report_file)
  cat("\nValidation report written to:", report_file, "\n")
}

# Helper functions for report generation
determine_overall_status <- function(results) {
  # Check for any errors in testthat tests
  has_errors <- any(sapply(results, function(x) {
    # Skip non-list items that can't have errors
    if (!is.list(x)) return(FALSE)
    
    # Check if this item has an error
    if (!is.null(x$error)) return(TRUE)
    
    # Check nested items for errors
    if (is.list(x) && any(sapply(x, function(y) {
      is.list(y) && !is.null(y$error)
    }))) return(TRUE)
    
    return(FALSE)
  }))

  if (has_errors) {
    return("❌ FAILED - Errors detected")
  }

  # Count test failures
  test_results <- results[!grepl("^(integration_|start_time|end_time|total_runtime)$", names(results))]
  total_failures <- sum(sapply(test_results, function(x) {
    if (inherits(x, "testthat_results")) {
      sum(sapply(x, function(test) length(test$failed) > 0))
    } else {
      0
    }
  }))

  if (total_failures > 0) {
    return(paste("⚠️ WARNING -", total_failures, "test failures detected"))
  }

  return("✅ PASSED - All tests successful")
}

format_all_test_results <- function(test_results) {
  if (length(test_results) == 0) return("No test results available")
  
  lines <- c()
  for (test_name in names(test_results)) {
    result <- test_results[[test_name]]
    
    if (is.list(result) && !is.null(result$error)) {
      lines <- c(lines, sprintf("- **%s**: ❌ ERROR - %s", test_name, result$error))
    } else if (inherits(result, "testthat_results")) {
      passed <- sum(sapply(result, function(x) if(is.null(x$failed) || length(x$failed) == 0) 1 else 0))
      failed <- sum(sapply(result, function(x) if(!is.null(x$failed) && length(x$failed) > 0) 1 else 0))
      warnings <- sum(sapply(result, function(x) if(!is.null(x$warning)) 1 else 0))
      
      status <- if (failed == 0) "✅ PASSED" else "❌ FAILED"
      lines <- c(lines, sprintf("- **%s**: %s - %d passed, %d failed, %d warnings", 
                               test_name, status, passed, failed, warnings))
    } else {
      lines <- c(lines, sprintf("- **%s**: ✅ COMPLETED", test_name))
    }
  }
  
  return(paste(lines, collapse = "\n"))
}

format_integration_results <- function(integration_results) {
  if (length(integration_results) == 0) return("No integration tests found")
  
  lines <- c()
  for (test_name in names(integration_results)) {
    result <- integration_results[[test_name]]
    
    if (is.list(result) && !is.null(result$error)) {
      lines <- c(lines, sprintf("- **%s**: ❌ ERROR - %s", test_name, result$error))
    } else if (is.list(result) && result$status == "completed") {
      lines <- c(lines, sprintf("- **%s**: ✅ COMPLETED", test_name))
    } else {
      lines <- c(lines, sprintf("- **%s**: ⚠️ %s", test_name, result$status))
    }
  }
  
  return(paste(lines, collapse = "\n"))
}

generate_test_summary <- function(results) {
  # Count total tests
  test_results <- results[!grepl("^(integration_|start_time|end_time|total_runtime)$", names(results))]
  integration_results <- results[grepl("^integration_", names(results))]
  
  total_files <- length(test_results) + length(integration_results)
  
  # Count errors
  total_errors <- sum(sapply(c(test_results, integration_results), function(x) {
    is.list(x) && !is.null(x$error)
  }))
  
  # Count test failures (only for testthat tests)
  total_failures <- sum(sapply(test_results, function(x) {
    if (inherits(x, "testthat_results")) {
      sum(sapply(x, function(test) if(!is.null(test$failed) && length(test$failed) > 0) 1 else 0))
    } else {
      0
    }
  }))
  
  summary_lines <- c(
    sprintf("- **Total test files executed**: %d", total_files),
    sprintf("- **testthat files**: %d", length(test_results)),
    sprintf("- **Integration files**: %d", length(integration_results)),
    sprintf("- **Files with errors**: %d", total_errors),
    sprintf("- **Individual test failures**: %d", total_failures),
    sprintf("- **Runtime**: %.2f hours", as.numeric(results$total_runtime))
  )
  
  return(paste(summary_lines, collapse = "\n"))
}


generate_recommendations <- function(results) {
  recommendations <- c()

  # Check for errors
  error_count <- sum(sapply(results, function(x) {
    is.list(x) && !is.null(x$error)
  }))

  if (error_count > 0) {
    recommendations <- c(recommendations,
                         sprintf("- Fix %d errors before proceeding", error_count))
  }

  # Check for test failures
  test_results <- results[!grepl("^(integration_|start_time|end_time|total_runtime)$", names(results))]
  total_failures <- sum(sapply(test_results, function(x) {
    if (inherits(x, "testthat_results")) {
      sum(sapply(x, function(test) if(!is.null(test$failed) && length(test$failed) > 0) 1 else 0))
    } else {
      0
    }
  }))

  if (total_failures > 0) {
    recommendations <- c(recommendations,
                         sprintf("- Address %d test failures", total_failures))
  }

  # Check runtime
  if (as.numeric(results$total_runtime) > 2) {
    recommendations <- c(recommendations,
                         "- Consider running tests in development mode for faster iteration")
  }

  if (length(recommendations) == 0) {
    recommendations <- "- All tests passed successfully - package validation complete"
  }

  return(paste(recommendations, collapse = "\n"))
}

print_validation_summary <- function(results) {
  cat("\n\n========================================\n")
  cat("       VALIDATION SUMMARY\n")
  cat("========================================\n")

  cat("\nTotal runtime:", round(as.numeric(results$total_runtime), 2), "hours\n")
  cat("Overall status:", determine_overall_status(results), "\n")

  # Count test results
  test_results <- results[!grepl("^(integration_|start_time|end_time|total_runtime)$", names(results))]
  integration_results <- results[grepl("^integration_", names(results))]
  
  cat("\nTest Execution Summary:\n")
  cat("  testthat files:", length(test_results), "\n")
  cat("  Integration files:", length(integration_results), "\n")
  
  # Count errors and failures
  error_count <- sum(sapply(c(test_results, integration_results), function(x) is.list(x) && !is.null(x$error)))
  failure_count <- sum(sapply(test_results, function(x) {
    if (inherits(x, "testthat_results")) {
      sum(sapply(x, function(test) if(!is.null(test$failed) && length(test$failed) > 0) 1 else 0))
    } else {
      0
    }
  }))
  
  cat("  Files with errors:", error_count, "\n")
  cat("  Individual test failures:", failure_count, "\n")

  cat("\nReports generated in:", getwd(), "/validation_results/\n")
}

# Quick validation for development  
quick_validation <- function() {
  cat("\n=== QUICK VALIDATION (Development) ===\n")
  cat("Running core consistency tests only...\n\n")

  results <- list()
  results$start_time <- Sys.time()

  # Run just the core consistency tests
  cat("Running test-cpp-consistency.R...\n")
  results$cpp_consistency <- tryCatch({
    testthat::test_file("tests/testthat/test-cpp-consistency.R")
  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
    list(error = e$message)
  })

  results$end_time <- Sys.time()
  results$total_runtime <- difftime(results$end_time, results$start_time, units = "hours")

  cat("\n=== QUICK VALIDATION COMPLETE ===\n")
  cat("Runtime:", round(as.numeric(results$total_runtime) * 60, 1), "minutes\n")
  
  return(results)
}

# Run validation based on command line arguments
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) == 0 || args[1] == "full") {
    results <- run_complete_validation()
  } else if (args[1] == "quick") {
    results <- quick_validation()
  } else {
    cat("Usage: Rscript run_all_validations.R [full|quick]\n")
  }
} else {
  cat("Run validation with:\n")
  cat("  results <- run_complete_validation()  # Full validation\n")
  cat("  results <- quick_validation()         # Quick validation\n")
}
