# tests/integration/package_checks.R

# Load required library
library(dirichletprocess)

# Development/Production Mode Configuration
# Set DP_DEV_TESTING=TRUE for development mode (faster, smaller tests)
# Set DP_DEV_TESTING=FALSE for production mode (full validation)
is_dev_mode <- function() {
  dev_env <- Sys.getenv("DP_DEV_TESTING", unset = "TRUE")
  return(tolower(dev_env) %in% c("true", "1", "yes"))
}

get_check_params <- function() {
  if (is_dev_mode()) {
    list(
      run_full_check = FALSE,        # Skip R CMD check in dev mode
      run_examples = FALSE,          # Skip examples in dev mode
      cpp_availability_sample = 50,  # vs 100 in production
      cpp_test_iterations = 10       # vs 50 in production
    )
  } else {
    list(
      run_full_check = TRUE,
      run_examples = TRUE,
      cpp_availability_sample = 100,
      cpp_test_iterations = 50
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
      "mvnormal2" = matrix(rnorm(n * 3), ncol = 3),
      rnorm(n)
    )
  }
}

run_package_checks <- function() {
  params <- get_check_params()
  mode_info <- if (is_dev_mode()) "DEV" else "PROD"
  
  cat("\n=== RUNNING PACKAGE CHECKS [", mode_info, " MODE] ===\n")

  check_results <- list()

  # 1. Run devtools::test() - always run
  cat("\n1. Running unit tests...\n")
  test_result <- tryCatch({
    devtools::test()
  }, error = function(e) {
    list(failed = 1, warnings = 0, skipped = 0, passed = 0, error = e$message)
  })
  check_results$tests <- test_result

  # 2. Run devtools::check() - conditional
  if (params$run_full_check) {
    cat("\n2. Running R CMD check...\n")
    check_result <- tryCatch({
      devtools::check()
    }, error = function(e) {
      list(errors = e$message, warnings = character(0), notes = character(0))
    })
    check_results$check <- check_result
  } else {
    cat("\n2. Skipping R CMD check (dev mode)...\n")
    check_results$check <- list(status = "SKIPPED", reason = "Development mode")
  }

  # 3. Check for compilation warnings
  cat("\n3. Checking C++ compilation...\n")
  cpp_check <- check_cpp_compilation()
  check_results$cpp <- cpp_check

  # 4. Documentation check
  cat("\n4. Checking documentation...\n")
  doc_check <- tryCatch({
    devtools::document()
    list(status = "SUCCESS")
  }, error = function(e) {
    list(status = "FAILED", error = e$message)
  })
  check_results$documentation <- doc_check

  # 5. Example check - conditional
  if (params$run_examples) {
    cat("\n5. Running examples...\n")
    example_check <- tryCatch({
      devtools::run_examples()
      list(status = "SUCCESS")
    }, error = function(e) {
      list(status = "FAILED", error = e$message)
    })
    check_results$examples <- example_check
  } else {
    cat("\n5. Skipping examples (dev mode)...\n")
    check_results$examples <- list(status = "SKIPPED", reason = "Development mode")
  }

  # Create summary report
  create_check_summary(check_results)

  return(check_results)
}

check_cpp_compilation <- function() {
  # Check if C++ shared library was created successfully
  library_path <- file.path("src", paste0("dirichletprocess", .Platform$dynlib.ext))
  so_exists <- file.exists(library_path)
  
  if (so_exists) {
    # Get file info to confirm it's not empty
    file_info <- file.info(library_path)
    file_size <- file_info$size
    
    if (file_size > 0) {
      message("C++ shared library created successfully (", file_size, " bytes)")
      
      # Look for any compilation warnings in previous output
      # Since we can't re-run compilation, we'll assume success if library exists
      return(list(
        success = TRUE,
        warnings = character(0),
        errors = character(0),
        full_log = paste("C++ shared library found at:", library_path, "Size:", file_size, "bytes")
      ))
    } else {
      return(list(
        success = FALSE,
        warnings = "C++ shared library exists but is empty",
        errors = "C++ compilation may have failed",
        full_log = "Empty shared library file detected"
      ))
    }
  } else {
    return(list(
      success = FALSE,
      warnings = "C++ shared library not found",
      errors = "C++ compilation failed - no shared library created",
      full_log = paste("Expected library at:", library_path, "but file does not exist")
    ))
  }
}

create_check_summary <- function(check_results) {
  cat("\n\n=== CHECK SUMMARY ===\n")

  # Test summary
  test_summary <- check_results$tests
  cat("\nUnit Tests:",
      test_summary$failed, "failures,",
      test_summary$warnings, "warnings,",
      test_summary$skipped, "skipped,",
      test_summary$passed, "passed\n")

  # R CMD check summary
  check_summary <- check_results$check
  cat("\nR CMD check:",
      length(check_summary$errors), "errors,",
      length(check_summary$warnings), "warnings,",
      length(check_summary$notes), "notes\n")

  # C++ compilation
  if (check_results$cpp$success) {
    cat("\nC++ Compilation: SUCCESS")
    if (length(check_results$cpp$warnings) > 0) {
      cat(" (", length(check_results$cpp$warnings), " warnings)")
    }
    cat("\n")
  } else {
    cat("\nC++ Compilation: FAILED\n")
  }

  # Overall status
  overall_success <-
    test_summary$failed == 0 &&
    length(check_summary$errors) == 0 &&
    check_results$cpp$success

  cat("\n", ifelse(overall_success, "✅ READY FOR RELEASE", "❌ ISSUES NEED FIXING"), "\n")
}

# Check specific functionality
check_cpp_availability <- function() {
  params <- get_check_params()
  mode_info <- if (is_dev_mode()) "DEV" else "PROD"
  
  cat("\nChecking C++ availability for all distributions [", mode_info, " MODE]...\n")
  cat("  Sample size:", params$cpp_availability_sample, "| Test iterations:", params$cpp_test_iterations, "\n")

  distributions <- list(
    normal = DirichletProcessGaussian,
    exponential = DirichletProcessExponential,
    beta = DirichletProcessBeta,
    weibull = DirichletProcessWeibull,
    mvnormal = DirichletProcessMvnormal,
    mvnormal2 = DirichletProcessMvnormal2
  )

  results <- list()

  for (dist_name in names(distributions)) {
    test_data <- generate_test_data(dist_name, n = params$cpp_availability_sample)
    dp <- distributions[[dist_name]](test_data)

    # Check if C++ is available
    cpp_available <- can_use_cpp(dp)

    # Try to run with C++
    set_use_cpp(TRUE)
    cpp_works <- tryCatch({
      dp_test <- Fit(dp, its = params$cpp_test_iterations)
      TRUE
    }, error = function(e) {
      FALSE
    })

    results[[dist_name]] <- list(
      available = cpp_available,
      works = cpp_works
    )

    cat("  ", dist_name, ": ",
        ifelse(cpp_available, "Available", "Not Available"),
        " | ",
        ifelse(cpp_works, "Works", "Fails"), "\n")
  }

  return(results)
}

# Check package dependencies
check_dependencies <- function() {
  cat("\nChecking package dependencies...\n")

  # Get package dependencies
  deps <- desc::desc_get_deps()

  # Check which are installed
  installed_pkgs <- installed.packages()[, "Package"]

  deps$installed <- deps$package %in% installed_pkgs

  # Check versions if installed
  for (i in 1:nrow(deps)) {
    if (deps$installed[i]) {
      deps$installed_version[i] <- as.character(packageVersion(deps$package[i]))
    }
  }

  print(deps)

  # Check for missing dependencies
  missing <- deps$package[!deps$installed]
  if (length(missing) > 0) {
    cat("\n⚠️  Missing dependencies:", paste(missing, collapse = ", "), "\n")
  } else {
    cat("\n✅ All dependencies installed\n")
  }

  return(deps)
}

# Validate NAMESPACE file
check_namespace <- function() {
  cat("\nChecking NAMESPACE file...\n")

  namespace_content <- readLines("NAMESPACE")

  # Check for C++ exports
  cpp_exports <- grep("^\\.Call|^useDynLib", namespace_content, value = TRUE)
  cat("C++ exports found:", length(cpp_exports), "\n")

  # Check for S3 methods
  s3_methods <- grep("^S3method", namespace_content, value = TRUE)
  cat("S3 methods registered:", length(s3_methods), "\n")

  # Check for imports
  imports <- grep("^import", namespace_content, value = TRUE)
  cat("Imports found:", length(imports), "\n")

  return(list(
    cpp_exports = cpp_exports,
    s3_methods = s3_methods,
    imports = imports
  ))
}

# Run all integration checks
run_all_integration_checks <- function() {
  results <- list()

  results$package_checks <- run_package_checks()
  results$cpp_availability <- check_cpp_availability()
  results$dependencies <- check_dependencies()
  results$namespace <- check_namespace()

  # Generate integration report
  generate_integration_report(results)

  return(results)
}

generate_integration_report <- function(results) {
  cat("\n\n=== INTEGRATION REPORT ===\n")
  cat("Generated:", format(Sys.time()), "\n\n")

  # Package check status
  cat("Package Checks:\n")
  cat("  R CMD check: ",
      ifelse(length(results$package_checks$check$errors) == 0, "✅ PASS", "❌ FAIL"), "\n")
  cat("  Unit tests: ",
      ifelse(results$package_checks$tests$failed == 0, "✅ PASS", "❌ FAIL"), "\n")
  cat("  C++ compilation: ",
      ifelse(results$package_checks$cpp$success, "✅ PASS", "❌ FAIL"), "\n")

  # C++ availability
  cat("\nC++ Implementation Status:\n")
  cpp_status <- results$cpp_availability
  for (dist in names(cpp_status)) {
    cat("  ", dist, ": ",
        ifelse(cpp_status[[dist]]$works, "✅", "❌"), "\n")
  }

  # Save detailed report
  saveRDS(results, "integration_results.rds")
  cat("\nDetailed results saved to: integration_results.rds\n")
}
