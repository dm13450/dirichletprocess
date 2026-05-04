# Complete Test Suite for CRAN Submission
# This runs ALL tests in the package using devtools::test()

library(devtools)
library(testthat)

cat("========================================\n")
cat("COMPLETE PACKAGE TEST SUITE FOR CRAN\n")
cat("========================================\n\n")

# Check package can be loaded
cat("1. Loading package... ")
tryCatch({
  library(dirichletprocess)
  cat("✅ SUCCESS\n")
}, error = function(e) {
  cat("❌ FAILED:", e$message, "\n")
  stop("Cannot load package")
})

# Check C++ status
cat("2. C++ Status: ", using_cpp(), "\n")
cat("3. C++ Available: ", can_use_cpp(), "\n\n")

# Run ALL tests using devtools::test()
cat("4. Running ALL tests with devtools::test()...\n")
cat("   (This includes all files in tests/testthat/ and subdirectories)\n\n")

start_time <- Sys.time()

# Run all tests
test_results <- devtools::test()

end_time <- Sys.time()
duration <- round(as.numeric(end_time - start_time), 1)

cat("\n========================================\n")
cat("TEST RESULTS SUMMARY\n")
cat("========================================\n")
cat("Total Duration:", duration, "seconds\n")

# Print the results summary
print(test_results)

# Check if all tests passed
if (all(test_results$failed == 0) && all(test_results$error == FALSE)) {
  cat("\n✅ ALL TESTS PASSED! ✅\n")
  cat("Package is ready for CRAN submission.\n")
} else {
  cat("\n❌ SOME TESTS FAILED ❌\n")
  cat("Package needs fixes before CRAN submission.\n")
  
  # Show failed tests
  failed_tests <- test_results[test_results$failed > 0 | test_results$error == TRUE, ]
  if (nrow(failed_tests) > 0) {
    cat("\nFailed test files:\n")
    print(failed_tests[, c("file", "failed", "error")])
  }
}

cat("\n========================================\n")