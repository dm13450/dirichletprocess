# Dirichlet Process C++ Testing Framework

This directory contains the comprehensive testing framework for validating the C++ implementation of the dirichletprocess package.

## Overview

The testing framework is designed to ensure:
1. **Statistical Equivalence**: R and C++ implementations produce identical results
2. **Performance Improvement**: C++ provides significant speedup over R
3. **Robustness**: Implementation handles edge cases and stress scenarios
4. **Production Readiness**: Package meets quality standards for release

## Quick Start

```r
# Run quick tests (5-10 minutes)
source("tests/run_tests.R")
run_dp_tests("quick")

# Run specific test suite
run_dp_tests("consistency")  # Test R/C++ consistency
run_dp_tests("performance")  # Benchmark performance
run_dp_tests("memory")       # Test memory usage

# Run full validation (3-6 hours)
run_dp_tests("full")
```

## Test Structure

```
tests/
├── testthat/                    # Unit tests
│   ├── helper-testing.R         # Test utilities
│   ├── test-cpp-consistency.R   # R/C++ consistency tests
│   ├── test-cpp-consistency-distributions.R
│   ├── test-cpp-edge-cases.R    # Edge case tests
│   ├── test-cpp-convergence.R   # Convergence tests
│   └── test-cpp-manual-mcmc.R   # Manual MCMC tests
├── integration/                 # Integration tests
│   ├── package_checks.R         # Package-level checks
│   ├── memory_tests.R           # Memory profiling
│   └── stress_tests.R           # Stress testing
├── run_tests.R                  # Main test runner
└── README.md                    # This file

benchmark/                       # Performance benchmarking
├── comprehensive_performance_tests.R
├── manual_mcmc_performance.R
└── visualize_performance.R

inst/validation/                 # Full validation suite
├── run_all_validations.R        # Master validation script
└── validation_report.Rmd        # Report template
```

## Test Suites

### 1. Quick Tests (5-10 minutes)
Basic functionality validation for rapid feedback during development.

```r
run_dp_tests("quick")
```

**Tests included:**
- Basic R/C++ consistency (Normal distribution only)
- Quick performance benchmark
- Manual MCMC interface basics
- Simple edge cases

### 2. Consistency Tests (10-20 minutes)
Validates statistical equivalence between R and C++ implementations.

```r
run_dp_tests("consistency")
```

**Tests included:**
- All 6 distributions (Normal, Exponential, Beta, Weibull, MVNormal, MVNormal2)
- Multiple sample sizes
- Different iteration counts
- Parameter estimation accuracy

**Key metrics:**
- Alpha parameter difference < 0.05
- Cluster count difference < 0.1
- Likelihood correlation > 0.95

### 3. Performance Tests (30-60 minutes)
Comprehensive performance benchmarking and scaling analysis.

```r
run_dp_tests("performance")
```

**Tests included:**
- Speedup measurements for all distributions
- Scaling analysis (100 to 10,000 samples)
- Memory profiling
- Manual MCMC vs Fit() comparison

**Expected results:**
- Average speedup > 2x
- Linear or better scaling
- Memory reduction > 30%

### 4. Integration Tests (20-30 minutes)
Package-level integration and compatibility checks.

```r
run_dp_tests("integration")
```

**Tests included:**
- R CMD check compliance
- C++ compilation warnings
- Dependency verification
- Example execution
- Documentation completeness

### 5. Memory Tests (15-20 minutes)
Memory usage profiling and leak detection.

```r
run_dp_tests("memory")
```

**Tests included:**
- Memory stability over extended runs
- R vs C++ memory comparison
- Large dataset memory scaling
- Manual MCMC memory leaks

### 6. Stress Tests (30-45 minutes)
Robustness testing under extreme conditions.

```r
run_dp_tests("stress")
```

**Tests included:**
- Large datasets (up to 100,000 points)
- High dimensions (up to 100D)
- Extreme parameter values
- Concurrent execution
- Numerical precision limits

### 7. Manual MCMC Tests (10-15 minutes)
Tests for the CppMCMCRunner interface.

```r
run_dp_tests("manual")
```

**Tests included:**
- Step function consistency
- Advanced features (temperature, predictive sampling)
- All covariance models
- Performance benchmarks

### 8. Full Validation (3-6 hours)
Complete validation suite for release preparation.

```r
run_dp_tests("full")
```

**Includes all above tests plus:**
- Extended convergence analysis
- Publication-quality benchmarks
- Comprehensive report generation

## Interpreting Results

### Console Output
```
================================================
  Test Summary - consistency
================================================
normal:              ✅ PASS (5 passed, 0 failed)
exponential:         ✅ PASS (5 passed, 0 failed)
beta:                ✅ PASS (5 passed, 0 failed)
weibull:             ✅ PASS (5 passed, 0 failed)
mvnormal:            ✅ PASS (5 passed, 0 failed)
mvnormal2:           ✅ PASS (5 passed, 0 failed)

Total: 30 passed, 0 failed

Overall Status: ✅ ALL TESTS PASSED
```

### Saved Results
Results are automatically saved to `test_results/` with timestamps:
```r
# Load previous results
results <- readRDS("test_results/consistency_20240719_143022.rds")

# Examine specific metrics
results$normal$alpha_mean_diff      # Should be < 0.05
results$normal$speedup_factor       # Should be > 2
```

### Performance Visualizations
Performance tests generate plots in the working directory:
- `performance_speedup_by_size.png`
- `performance_speedup_by_distribution.png`
- `performance_scaling.png`
- `performance_speedup_scaling.png`

## Continuous Integration

The test framework integrates with GitHub Actions:

```yaml
# Run on every push
name: C++ Validation
on: [push, pull_request]

# Quick tests run automatically
# Full validation runs weekly or on demand
```

## Troubleshooting

### Common Issues

1. **C++ not available**
```r
set_use_cpp(TRUE)
get_cpp_status()  # Check what's available
```

2. **Test failures**
```r
# Run with verbose output
run_dp_tests("quick", verbose = TRUE)

# Check specific distribution
test_data <- generate_test_data("normal", 100)
validate_r_cpp_consistency("normal", test_data)
```

3. **Memory issues**
```r
# Reduce test size
options(dirichletprocess.test.size = "small")
run_dp_tests("memory")
```

## Development Workflow

### 1. During Development
```r
# After making changes
run_dp_tests("quick")  # Quick validation

# Before committing
run_dp_tests("consistency")  # Ensure equivalence
```

### 2. Before Pull Request
```r
# Run comprehensive tests
run_dp_tests("performance")
run_dp_tests("integration")
```

### 3. Release Preparation
```r
# Full validation
run_dp_tests("full")

# Generate report
rmarkdown::render("inst/validation/validation_report.Rmd")
```

## Adding New Tests

### 1. Add to appropriate test file
```r
# tests/testthat/test-cpp-new-feature.R
test_that("New feature works correctly", {
  # Test implementation
  expect_equal(result_r, result_cpp)
})
```

### 2. Update test runner
```r
# In run_tests.R, add to appropriate suite
run_new_feature_tests <- function(verbose = TRUE) {
  # Run new tests
}
```

### 3. Document expected behavior
```r
# Add to this README
# Document new test suite and expected results
```

## Success Criteria

The package is considered ready for production when:

- [x] All consistency tests pass (differences within tolerance)
- [x] Performance shows >2x average speedup
- [x] Memory usage reduced by >30%
- [x] All stress tests complete without errors
- [ ] R CMD check: 0 errors, 0 warnings, 0 notes
- [x] Test coverage >80%
- [x] CI passes on all platforms

## Contact

For questions about the testing framework:
- Check existing test implementations
- Review test output carefully
- Submit issues with full test logs

---

*Testing framework version: 1.0.0*  
*Last updated: 2024-07-19*
