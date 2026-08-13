context("Fit sample storage")

test_that("default ordinary Fit retains all samples and records stored iterations", {
  old_cpp_setting <- using_cpp()
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)
  set_use_cpp(FALSE)

  set.seed(101)
  dp <- DirichletProcessGaussian(rnorm(8))
  dp <- Fit(dp, 4, FALSE, FALSE)

  expect_length(dp$alphaChain, 4)
  expect_length(dp$likelihoodChain, 4)
  expect_length(dp$weightsChain, 4)
  expect_length(dp$clusterParametersChain, 4)
  expect_length(dp$priorParametersChain, 4)
  expect_length(dp$labelsChain, 4)
  expect_equal(dp$storedIterations, 1:4)
  expect_identical(dp$totalIterations, 4L)
})

test_that("storeSamples = FALSE suppresses newly retained history for ordinary C++ Fit", {
  old_cpp_setting <- using_cpp()
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)
  set_use_cpp(TRUE)

  set.seed(102)
  dp <- DirichletProcessGaussian(rnorm(8), cpp = TRUE)
  skip_if_not(can_use_cpp(dp), "C++ sampler not available")

  dp <- Fit(dp, 3, FALSE, FALSE, storeSamples = FALSE)

  expect_length(dp$alphaChain, 0)
  expect_length(dp$likelihoodChain, 0)
  expect_length(dp$weightsChain, 0)
  expect_length(dp$clusterParametersChain, 0)
  expect_length(dp$priorParametersChain, 0)
  expect_length(dp$labelsChain, 0)
  expect_equal(dp$storedIterations, integer(0))
  expect_identical(dp$totalIterations, 3L)
  expect_equal(length(dp$clusterLabels), dp$n)
})

test_that("thinning only reduces newly retained samples for ordinary C++ Fit", {
  old_cpp_setting <- using_cpp()
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)
  set_use_cpp(TRUE)

  set.seed(103)
  dp <- DirichletProcessGaussian(rnorm(10), cpp = TRUE)
  skip_if_not(can_use_cpp(dp), "C++ sampler not available")

  dp <- Fit(dp, 5, FALSE, FALSE, thinning = 2)

  expect_length(dp$alphaChain, 3)
  expect_length(dp$labelsChain, 3)
  expect_equal(dp$storedIterations, c(1L, 3L, 5L))
  expect_identical(dp$totalIterations, 5L)
})

test_that("repeated ordinary Fit calls append retained samples without re-thinning old ones", {
  old_cpp_setting <- using_cpp()
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)
  set_use_cpp(TRUE)

  set.seed(104)
  dp <- DirichletProcessGaussian(rnorm(10), cpp = TRUE)
  skip_if_not(can_use_cpp(dp), "C++ sampler not available")

  dp <- Fit(dp, 4, FALSE, FALSE)
  first_alpha_chain <- dp$alphaChain
  first_stored_iterations <- dp$storedIterations

  dp <- Fit(dp, 5, FALSE, FALSE, thinning = 2)

  expect_equal(dp$alphaChain[seq_along(first_alpha_chain)], first_alpha_chain)
  expect_equal(dp$storedIterations[seq_along(first_stored_iterations)],
               first_stored_iterations)
  expect_equal(dp$storedIterations, c(1L, 2L, 3L, 4L, 5L, 7L, 9L))
  expect_identical(dp$totalIterations, 9L)
  expect_length(dp$alphaChain, 7)

  alpha_before_no_store <- dp$alphaChain
  stored_before_no_store <- dp$storedIterations

  dp <- Fit(dp, 2, FALSE, FALSE, storeSamples = FALSE)

  expect_equal(dp$alphaChain, alpha_before_no_store)
  expect_equal(dp$storedIterations, stored_before_no_store)
  expect_identical(dp$totalIterations, 11L)
})
