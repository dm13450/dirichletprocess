context("General ordinary Dirichlet process constructor")

expect_general_dp <- function(dp, kernel, conjugacy) {
  expect_identical(class(dp), c("dirichletprocess", "list"))
  expect_equal(dp$kernel, kernel)
  expect_equal(dp$conjugacy, conjugacy)
  expect_identical(class(dp$mixingDistribution)[1], kernel)
  expect_true(inherits(dp$mixingDistribution, "mixing_distribution"))
  expect_identical(tail(class(dp$mixingDistribution), 1), "list")
}

test_that("general Gaussian constructor matches existing constructor metadata", {
  set.seed(8101)
  y <- rnorm(8)

  dp <- DirichletProcess(y, family = "gaussian")
  ref <- DirichletProcessGaussian(y)

  expect_identical(class(dp), class(ref))
  expect_equal(dp$kernel, ref$kernel)
  expect_equal(dp$conjugacy, ref$conjugacy)
  expect_identical(class(dp$mixingDistribution), class(ref$mixingDistribution))
  expect_false(inherits(dp, "normal"))
  expect_false(inherits(dp, "conjugate"))
})

test_that("family aliases and normalisation work without partial matching", {
  set.seed(8102)
  y <- rnorm(6)

  expect_general_dp(DirichletProcess(y, family = "normal"),
                    "normal", "conjugate")
  expect_general_dp(DirichletProcess(y, family = "  GAUSSIAN  "),
                    "normal", "conjugate")
  expect_general_dp(DirichletProcess(y, family = "normal inverse gamma"),
                    "normal", "conjugate")
  expect_general_dp(DirichletProcess(y, family = "normal-inverse-gamma"),
                    "normal", "conjugate")

  x <- matrix(rnorm(12), ncol = 2)
  expect_general_dp(DirichletProcess(x, family = "mvn"),
                    "mvnormal", "conjugate")
  expect_general_dp(DirichletProcess(x, family = "semi conjugate mvnormal"),
                    "mvnormal2", "nonconjugate")
  expect_general_dp(DirichletProcess(x, family = "semi-conjugate-mvn"),
                    "mvnormal2", "nonconjugate")

  fixed <- DirichletProcess(y, family = "normalFixedVariance", sigma = 1)
  expect_general_dp(fixed, "normalFixedVariance", "conjugate")

  expect_error(DirichletProcess(y, family = "gauss"),
               "Unsupported Dirichlet process family")
})

test_that("all ordinary families dispatch to existing constructors", {
  set.seed(8103)

  expect_general_dp(DirichletProcess(rnorm(6), "gaussian"),
                    "normal", "conjugate")

  beta_dp <- suppressMessages(
    DirichletProcess(c(.2, .3, .6, .8), "beta",
                     maxY = 2, verbose = FALSE, cpp = FALSE)
  )
  expect_general_dp(beta_dp, "beta", "nonconjugate")
  expect_equal(beta_dp$mixingDistribution$maxT, 2)

  beta2_dp <- suppressMessages(
    DirichletProcess(c(.2, .4, .6, .8), "beta2",
                     maxY = 1, verbose = FALSE, mhDraws = 5, cpp = FALSE)
  )
  expect_general_dp(beta2_dp, "beta2", "nonconjugate")

  weibull_dp <- suppressMessages(
    DirichletProcess(c(.4, .7, 1.1, 1.6), "weibull",
                     g0Priors = c(10, 2, .01),
                     verbose = FALSE, mhDraws = 5, cpp = FALSE)
  )
  expect_general_dp(weibull_dp, "weibull", "nonconjugate")

  x <- matrix(rnorm(10), ncol = 2)
  expect_general_dp(DirichletProcess(x, "mvnormal", cpp = FALSE),
                    "mvnormal", "conjugate")
  expect_general_dp(DirichletProcess(x, "mvnormal2", cpp = FALSE),
                    "mvnormal2", "nonconjugate")

  expect_general_dp(DirichletProcess(rexp(6), "exponential", cpp = FALSE),
                    "exponential", "conjugate")

  fixed <- DirichletProcess(rnorm(6), "gaussian_fixed_variance",
                            sigma = 1.5, cpp = FALSE)
  expect_general_dp(fixed, "normalFixedVariance", "conjugate")
  expect_equal(fixed$mixingDistribution$sigma, 1.5)
})

test_that("unsupported non-ordinary families remain separate", {
  y <- rnorm(4)

  expect_error(DirichletProcess(y, "hierarchical_beta"),
               "For hierarchical models use DirichletProcessHierarchicalBeta")
  expect_error(DirichletProcess(y, "hierarchical_mvnormal2"),
               "For hierarchical models use DirichletProcessHierarchicalBeta")
  expect_error(DirichletProcess(y, "hmm"),
               "for HMM models use DirichletHMMCreate")
  expect_error(DirichletProcess(y, "markov"),
               "for HMM models use DirichletHMMCreate")
  expect_error(DirichletProcess(y, "custom"),
               "for custom kernels use MixingDistribution")
})

test_that("obvious required family-specific arguments get friendly errors", {
  y <- c(.2, .4, .6, .8)

  expect_error(DirichletProcess(y, "beta2"),
               "family 'beta2' requires `maxY`")
  expect_error(DirichletProcess(c(.4, .7, 1.1), "weibull"),
               "family 'weibull' requires `g0Priors`")
  expect_error(DirichletProcess(rnorm(4), "gaussian_fixed_variance"),
               "family 'gaussian_fixed_variance' requires `sigma`")
})

test_that("existing constructors still work and class architecture is unchanged", {
  set.seed(8104)

  gaussian <- DirichletProcessGaussian(rnorm(5), cpp = FALSE)
  beta <- suppressMessages(
    DirichletProcessBeta(c(.2, .3, .6, .8),
                         maxY = 1, verbose = FALSE, cpp = FALSE)
  )
  fixed <- DirichletProcessGaussianFixedVariance(rnorm(5), sigma = 1,
                                                 cpp = FALSE)

  expect_general_dp(gaussian, "normal", "conjugate")
  expect_general_dp(beta, "beta", "nonconjugate")
  expect_general_dp(fixed, "normalFixedVariance", "conjugate")
  expect_false(any(c("normal", "beta", "conjugate", "nonconjugate") %in%
                     class(gaussian)))
})

test_that("metadata helpers and lower-level routing helpers remain internal", {
  exports <- getNamespaceExports("dirichletprocess")

  expect_false("dp_type" %in% exports)
  expect_false("dp_kernel" %in% exports)
  expect_false("dp_conjugacy" %in% exports)
  expect_false("mixing_distribution_primary_class" %in% exports)
  expect_false("mixing_distribution_supported_cpp_class" %in% exports)
})
