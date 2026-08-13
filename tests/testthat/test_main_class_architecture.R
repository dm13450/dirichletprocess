context("Main S3 Class Architecture")

dp_type_ <- getFromNamespace("dp_type", "dirichletprocess")
dp_kernel_ <- getFromNamespace("dp_kernel", "dirichletprocess")
dp_conjugacy_ <- getFromNamespace("dp_conjugacy", "dirichletprocess")
md_kernel_ <- getFromNamespace("mixing_distribution_kernel", "dirichletprocess")
md_conjugacy_ <- getFromNamespace("mixing_distribution_conjugacy", "dirichletprocess")
md_cpp_class_ <- getFromNamespace("mixing_distribution_supported_cpp_class", "dirichletprocess")

expect_clean_dp_class <- function(dp, kernel, conjugacy, md_primary = kernel) {
  expect_identical(class(dp), c("dirichletprocess", "list"))
  expect_false(any(c("normal", "beta", "beta2", "weibull", "exponential",
                     "mvnormal", "mvnormal2", "normalFixedVariance",
                     "conjugate", "nonconjugate") %in% class(dp)))
  expect_equal(dp_type_(dp), "dirichletprocess")
  expect_equal(dp_kernel_(dp), kernel)
  expect_equal(dp_conjugacy_(dp), conjugacy)

  expect_identical(class(dp$mixingDistribution)[1], md_primary)
  expect_identical(tail(class(dp$mixingDistribution), 1), "list")
  expect_true(inherits(dp$mixingDistribution, "mixing_distribution"))
  expect_false(any(c("conjugate", "nonconjugate") %in%
                     class(dp$mixingDistribution)))
  expect_equal(md_kernel_(dp$mixingDistribution), kernel)
  expect_equal(md_conjugacy_(dp$mixingDistribution), conjugacy)
}

test_that("ordinary public constructors return clean DP classes", {
  set.seed(4401)

  expect_clean_dp_class(DirichletProcessGaussian(rnorm(5), cpp = FALSE),
                        "normal", "conjugate")
  expect_clean_dp_class(DirichletProcessBeta(c(.2, .3, .6, .8),
                                             maxY = 1,
                                             verbose = FALSE,
                                             cpp = FALSE),
                        "beta", "nonconjugate")
  expect_clean_dp_class(DirichletProcessBeta2(c(.2, .4, .6, .8),
                                              maxY = 1,
                                              verbose = FALSE,
                                              cpp = FALSE),
                        "beta2", "nonconjugate")
  expect_clean_dp_class(DirichletProcessWeibull(c(.4, .7, 1.1, 1.6),
                                                c(10, 2, .01),
                                                verbose = FALSE,
                                                cpp = FALSE),
                        "weibull", "nonconjugate")
  expect_clean_dp_class(DirichletProcessMvnormal(matrix(rnorm(10), ncol = 2),
                                                 cpp = FALSE),
                        "mvnormal", "conjugate")
  expect_clean_dp_class(DirichletProcessMvnormal2(matrix(rnorm(10), ncol = 2),
                                                  cpp = FALSE),
                        "mvnormal2", "nonconjugate")
  expect_clean_dp_class(DirichletProcessExponential(rexp(5), cpp = FALSE),
                        "exponential", "conjugate")
  expect_clean_dp_class(DirichletProcessGaussianFixedVariance(rnorm(5), 1,
                                                              cpp = FALSE),
                        "normalFixedVariance", "conjugate")
})

test_that("hierarchical and Markov containers keep only object-type classes", {
  set.seed(4402)

  hbeta <- DirichletProcessHierarchicalBeta(
    list(c(.2, .4, .6), c(.3, .5, .7)),
    maxY = 1,
    mhDraws = 5,
    cpp = FALSE
  )

  expect_identical(class(hbeta), c("hierarchical", "dirichletprocess", "list"))
  expect_equal(dp_type_(hbeta), "hierarchical")
  expect_equal(dp_kernel_(hbeta), "beta")
  expect_equal(dp_conjugacy_(hbeta), "nonconjugate")
  expect_identical(class(hbeta$indDP[[1]]), c("dirichletprocess", "list"))
  expect_false(inherits(hbeta$indDP[[1]], "hierarchical"))

  hmv <- DirichletProcessHierarchicalMvnormal2(
    list(matrix(rnorm(8), ncol = 2), matrix(rnorm(8), ncol = 2)),
    mhDraws = 5,
    cpp = FALSE
  )

  expect_identical(class(hmv), c("hierarchical", "dirichletprocess", "list"))
  expect_equal(dp_kernel_(hmv), "mvnormal2")
  expect_identical(class(hmv$indDP[[1]]), c("dirichletprocess", "list"))
  expect_false(inherits(hmv$indDP[[1]], "hierarchical"))

  hmm <- DirichletHMMCreate(rnorm(4), GaussianMixtureCreate(),
                            alpha = 2, beta = 3, cpp = FALSE)
  expect_identical(class(hmm), c("markov", "dirichletprocess", "list"))
  expect_equal(dp_type_(hmm), "markov")
  expect_equal(dp_kernel_(hmm), "normal")
  expect_equal(dp_conjugacy_(hmm), "conjugate")
})

test_that("short fits and posterior helpers dispatch under clean DP classes", {
  set.seed(4403)

  gaussian_dp <- DirichletProcessGaussian(rnorm(8), cpp = FALSE)
  old_cpp_setting <- using_cpp()
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)
  set_use_cpp(FALSE)

  gaussian_fit <- Fit(gaussian_dp, 2, progressBar = FALSE)
  expect_identical(class(gaussian_fit), c("dirichletprocess", "list"))
  expect_length(gaussian_fit$alphaChain, 2)
  expect_s3_class(PosteriorClusters(gaussian_fit),
                  "dirichletprocess_posterior_clusters")

  beta_dp <- DirichletProcessBeta(c(.2, .4, .6, .8),
                                  maxY = 1,
                                  verbose = FALSE,
                                  mhDraws = 5,
                                  cpp = FALSE)
  beta_fit <- Fit(beta_dp, 2, progressBar = FALSE)
  expect_identical(class(beta_fit), c("dirichletprocess", "list"))
  expect_s3_class(PosteriorSummary(beta_fit, seq(.2, .8, length.out = 3)),
                  "dirichletprocess_posterior_summary")
})

test_that("custom subclasses stay out of conservative C++ auto-routing", {
  md <- ExponentialMixtureCreate(c(.5, .5))
  class(md) <- c("expcustom", "exponential", "conjugate")
  dp <- DirichletProcessCreate(rexp(4), md, c(2, 4))

  expect_identical(class(dp), c("dirichletprocess", "list"))
  expect_equal(dp_kernel_(dp), "exponential")
  expect_equal(dp_conjugacy_(dp), "conjugate")
  expect_null(md_cpp_class_(dp$mixingDistribution))
  expect_false(can_use_cpp(dp))
})
