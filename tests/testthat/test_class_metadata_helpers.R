context("Class Metadata Helpers")

metadata_helper <- function(name) {
  getFromNamespace(name, "dirichletprocess")
}

dp_type_ <- metadata_helper("dp_type")
dp_kernel_ <- metadata_helper("dp_kernel")
dp_conjugacy_ <- metadata_helper("dp_conjugacy")
is_conjugate_dp_ <- metadata_helper("is_conjugate_dp")
md_kernel_ <- metadata_helper("mixing_distribution_kernel")
md_conjugacy_ <- metadata_helper("mixing_distribution_conjugacy")
md_display_class_ <- metadata_helper("mixing_distribution_display_class")
md_cpp_class_ <- metadata_helper("mixing_distribution_supported_cpp_class")

expect_dp_metadata <- function(dp, type, kernel, conjugacy) {
  expect_equal(dp_type_(dp), type)
  expect_equal(dp_kernel_(dp), kernel)
  expect_equal(dp_conjugacy_(dp), conjugacy)
  expect_equal(is_conjugate_dp_(dp), identical(conjugacy, "conjugate"))
  expect_equal(md_kernel_(dp$mixingDistribution), kernel)
  expect_equal(md_conjugacy_(dp$mixingDistribution), conjugacy)
}

test_that("metadata helpers identify ordinary DP constructors", {
  set.seed(1201)

  gaussian_dp <- DirichletProcessGaussian(rnorm(5), cpp = FALSE)
  beta_dp <- DirichletProcessBeta(c(0.18, 0.28, 0.52, 0.72, 0.88),
                                  maxY = 1,
                                  verbose = FALSE,
                                  cpp = FALSE)
  beta2_dp <- DirichletProcessBeta2(c(0.2, 0.4, 0.6, 0.8),
                                    maxY = 1,
                                    verbose = FALSE,
                                    cpp = FALSE)
  weibull_dp <- DirichletProcessWeibull(c(0.4, 0.7, 1.1, 1.6),
                                        g0Priors = c(10, 2, 0.01),
                                        verbose = FALSE,
                                        cpp = FALSE)
  mvnormal_dp <- DirichletProcessMvnormal(matrix(rnorm(10), ncol = 2),
                                          cpp = FALSE)
  mvnormal2_dp <- DirichletProcessMvnormal2(matrix(rnorm(10), ncol = 2),
                                            cpp = FALSE)
  exponential_dp <- DirichletProcessExponential(rexp(5), cpp = FALSE)

  expect_dp_metadata(gaussian_dp, "dirichletprocess", "normal", "conjugate")
  expect_dp_metadata(beta_dp, "dirichletprocess", "beta", "nonconjugate")
  expect_dp_metadata(beta2_dp, "dirichletprocess", "beta2", "nonconjugate")
  expect_dp_metadata(weibull_dp, "dirichletprocess", "weibull", "nonconjugate")
  expect_dp_metadata(mvnormal_dp, "dirichletprocess", "mvnormal", "conjugate")
  expect_dp_metadata(mvnormal2_dp, "dirichletprocess", "mvnormal2", "nonconjugate")
  expect_dp_metadata(exponential_dp, "dirichletprocess", "exponential", "conjugate")
})

test_that("metadata helpers distinguish top-level hierarchical and local DP objects", {
  set.seed(1202)

  hdp <- DirichletProcessHierarchicalBeta(
    list(c(0.15, 0.25, 0.35), c(0.55, 0.65, 0.75)),
    maxY = 1,
    mhDraws = 5,
    cpp = FALSE
  )
  local_dp <- hdp$indDP[[1]]

  expect_equal(dp_type_(hdp), "hierarchical")
  expect_equal(dp_kernel_(hdp), "beta")
  expect_equal(dp_conjugacy_(hdp), "nonconjugate")
  expect_false(is_conjugate_dp_(hdp))

  expect_equal(dp_type_(local_dp), "dirichletprocess")
  expect_equal(dp_kernel_(local_dp), "beta")
  expect_equal(dp_conjugacy_(local_dp), "nonconjugate")
  expect_equal(md_kernel_(local_dp$mixingDistribution), "beta")
  expect_equal(md_conjugacy_(local_dp$mixingDistribution), "nonconjugate")
})

test_that("metadata helpers identify Markov/HMM objects without class positions", {
  set.seed(1203)

  hmm_dp <- DirichletHMMCreate(rnorm(4), GaussianMixtureCreate(), alpha = 2, beta = 3)

  expect_equal(dp_type_(hmm_dp), "markov")
  expect_equal(dp_kernel_(hmm_dp), "normal")
  expect_equal(dp_conjugacy_(hmm_dp), "conjugate")
  expect_equal(md_kernel_(hmm_dp$mixingDistribution), "normal")
  expect_equal(md_conjugacy_(hmm_dp$mixingDistribution), "conjugate")
})

test_that("metadata helpers survive future-style class vectors without list first", {
  md <- list(distribution = "normal", conjugacy = "conjugate")
  class(md) <- c("normal", "mixing_distribution", "list")
  dp <- list(mixingDistribution = md)
  class(dp) <- c("dirichletprocess", "normal", "conjugate", "list")

  expect_equal(dp_type_(dp), "dirichletprocess")
  expect_equal(dp_kernel_(dp), "normal")
  expect_equal(dp_conjugacy_(dp), "conjugate")
  expect_true(is_conjugate_dp_(dp))
  expect_equal(md_kernel_(md), "normal")
  expect_equal(md_conjugacy_(md), "conjugate")

  class(md) <- c("mixing_distribution", "normal")
  class(dp) <- c("normal", "dirichletprocess")

  expect_equal(dp_type_(dp), "dirichletprocess")
  expect_equal(dp_kernel_(dp), "normal")
  expect_equal(dp_conjugacy_(dp), "conjugate")
})

make_metadata_weibullcens_md <- function() {
  md <- MixingDistribution("weibullcens",
                           c(1, 2, 0.5),
                           "nonconjugate",
                           mhStepSize = c(0.11, 0.11),
                           hyperPriorParameters = c(2.222, 2, 1, 0.05))
  class(md) <- c("weibullcens", "weibull", "nonconjugate")
  md
}

make_metadata_expcustom_md <- function() {
  md <- ExponentialMixtureCreate(c(0.5, 0.5))
  class(md) <- c("expcustom", "exponential", "conjugate")
  md
}

test_that("custom subclasses keep metadata distinct from safe C++ routing", {
  weibullcens_md <- make_metadata_weibullcens_md()
  expcustom_md <- make_metadata_expcustom_md()

  expect_equal(md_kernel_(weibullcens_md), "weibullcens")
  expect_equal(md_conjugacy_(weibullcens_md), "nonconjugate")
  expect_equal(md_display_class_(weibullcens_md), "weibullcens")
  expect_null(md_cpp_class_(weibullcens_md))

  expect_equal(md_kernel_(expcustom_md), "exponential")
  expect_equal(md_conjugacy_(expcustom_md), "conjugate")
  expect_equal(md_display_class_(expcustom_md), "expcustom")
  expect_null(md_cpp_class_(expcustom_md))

  custom_dp <- DirichletProcessCreate(rexp(4), expcustom_md, c(2, 4))

  expect_equal(dp_type_(custom_dp), "dirichletprocess")
  expect_equal(dp_kernel_(custom_dp), "exponential")
  expect_equal(dp_conjugacy_(custom_dp), "conjugate")
  expect_false(can_use_cpp(custom_dp))
})
