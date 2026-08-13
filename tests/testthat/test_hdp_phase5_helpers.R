context("HDP Phase 5 Helper And Routing Cleanup")

test_that("Hierarchical beta top-level posterior helpers fail informatively", {
  plot_dp <- getFromNamespace("plot_dirichletprocess", "dirichletprocess")

  set.seed(61)
  dp <- DirichletProcessHierarchicalBeta(list(rbeta(8, 2, 5), rbeta(8, 3, 4)), 1)
  dp <- Fit(dp, 1, progressBar = FALSE)

  expect_error(
    PosteriorFunction(dp),
    "not defined for top-level hierarchical HDP objects"
  )
  expect_error(
    PosteriorClusters(dp),
    "not defined for top-level hierarchical HDP objects"
  )
  expect_error(
    PosteriorFrame(dp, seq(0.1, 0.9, length.out = 5)),
    "not defined for top-level hierarchical HDP objects"
  )
  expect_error(
    PosteriorSummary(dp, seq(0.1, 0.9, length.out = 5)),
    "not defined for top-level hierarchical HDP objects"
  )
  expect_error(
    plot(dp),
    "not defined for top-level hierarchical HDP objects"
  )
  expect_error(
    plot_dp(dp),
    "not defined for top-level hierarchical HDP objects"
  )
})

test_that("Hierarchical mvnormal2 top-level posterior helpers fail informatively", {
  set.seed(62)
  dp <- DirichletProcessHierarchicalMvnormal2(list(
    mvtnorm::rmvnorm(8, c(0, 0), diag(2)),
    mvtnorm::rmvnorm(8, c(1, -1), diag(2))
  ))
  dp <- Fit(dp, 1, progressBar = FALSE)

  expect_error(
    PosteriorFunction(dp),
    "not defined for top-level hierarchical HDP objects"
  )
  expect_error(
    PosteriorClusters(dp),
    "not defined for top-level hierarchical HDP objects"
  )
  expect_error(
    PosteriorSummary(dp, matrix(c(0, 0), ncol = 2)),
    "not defined for top-level hierarchical HDP objects"
  )
})

test_that("Non-hierarchical posterior helpers still work", {
  set.seed(63)
  dp <- DirichletProcessGaussian(rnorm(20))
  dp <- Fit(dp, 2, progressBar = FALSE)

  post_fun <- PosteriorFunction(dp)
  post_vals <- post_fun(seq(-1, 1, length.out = 5))
  post_frame <- PosteriorFrame(dp, seq(-1, 1, length.out = 5), ndraws = 5)

  expect_type(post_vals, "double")
  expect_length(post_vals, 5)
  expect_s3_class(post_frame, "data.frame")
  expect_equal(nrow(post_frame), 5)
})

test_that("Hierarchical live routing stays on rewritten R path even when C++ is forced", {
  old_cpp_setting <- using_cpp()
  old_hierarchical_setting <- getOption("dirichletprocesscpp.force_cpp_hierarchical", NULL)
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)
  on.exit(options(dirichletprocesscpp.force_cpp_hierarchical = old_hierarchical_setting), add = TRUE)

  set_use_cpp(TRUE)
  enable_cpp_hierarchical_samplers(TRUE)

  set.seed(64)
  beta_dp <- DirichletProcessHierarchicalBeta(list(rbeta(8, 2, 5), rbeta(8, 3, 4)), 1, cpp = TRUE)
  mvnormal2_dp <- DirichletProcessHierarchicalMvnormal2(list(
    mvtnorm::rmvnorm(8, c(0, 0), diag(2)),
    mvtnorm::rmvnorm(8, c(1, -1), diag(2))
  ), cpp = TRUE)

  expect_false(can_use_hierarchical_cpp(beta_dp))
  expect_false(can_use_hierarchical_cpp(mvnormal2_dp))

  expect_no_warning({
    beta_dp <- Fit(beta_dp, 1, progressBar = FALSE)
    mvnormal2_dp <- Fit(mvnormal2_dp, 1, progressBar = FALSE)
  })

  expect_true(inherits(beta_dp, "hierarchical"))
  expect_true(inherits(mvnormal2_dp, "hierarchical"))
  expect_true(length(beta_dp$tableDishLabels) == 2L)
  expect_true(length(mvnormal2_dp$tableDishLabels) == 2L)
})
