context("Plot default summary path")

make_expcustom_md_for_plot_validation <- function() {
  md <- ExponentialMixtureCreate(c(0.5, 0.5))
  class(md) <- c("expcustom", "exponential", "conjugate")
  md
}

test_that("ordinary univariate plot uses PosteriorSummary when stored samples exist", {
  old_cpp_setting <- set_use_cpp(FALSE)
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)

  set.seed(401)
  dp <- DirichletProcessGaussian(rnorm(20), cpp = FALSE)
  dp <- Fit(dp, 6, progressBar = FALSE)

  xlim <- c(-2, 2)
  x_grid <- seq(xlim[1], xlim[2], length.out = 11)

  set.seed(402)
  expected <- PosteriorSummary(dp, x_grid, level = 0.95)
  set.seed(402)
  graph <- plot(dp, data_method = "none", xgrid_pts = 11, xlim = xlim)
  built <- ggplot2::ggplot_build(graph)

  expect_is(graph, c("gg", "ggplot"))
  expect_length(built$data, 3)
  expect_equal(built$data[[1]]$x, expected$x)
  expect_equal(built$data[[1]]$y, expected$Lower)
  expect_equal(built$data[[2]]$y, expected$Mean)
  expect_equal(built$data[[3]]$y, expected$Upper)
})

test_that("ordinary univariate plot does not call PosteriorFrame by default", {
  old_cpp_setting <- set_use_cpp(FALSE)
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)

  set.seed(406)
  dp <- DirichletProcessGaussian(rnorm(20), cpp = FALSE)
  dp <- Fit(dp, 6, progressBar = FALSE)

  testthat::local_mocked_bindings(
    PosteriorFrame = function(...) {
      stop("PosteriorFrame should not be called by default ordinary plot().")
    },
    .package = "dirichletprocess"
  )

  expect_no_error(
    graph <- plot(dp, data_method = "none", xgrid_pts = 11, xlim = c(-2, 2))
  )
  expect_is(graph, c("gg", "ggplot"))
})

test_that("ordinary univariate plot omits intervals and messages when no stored samples exist", {
  old_cpp_setting <- set_use_cpp(FALSE)
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)

  set.seed(403)
  dp <- DirichletProcessGaussian(rnorm(20), cpp = FALSE)
  dp <- Fit(dp, 4, progressBar = FALSE, storeSamples = FALSE)

  xlim <- c(-2, 2)
  x_grid <- seq(xlim[1], xlim[2], length.out = 9)
  expected_curve <- LikelihoodFunction(dp)(x_grid)

  expect_message(
    graph <- plot(dp, data_method = "none", xgrid_pts = 9, xlim = xlim),
    "Credible intervals are unavailable because this object has no usable stored MCMC samples"
  )
  built <- ggplot2::ggplot_build(graph)

  expect_is(graph, c("gg", "ggplot"))
  expect_length(built$data, 1)
  expect_equal(built$data[[1]]$x, x_grid)
  expect_equal(built$data[[1]]$y, expected_curve)
})

test_that("ordinary custom subclasses can plot through the default PosteriorSummary path", {
  old_cpp_setting <- set_use_cpp(TRUE)
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)

  set.seed(407)
  dp <- DirichletProcessCreate(rexp(20),
                               make_expcustom_md_for_plot_validation(),
                               c(2, 4))
  dp <- Initialise(dp)
  dp <- Fit(dp, 4, progressBar = FALSE)

  graph <- plot(dp, data_method = "none", xgrid_pts = 9, xlim = c(0, 3))
  built <- ggplot2::ggplot_build(graph)

  expect_is(graph, c("gg", "ggplot"))
  expect_length(built$data, 3)
})

test_that("PosteriorFrame direct use remains available and unchanged", {
  old_cpp_setting <- set_use_cpp(FALSE)
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)

  set.seed(404)
  dp <- DirichletProcessGaussian(rnorm(20), cpp = FALSE)
  dp <- Fit(dp, 4, progressBar = FALSE)

  frame <- PosteriorFrame(dp, seq(-1, 1, length.out = 5), ndraws = 5)

  expect_s3_class(frame, "data.frame")
  expect_equal(ncol(frame), 4)
  expect_equal(nrow(frame), 5)
})

test_that("hierarchical top-level plotting behavior is unaffected", {
  set.seed(405)
  dp <- DirichletProcessHierarchicalBeta(list(rbeta(8, 2, 5), rbeta(8, 3, 4)), 1)
  dp <- Fit(dp, 1, progressBar = FALSE)

  expect_error(
    plot(dp, data_method = "none"),
    "not defined for top-level hierarchical HDP objects"
  )
})
