context("Posterior summary plot")

with_pdf_device_for_summary_plot <- function(expr) {
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit({
    grDevices::dev.off()
    unlink(path)
  }, add = TRUE)

  force(expr)
}

make_summary_plot_test_dp <- function() {
  old_cpp_setting <- set_use_cpp(FALSE)
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)

  set.seed(901)
  dp <- DirichletProcessGaussian(rnorm(12), cpp = FALSE)
  Fit(dp, 4, FALSE, FALSE)
}

test_that("posterior summary plot draws mean and credible interval invisibly", {
  dp <- make_summary_plot_test_dp()
  grid <- seq(-2, 2, length.out = 6)
  summary_df <- PosteriorSummary(dp, grid, level = 0.9)

  expect_s3_class(summary_df, "dirichletprocess_posterior_summary")
  expect_s3_class(summary_df, "data.frame")
  expect_equal(names(summary_df), c("x", "Mean", "Median", "Lower", "Upper"))

  result <- with_pdf_device_for_summary_plot(withVisible(plot(summary_df)))

  expect_false(result$visible)
  expect_equal(result$value, summary_df)
})

test_that("posterior summary plot has no default title", {
  plot_method <- getS3method("plot", "dirichletprocess_posterior_summary")

  expect_null(formals(plot_method)$main)
})

test_that("posterior summary plot works without credible interval columns", {
  dp <- make_summary_plot_test_dp()
  grid <- seq(-2, 2, length.out = 6)
  summary_df <- PosteriorSummary(dp, grid, level = 0.9)
  summary_no_interval <- summary_df
  summary_no_interval$Lower <- NULL
  summary_no_interval$Upper <- NULL

  expect_s3_class(summary_no_interval, "dirichletprocess_posterior_summary")
  expect_equal(names(summary_no_interval), c("x", "Mean", "Median"))

  result <- with_pdf_device_for_summary_plot(withVisible(plot(summary_no_interval)))

  expect_false(result$visible)
  expect_equal(result$value, summary_no_interval)
})

test_that("manuscript plotting helpers remain outside the exported API", {
  exports <- getNamespaceExports("dirichletprocess")

  expect_false("make_summary_plot" %in% exports)
  expect_false("make_cluster_plot" %in% exports)
  expect_false("PosteriorCurveSummary" %in% exports)
})
